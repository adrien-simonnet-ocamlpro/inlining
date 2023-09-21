type var = int
type pointer = int
type tag = int
type frame = pointer * var list
type stack = frame list

module VarMap = Map.Make (Int)
module PointerMap = Map.Make (Int)
module PointerSet = Set.Make (Int)

type expr =
| Var of var
| Int of int
| Add of var * var
| Sub of var * var
| Tuple of var list
| Get of var * int
| Pointer of int

type instr =
| Let of var * expr * instr
| Apply_direct of pointer * var list * stack
| Apply_indirect of var * var list * stack
| If of var * (int * pointer * var list) list * (pointer * var list) * stack
| Return of var

type block = var list * instr

type blocks = block PointerMap.t

type benchmark = {
  mutable const: int;
  mutable write: int;
  mutable read: int;
  mutable add: int;
  mutable sub: int;
  mutable push: int;
  mutable pop: int;
  mutable jmp: int
}

type value =
| Int of int
| Pointer of pointer
| Tuple of value list

type 'a map = (var * 'a) list

type env = value VarMap.t

let string_of_sub (var: var) (subs: string VarMap.t): string =
  match VarMap.find_opt var subs with
  | Some str -> str ^ "_" ^ (string_of_int var)
  | None -> "_" ^ (string_of_int var)

let rec pp_args ?(subs = (VarMap.empty: string VarMap.t)) ?(empty=(" ": string)) ?(split=(" ": string)) (fmt: Format.formatter) (args: var list): unit =
  match args with
  | [] -> Format.fprintf fmt "%s" empty
  | [ arg ] -> Format.fprintf fmt "%s" (string_of_sub arg subs)
  | arg :: args' -> Format.fprintf fmt "%s%s%a" (string_of_sub arg subs) split (pp_args ~split ~empty ~subs) args'

let pp_frame (fmt: Format.formatter) ((k, args): frame): unit =
  Format.fprintf fmt "k%d %a" k (pp_args ~subs: VarMap.empty ~split: " " ~empty: "()") args

let rec pp_stack (fmt: Format.formatter) (stack: stack): unit =
  match stack with
  | [] -> Format.fprintf fmt ""
  | [ frame ] -> Format.fprintf fmt "%a" pp_frame frame
  | frame :: stack' -> Format.fprintf fmt "%a; %a" pp_frame frame pp_stack stack'

let pp_expr (subs: string VarMap.t) (fmt: Format.formatter) (expr: expr): unit =
  match expr with
  | Pointer p -> Format.fprintf fmt "Pointer %d" p
  | Int x -> Format.fprintf fmt "Int %d" x
  | Add (x1, x2) -> Format.fprintf fmt "add %s %s" (string_of_sub x1 subs) (string_of_sub x2 subs)
  | Sub (x1, x2) -> Format.fprintf fmt "sub %s %s" (string_of_sub x1 subs) (string_of_sub x2 subs)
  | Var x -> Format.fprintf fmt "%s" (string_of_sub x subs)
  | Tuple [] -> Format.fprintf fmt ""
  | Tuple [ value ] -> Format.fprintf fmt "%s" (string_of_sub value subs)
  | Tuple (value :: values') -> begin
      Format.fprintf fmt "Tuple [%s" (string_of_sub value subs);
      List.iter (fun v' -> Format.fprintf fmt "; %s" (string_of_sub v' subs)) values';
      Format.fprintf fmt "]"
    end
  | Get (record, pos) -> Format.fprintf fmt "get %s %d" (string_of_sub record subs) pos

let rec pp_instr (subs: string VarMap.t) (fmt: Format.formatter) (cps : instr): unit =
  match cps with
  | Let (var, expr, instr) -> Format.fprintf fmt "\tlet %s = %a in\n%a" (string_of_sub var subs) (pp_expr subs) expr (pp_instr subs) instr
  | If (var, matchs, (kf, argsf), stack) -> Format.fprintf fmt "\tmatch %s with%s | _ -> k%d %a [%a]" (string_of_sub var subs) (List.fold_left (fun acc (n, kt, argst) -> acc ^ (Format.asprintf "| Int %d -> k%d %a [%a] " n kt (pp_args ~subs ~empty: "()" ~split: " ") argst pp_stack stack)) " " matchs) kf (pp_args ~subs ~empty: "()" ~split: " ") argsf pp_stack stack
  | Return x -> Format.fprintf fmt "\t%s" (string_of_sub x subs)
  | Apply_indirect (x, args, stack) -> Format.fprintf fmt "\t!%s %a [%a]" (string_of_sub x subs) (pp_args ~split:" " ~subs ~empty: "()") args pp_stack stack
  | Apply_direct (k', args, stack) -> Format.fprintf fmt "\tk%d %a [%a]" k' (pp_args ~split:" " ~subs ~empty: "()") args pp_stack stack

let pp_block (subs: string VarMap.t) (fmt: Format.formatter) ((args, e): block): unit =
  Format.fprintf fmt "%a =\n%a%!" (pp_args ~subs ~empty: "()" ~split: " ") args (pp_instr subs) e

let pp_blocks (subs: string VarMap.t) (fmt: Format.formatter) (blocks: blocks) : unit =
  match PointerMap.bindings blocks with
  | [] -> Format.fprintf fmt "@."
  | [p, b] -> Format.fprintf fmt "let rec _b%d %a\n\n@." p (pp_block subs) b
  | (p, b) :: bs -> begin
      Format.fprintf fmt "let rec _b%d %a\n\n@." p (pp_block subs) b;
      List.iter (fun (p, b) -> Format.fprintf fmt "and k%d %a\n%!" p (pp_block subs) b) bs
    end

let rec pp_value fmt (value: value) =
  match value with
  | Int i -> Format.fprintf fmt "%d" i
  | Pointer p -> Format.fprintf fmt "k%d" p
  | Tuple [] -> Format.fprintf fmt ""
  | Tuple [ value ] -> Format.fprintf fmt "%a" pp_value value
  | Tuple (value :: values') -> begin
      Format.fprintf fmt "(%a" pp_value value;
      List.iter (fun v' -> Format.fprintf fmt ", %a" pp_value v') values';
      Format.fprintf fmt ")"
    end

let update_var (var: var) (alias: var VarMap.t): var =
  if VarMap.mem var alias then VarMap.find var alias else var
let update_vars (vars: var list) (alias: var VarMap.t): var list =
  List.map (fun var -> update_var var alias) vars

let update_frame_vars (alias: var VarMap.t) (k, args: frame): frame =
  k, update_vars args alias
let update_stack_vars (alias: var VarMap.t) (stack: stack): stack =
  List.map (update_frame_vars alias) stack

let inline_expr (expr : expr) (alias: var VarMap.t): expr =
  match expr with
  | Int x -> Int x
  | Add (x1, x2) -> Add (update_var x1 alias, update_var x2 alias)
  | Sub (x1, x2) -> Sub (update_var x1 alias, update_var x2 alias)
  | Var x -> Var (update_var x alias)
  | Tuple args -> Tuple (update_vars args alias)
  | Get (record, pos) -> Get (update_var record alias, pos)
  | Pointer k -> Pointer k

let rec inline (cps : instr) (alias: var VarMap.t) (stack: (pointer * var list) list): instr =
  match cps with
  | Let (var, expr, instr) -> Let (update_var var alias, inline_expr expr alias, inline instr alias stack)
  | Apply_direct (k, args, stack') -> Apply_direct (k, update_vars args alias, (update_stack_vars alias stack') @ stack)
  | If (var, matchs, (kf, argsf), stack') -> If (update_var var alias, List.map (fun (n, k, argst) -> n, k, update_vars argst alias) matchs, (kf, update_vars argsf alias), (update_stack_vars alias stack') @ stack)
  | Return v -> begin
      match stack with
      | [] -> Return (update_var v alias)
      | (k, env') :: stack' -> Apply_direct (k, update_var v alias :: env', stack')
    end
  | Apply_indirect (x, args, stack') -> Apply_indirect (update_var x alias, update_vars args alias, (update_stack_vars alias stack') @ stack)

let rec inline_parent (cps : instr) (blocks: blocks): instr =
  match cps with
  | Let (var, expr, instr) -> Let (var, expr, inline_parent instr blocks)
  | Apply_direct (k, args, stack') when PointerMap.mem k blocks -> begin
      Logger.start "Inlining %d\n" k;
      let args', block = PointerMap.find k blocks in
      let instr' = inline_parent (inline block (List.fold_left2 (fun alias arg' arg -> VarMap.add arg' arg alias) VarMap.empty args' args) stack') blocks in
      Logger.stop ();
      instr'
    end
  | Apply_direct (k, args, stack') -> Apply_direct (k, args, stack')
  | If (var, matchs, (kf, argsf), stack') -> If (var, matchs, (kf, argsf), stack')
  | Return var -> Return var
  | Apply_indirect (x, args, stack') -> Apply_indirect (x, args, stack')

let inline_blocks (blocks : blocks) (targets: PointerSet.t): blocks =
  let targets = PointerMap.filter (fun k _ -> PointerSet.mem k targets) blocks in
  PointerMap.map (fun (args, block) -> args, inline_parent block targets) blocks

let elim_unused_vars_expr (vars : int array) conts (expr : expr): unit =
  match expr with
  | Int x -> Array.set vars x (Array.get vars x + 1)
  | Add (x1, x2) -> Array.set vars x1 (Array.get vars x1 + 1); Array.set vars x2 (Array.get vars x2 + 1)
  | Sub (x1, x2) -> Array.set vars x1 (Array.get vars x1 + 1); Array.set vars x2 (Array.get vars x2 + 1)
  | Var x -> Array.set vars x (Array.get vars x + 1)
  | Tuple args -> List.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args
  | Get (arg, _) -> Array.set vars arg (Array.get vars arg + 1)
  | Pointer k -> Array.set conts k (Array.get conts k + 1)

let rec elim_unused_vars (vars : int array) (conts : int array) (cps : instr) : instr =
  match cps with
  | Let (var, e1, e2) ->
    let e2' = elim_unused_vars vars conts e2 in
    if Array.get vars var > 0 then begin
      elim_unused_vars_expr vars conts e1;
      Let (var, e1, e2')
    end else e2'
  | Apply_direct (k, args, stack) ->
    List.iter (fun (k, args2) -> Array.set conts k (Array.get conts k + 1);
    List.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args2) stack;
    Array.set conts k (Array.get conts k + 1);
    List.iter
      (fun arg ->
        Array.set vars arg (Array.get vars arg + 1))
      args;
    Apply_direct (k, args, stack)
  | If (var, matchs, (kf, argsf), stack) ->
    List.iter (fun (k, args2) -> Array.set conts k (Array.get conts k + 1);
    List.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args2) stack;
    Array.set vars var (Array.get vars var + 1);
    List.iter (fun (_, kt, args) -> List.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args; Array.set conts kt (Array.get conts kt + 1)) matchs;
    List.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) argsf;
    Array.set conts kf (Array.get conts kf + 1);
    If (var, matchs, (kf, argsf), stack)
  | Return x ->
    Array.set vars x (Array.get vars x + 1);
    Return x
  | Apply_indirect (x, args, stack) ->
    Array.set vars x (Array.get vars x + 1);
    List.iter (fun (k, args2) -> Array.set conts k (Array.get conts k + 1);
    List.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args2) stack;
    List.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args;
      Apply_indirect (x, args, stack)

let elim_unused_vars_block (conts : int array) ((args', e1) : block) : block =
    (args', elim_unused_vars (Array.make 10000 0) conts e1)

let elim_unused_vars_blocks (blocks : blocks) : blocks * int array =
  let conts = Array.make 10000 0 in
  PointerMap.map (elim_unused_vars_block conts) blocks, conts

let elim_unused_blocks (conts : int array) (blocks : blocks) : blocks = PointerMap.filter (fun k _ -> Array.get conts k > 0) blocks




let get env x = VarMap.find x env

let interp_expr (expr : expr) (env : env) (benchmark: benchmark): value =
  match expr with
  | Int x -> benchmark.const <- benchmark.const + 1; Int x
  | Add (x1, x2) -> begin
      benchmark.add <- benchmark.add + 1; 
      match get env x1, get env x2 with
      | Int n1, Int n2 -> Int (n1 + n2)
      | _ -> assert false
    end
  | Sub (x1, x2) -> begin
      benchmark.add <- benchmark.add + 1; 
      match get env x1, get env x2 with
      | Int n1, Int n2 -> Int (n1 - n2)
      | _ -> assert false
    end
  | Var x -> benchmark.read <- benchmark.read + 1; get env x
  | Tuple args -> Tuple (List.map (fun arg -> benchmark.read <- benchmark.read + 1; get env arg) args)
  | Get (record, pos) -> begin
      benchmark.read <- benchmark.read + 1; 
      match get env record with
      | Tuple (values) -> List.nth values pos
      | _ -> assert false
    end
  | Pointer k -> Pointer k

let rec interp (stack: (pointer * value list) list) (cps : instr) (env : env) (conts : blocks) (benchmark: benchmark): value =
  match cps with
  | Let (var, expr, instr) -> begin
      benchmark.write <- benchmark.write + 1;
      interp stack instr (VarMap.add var (interp_expr expr env benchmark) env) conts benchmark
    end
  | Apply_direct (k, args, stack') -> begin
      benchmark.jmp <- benchmark.jmp + 1;
      if PointerMap.mem k conts then begin
        let args', cont = PointerMap.find k conts in
        let stack''' = (List.map (fun (k, env') -> (k, (List.map (fun arg -> begin
          benchmark.push <- benchmark.push + 1;
          get env arg
        end) env'))) stack') @ stack in
        let env''' = List.fold_left2 (fun env'' arg' arg -> begin
          benchmark.read <- benchmark.read + 1;
          VarMap.add arg' (get env arg) env''
        end) VarMap.empty args' args in
        interp stack''' cont env''' conts benchmark
      end else failwith ("k" ^ (string_of_int k) ^ "not found")
    end
  | If (var, matchs, (kf, argsf), stack') -> begin
      benchmark.read <- benchmark.read + 1;
      match get env var with
      | Int n -> begin
          match List.find_opt (fun (n', _, _) -> benchmark.jmp <- benchmark.jmp + 1; n = n') matchs with
          | Some (_, kt, argst) -> interp stack (Apply_direct (kt, argst, stack')) env conts benchmark
          | None -> interp stack (Apply_direct (kf, argsf, stack')) env conts benchmark
        end
      | _ -> assert false
    end
  | Return v -> begin
      benchmark.pop <- benchmark.pop + 1;
      benchmark.read <- benchmark.read + 1;
      match stack with
      | [] -> benchmark.read <- benchmark.read + 1; get env v
      | (k, env') :: stack' -> begin
          let args2', cont'' = PointerMap.find k conts in
          let env''' = VarMap.add (List.hd args2') (get env v) (List.fold_left2 (fun env'' arg' arg -> begin
            benchmark.pop <- benchmark.pop + 1;
            VarMap.add arg' arg env''
          end) VarMap.empty (List.tl args2') env') in
          interp stack' cont'' env''' conts benchmark
        end
    end
  | Apply_indirect (x, args, stack') -> begin
      benchmark.read <- benchmark.read + 1;
      benchmark.jmp <- benchmark.jmp + 1;
      match get env x with
      | Pointer k' -> begin
          let args', cont = PointerMap.find k' conts in
          let stack''' = (List.map (fun (k, env') -> (k, (List.map (fun arg -> begin
            benchmark.push <- benchmark.push + 1;
            get env arg
          end) env'))) stack')@stack in
          let env''' = List.fold_left2 (fun env'' arg' arg -> begin
            benchmark.read <- benchmark.read + 1;
            VarMap.add arg' (get env arg) env''
          end) VarMap.empty args' args in
          interp stack''' cont env''' conts benchmark
        end
      | _ -> failwith ("invalid type")
    end

let pp_benchmark (benchmark: benchmark) fmt: unit =
  Format.fprintf fmt "const: %d; write: %d; read: %d; add: %d; sub: %d; push: %d; pop: %d; jmp: %d\n%!" benchmark.const benchmark.write benchmark.read benchmark.add benchmark.sub benchmark.push benchmark.pop benchmark.jmp

let size_expr (expr : expr): int =
  match expr with
  | Int _ -> 1
  | Add (_, _) -> 2
  | Sub (_, _) -> 2
  | Var _ -> 1
  | Tuple args -> List.length args
  | Get (_, _) -> 2
  | Pointer _ -> 1

let rec size_instr (cps : instr): int =
    match cps with
    | Let (_, expr, instr) -> 1 + size_expr expr + size_instr instr
    | Apply_direct (_, args, stack) -> 1 + List.length args + List.fold_left (fun size (_, args) -> size + 1 + List.length args) 0 stack
    | If (_, matchs, (_, argsf), stack) -> List.fold_left (fun size (_, _, args) -> size + 1 + List.length args) 0 matchs + 1 + List.length argsf + List.fold_left (fun size (_, args) -> size + 1 + List.length args) 0 stack
    | Return _ -> 1
    | Apply_indirect (_, args, stack) -> 2 + List.length args + List.fold_left (fun size (_, args) -> size + 1 + List.length args) 0 stack

let size_block (args, instr: block): int =
  List.length args + size_instr instr

let size_blocks (blocks: blocks): int =
  PointerMap.fold (fun _ block size -> size + size_block block) blocks 0

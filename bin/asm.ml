type var = int
type pointer = int
type tag = int
type frame = pointer * var list
type stack = frame list

module VarMap = Map.Make (Int)
module BlockMap = Map.Make (Int)
module BlockSet = Set.Make (Int)

type prim =
| Add
| Sub
| Const of int
| Print

type named =
| Prim of prim * var list
| Var of var
| Tuple of var list
| Get of var * int
| Pointer of int

and expr =
| Let of var * named * expr
| Apply_direct of pointer * var list * stack
| Apply_indirect of var * var list * stack
| If of var * (int * pointer * var list) list * (pointer * var list) * stack
| Return of var

type block = var list * expr

type blocks = block BlockMap.t

let gen_name (var: var) (subs: string VarMap.t): string =
  match VarMap.find_opt var subs with
  | Some str -> str ^ "_" ^ (string_of_int var)
  | None -> "_" ^ (string_of_int var)

let rec pp_args ?(subs = (VarMap.empty: string VarMap.t)) ?(empty=(" ": string)) ?(split=(" ": string)) (fmt: Format.formatter) (args: var list): unit =
  match args with
  | [] -> Format.fprintf fmt "%s" empty
  | [ arg ] -> Format.fprintf fmt "%s" (gen_name arg subs)
  | arg :: args' -> Format.fprintf fmt "%s%s%a" (gen_name arg subs) split (pp_args ~split ~empty ~subs) args'

let pp_frame (fmt: Format.formatter) ((k, args): frame): unit = Format.fprintf fmt "k%d %a" k (pp_args ~subs: VarMap.empty ~split: " " ~empty: "()") args

let rec pp_stack (fmt: Format.formatter) (stack: stack): unit =
  match stack with
  | [] -> Format.fprintf fmt ""
  | [ frame ] -> Format.fprintf fmt "%a" pp_frame frame
  | frame :: stack' -> Format.fprintf fmt "%a; %a" pp_frame frame pp_stack stack'

let pp_prim (subs: string VarMap.t) (fmt: Format.formatter) (prim : prim) (args: var list): unit =
  match prim, args with
  | Const x, [] -> Format.fprintf fmt "Int %d" x
  | Add, x1 :: x2 :: [] -> Format.fprintf fmt "add %s %s" (gen_name x1 subs) (gen_name x2 subs)
  | Sub, x1 :: x2 :: [] -> Format.fprintf fmt "sub %s %s" (gen_name x1 subs) (gen_name x2 subs)
  | Print, x1 :: [] -> Format.fprintf fmt "print %s" (gen_name x1 subs)
  | _ -> failwith "Asm.pp_prim: invalid args"

let pp_named (subs: string VarMap.t) (fmt: Format.formatter) (named: named): unit =
  match named with
  | Pointer p -> Format.fprintf fmt "k%d" p
  | Prim (prim, args) -> pp_prim subs fmt prim args
  | Var x -> Format.fprintf fmt "%s" (gen_name x subs)
  | Tuple (args) -> Format.fprintf fmt "Tuple [%a]" (pp_args ~split: "; " ~subs ~empty:"") args
  | Get (record, pos) -> Format.fprintf fmt "Get (%s, %d)" (gen_name record subs) pos

let rec pp_expr (subs: string VarMap.t) (fmt: Format.formatter) (cps : expr): unit =
  match cps with
  | Let (var, named, expr) -> Format.fprintf fmt "\tlet %s = %a in\n%a" (gen_name var subs) (pp_named subs) named (pp_expr subs) expr
  | If (var, matchs, (kf, argsf), []) -> Format.fprintf fmt "\tmatch %s with%s | _ -> k%d %a" (gen_name var subs) (List.fold_left (fun acc (n, kt, argst) -> acc ^ (Format.asprintf "| Int %d -> k%d %a " n kt (pp_args ~subs ~empty: "()" ~split: " ") argst)) " " matchs) kf (pp_args ~subs ~empty: "()" ~split: " ") argsf
  | Return x -> Format.fprintf fmt "\t%s" (gen_name x subs)
  | Apply_indirect (x, args, stack) -> Format.fprintf fmt "\t!%s %a [%a]" (gen_name x subs) (pp_args ~split:" " ~subs ~empty: "()") args pp_stack stack
  | Apply_direct (k', args, stack) -> Format.fprintf fmt "\tk%d %a [%a]" k' (pp_args ~split:" " ~subs ~empty: "()") args pp_stack stack
  | _ -> assert false

let pp_block (subs: string VarMap.t) (fmt: Format.formatter) ((args, e): block): unit = Format.fprintf fmt "%a =\n%a" (pp_args ~subs ~empty: "()" ~split: " ") args (pp_expr subs) e

let pp_blocks (subs: string VarMap.t) (fmt: Format.formatter) (block : blocks) : unit = BlockMap.iter (fun k block -> Format.fprintf fmt "k%d %a\n" k (pp_block subs) block) block

let update_var (var: var) (alias: var VarMap.t): var = if VarMap.mem var alias then VarMap.find var alias else var
let update_vars (vars: var list) (alias: var VarMap.t): var list = List.map (fun var -> update_var var alias) vars

let rec inline_named (named : named) (alias: var VarMap.t): named =
  match named with
  | Prim (prim, args) -> Prim (prim, update_vars args alias)
  | Var x -> Var (update_var x alias)
  | Tuple args -> Tuple (update_vars args alias)
  | Get (record, pos) -> Get (update_var record alias, pos)
  | Pointer k -> Pointer k

and inline (cps : expr) (alias: var VarMap.t) (stack: (pointer * var list) list): expr =
  match cps with
  | Let (var, named, expr) -> Let (var, inline_named named alias, inline expr alias stack)
  | Apply_direct (k, args, stack') -> Apply_direct (k, update_vars args alias, stack' @ stack)
  | If (var, matchs, (kf, argsf), stack') -> If (var, List.map (fun (n, k, argst) -> n, k, update_vars argst alias) matchs, (kf, update_vars argsf alias), stack' @ stack)
  | Return v -> begin
      match stack with
      | [] -> Return (update_var v alias)
      | (k, env') :: stack' -> Apply_direct (k, update_var v alias :: env', stack')
    end
  | Apply_indirect (x, args, stack') -> Apply_indirect (update_var x alias, update_vars args alias, stack' @ stack)

let rec inline_parent (cps : expr) (blocks: blocks): expr =
  match cps with
  | Let (var, named, expr) -> Let (var, named, inline_parent expr blocks)
  | Apply_direct (k, args, stack') when BlockMap.mem k blocks -> begin
      let args', block = BlockMap.find k blocks in
      inline_parent (inline block (List.fold_left2 (fun alias arg' arg -> VarMap.add arg' arg alias) VarMap.empty args' args) stack') blocks 
    end
  | Apply_direct (k, args, stack') -> Apply_direct (k, args, stack')
  | If (var, matchs, (kf, argsf), stack') -> If (var, matchs, (kf, argsf), stack')
  | Return var -> Return var
  | Apply_indirect (x, args, stack') -> Apply_indirect (x, args, stack')

let inline_blocks (blocks : blocks) (targets: BlockSet.t): blocks =
  let targets = BlockMap.filter (fun k _ -> BlockSet.mem k targets) blocks in
  BlockMap.map (fun (args, block) -> args, inline_parent block targets) blocks


let rec elim_unused_vars_named (vars : int array) conts (named : named): named =
  match named with
  | Prim (prim, args) ->
    List.iter
      (fun arg ->
        Array.set vars arg (Array.get vars arg + 1))
      args;
    Prim (prim, args)
    | Var x ->
      Array.set vars x (Array.get vars x + 1);
      Var x
  | Tuple args -> List.iter
  (fun arg ->
    Array.set vars arg (Array.get vars arg + 1))
  args; Tuple args
  | Get (arg, pos) -> Array.set vars arg (Array.get vars arg + 1); Get (arg, pos)
  | Pointer k -> Array.set conts k (Array.get conts k + 1); Pointer k

and elim_unused_vars (vars : int array) (conts : int array) (cps : expr) : expr =
  match cps with
  | Let (var, e1, e2) ->
    let e2' = elim_unused_vars vars conts e2 in
    if Array.get vars var > 0
    then (
      let e1' = elim_unused_vars_named vars conts e1 in
      Let (var, e1', e2'))
    else e2'
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
  BlockMap.map (elim_unused_vars_block conts) blocks, conts

let elim_unused_blocks (conts : int array) (blocks : blocks) : blocks = BlockMap.filter (fun k _ -> Array.get conts k > 0) blocks

type benchmark = { mutable const: int; mutable write: int; mutable read: int; mutable add: int; mutable sub: int; mutable push: int; mutable pop: int; mutable jmp: int }

type value =
  | Int of int
  | Tuple of value list

type 'a map = (var * 'a) list

type env = value map

let get = Env.get2

let interp_prim var (prim : prim) args (env : (var * value) list) (benchmark: benchmark) =
  match prim, args with
  | Const x, _ -> benchmark.const <- benchmark.const + 1; [ var, Int x ]
  | Add, x1 :: x2 :: _ -> benchmark.add <- benchmark.add + 1; 
    (match (get env x1 : value) with
     | Int n1 ->
       (match get env x2 with
        | Int n2 -> [ var, Int (n1 + n2) ]
        | _ -> assert false)
     | _ -> assert false)
  | Sub, x1 :: x2 :: _ -> benchmark.sub <- benchmark.sub + 1; 
      (match (get env x1 : value) with
       | Int n1 ->
         (match get env x2 with
          | Int n2 -> [ var, Int (n1 - n2) ]
          | _ -> assert false)
       | _ -> assert false)
  | Print, x1 :: _ ->
    (match (get env x1 : value) with
     | Int n ->
       Printf.printf "%d\n" n;
       []
     | _ -> assert false)
  | _ -> failwith "invalid args"

let interp_named var (named : named) (env : (var * value) list) (benchmark: benchmark) =
  match named with
  | Prim (prim, args) -> interp_prim var prim args env benchmark
  | Var x -> benchmark.read <- benchmark.read + 1; [ var, get env x ]
  | Tuple (args) -> [var, Tuple (List.map (fun arg -> benchmark.read <- benchmark.read + 1; get env arg) args)]
  | Get (record, pos) -> benchmark.read <- benchmark.read + 1; begin
    match get env record with
    | Tuple (values) -> [var, List.nth values pos]
    | _ -> assert false
    end
  | Pointer k -> [ var, Int k ]

let rec interp (stack: (pointer * value list) list) (cps : expr) (env : env) (conts : blocks) (benchmark: benchmark): value =
    match cps with
    | Let (var, named, expr) -> benchmark.write <- benchmark.write + 1; interp stack expr (interp_named var named env benchmark @ env) conts benchmark
    | Apply_direct (k, args, stack') -> begin
        benchmark.jmp <- benchmark.jmp + 1;
        let args', cont = BlockMap.find k conts in
        interp ((List.map (fun (k, env') -> (k, (List.map (fun arg -> benchmark.push <- benchmark.push + 1; get env arg) env'))) stack') @ stack) cont (List.map2 (fun arg' arg -> benchmark.read <- benchmark.read + 1; arg', get env arg) args' args ) conts benchmark
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
        match stack with
        | [] -> benchmark.read <- benchmark.read + 1; get env v
        | (k, env') :: stack' -> begin
            let args2', cont'' = BlockMap.find k conts in
            interp stack' cont'' ((benchmark.read <- benchmark.read + 1; List.hd args2', get env v) :: (List.map2 (fun arg' arg -> benchmark.pop <- benchmark.pop + 1; arg', arg) (List.tl args2') env') ) conts benchmark
          end
      end
    | Apply_indirect (x, args, stack') -> begin
        benchmark.read <- benchmark.read + 1;
        benchmark.jmp <- benchmark.jmp + 1;
        match get env x with
        | Int k' -> let args', cont = BlockMap.find k' conts in
          interp ((List.map (fun (k, env') -> (k, (List.map (fun arg -> benchmark.push <- benchmark.push + 1; get env arg) env'))) stack')@stack) cont ((List.map2 (fun arg' arg -> benchmark.read <- benchmark.read + 1; arg', get env arg) args' args)) conts benchmark
        | _ -> failwith ("invalid type")
       end

let interp_blocks (blocks : blocks) k env: value * benchmark =
  let benchmark = { const =  0; write = 0; read = 0; add =  0; sub =  0; push =  0; pop =  0; jmp =  0 } in
  let _, e = BlockMap.find k blocks in
  interp [] e env blocks benchmark, benchmark

let pp_benchmark (benchmark: benchmark) fmt: unit =
  Format.fprintf fmt "const: %d; write: %d; read: %d; add: %d; sub: %d; push: %d; pop: %d; jmp: %d\n%!" benchmark.const benchmark.write benchmark.read benchmark.add benchmark.sub benchmark.push benchmark.pop benchmark.jmp

let size_named (named : named): int =
  match named with
  | Prim (_, args) -> 1 + List.length args
  | Var _ -> 1
  | Tuple args -> List.length args
  | Get (_, _) -> 2
  | Pointer _ -> 1

let rec size_expr (cps : expr): int =
    match cps with
    | Let (_, named, expr) -> 1 + size_named named + size_expr expr
    | Apply_direct (_, args, stack) -> 1 + List.length args + List.fold_left (fun size (_, args) -> size + 1 + List.length args) 0 stack
    | If (_, matchs, (_, argsf), stack) -> List.fold_left (fun size (_, _, args) -> size + 1 + List.length args) 0 matchs + 1 + List.length argsf + List.fold_left (fun size (_, args) -> size + 1 + List.length args) 0 stack
    | Return _ -> 1
    | Apply_indirect (_, args, stack) -> 2 + List.length args + List.fold_left (fun size (_, args) -> size + 1 + List.length args) 0 stack

let size_block (args, expr: block): int =
  List.length args + size_expr expr

let size_blocks (blocks: blocks): int =
  BlockMap.fold (fun _ block size -> size + size_block block) blocks 0

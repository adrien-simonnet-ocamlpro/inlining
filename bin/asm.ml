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

let pp_prim (subs: string VarMap.t) (fmt: Format.formatter) (prim : prim) (args: var list): unit =
  match prim, args with
  | Const x, _ -> Format.fprintf fmt "Int %d" x
  | Add, x1 :: x2 :: _ -> Format.fprintf fmt "add %s %s" (gen_name x1 subs) (gen_name x2 subs)
  | Sub, x1 :: x2 :: _ -> Format.fprintf fmt "sub %s %s" (gen_name x1 subs) (gen_name x2 subs)
  | Print, x1 :: _ -> Format.fprintf fmt "print %s" (gen_name x1 subs)
  | _ -> failwith "invalid args"  

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
  | Apply_direct (k, args, []) -> Format.fprintf fmt "\tk%d %a" k (pp_args ~subs ~split: " " ~empty: "") args
  | If (var, matchs, (kf, argsf), []) -> Format.fprintf fmt "\tmatch %s with%s | _ -> k%d %a" (gen_name var subs) (List.fold_left (fun acc (n, kt, argst) -> acc ^ (Format.asprintf "| Int %d -> k%d %a " n kt (pp_args ~subs ~empty: "()" ~split: " ") argst)) " " matchs) kf (pp_args ~subs ~empty: "()" ~split: " ") argsf
  | Return x -> Format.fprintf fmt "\t%s" (gen_name x subs)
  | Apply_indirect (x, args, [(k, kargs)]) -> Format.fprintf fmt "\tk%d (%s %a) %a" k (gen_name x subs) (pp_args ~split:" " ~subs ~empty: "()") args (pp_args ~split:" " ~subs ~empty: "()") kargs
  | Apply_direct (k', args, [(k, kargs)]) -> Format.fprintf fmt "\tk%d (%d %a) %a" k k' (pp_args ~split:" " ~subs ~empty: "()") args (pp_args ~split:" " ~subs ~empty: "()") kargs
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
      inline block (List.fold_left2 (fun alias arg' arg -> VarMap.add arg' arg alias) VarMap.empty args' args) stack' 
    end
  | Apply_direct (k, args, stack') -> Apply_direct (k, args, stack')
  | If (var, matchs, (kf, argsf), stack') -> If (var, matchs, (kf, argsf), stack')
  | Return var -> Return var
  | Apply_indirect (x, args, stack') -> Apply_indirect (x, args, stack')

let inline_blocks (blocks : blocks) (targets: BlockSet.t): blocks =
  let targets = BlockMap.filter (fun k _ -> BlockSet.mem k targets) blocks in
  BlockMap.map (fun (args, block) -> args, inline_parent block targets) blocks

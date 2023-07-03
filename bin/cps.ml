type var = int
type pointer = int
type tag = int
type frame = pointer * var list

module VarMap = Map.Make (Int)
module BlockMap = Map.Make (Int)

type prim = Asm.prim

type named =
| Prim of prim * var list
| Var of var
| Tuple of var list
| Get of var * int
| Closure of pointer * var list
| Constructor of tag * var list

and expr =
| Let of var * named * expr
| Apply_block of pointer * var list
| Call of var * var list * frame
| If of var * (int * pointer * var list) list * (pointer * var list)
| Match_pattern of var * (tag * pointer * var list) list * (pointer * var list)
| Return of var

type block =
| Cont of var list * expr
| Clos of var list * var list * expr
| Return of var * var list * expr
| If_branch of var list * var list * expr
| If_join of var * var list * expr
| Match_branch of var list * var list * var list * expr
| Match_join of var * var list * expr

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

let pp_named (subs: string VarMap.t) (fmt: Format.formatter) named =
match named with
| Prim (prim, args) -> pp_prim subs fmt prim args
| Var x -> Format.fprintf fmt "%s" (gen_name x subs)
| Tuple (args) -> Format.fprintf fmt "Tuple [%a]" (pp_args ~split: "; " ~subs ~empty:"") args
| Get (record, pos) -> Format.fprintf fmt "Get (%s, %d)" (gen_name record subs) pos
| Closure (k, env) -> Format.fprintf fmt "Closure (f%d, %a)" k (pp_args ~split:" " ~subs ~empty: "()") env
| Constructor (tag, env) -> Format.fprintf fmt "Constructor (%d, [%a])" tag (pp_args ~split: "; " ~subs ~empty: "") env

let rec pp_expr (subs: string VarMap.t) (fmt: Format.formatter) (block : expr) : unit =
  match block with
  | Let (var, named, expr) -> Format.fprintf fmt "\tlet %s = %a in\n%a" (gen_name var subs) (pp_named subs) named (pp_expr subs) expr
  | Apply_block (k, args) -> Format.fprintf fmt "\tk%d %a" k (pp_args ~subs ~split: " " ~empty: "") args
  | If (var, matchs, (kf, argsf)) -> Format.fprintf fmt "\tmatch %s with%s | _ -> k%d %a" (gen_name var subs) (List.fold_left (fun acc (n, kt, argst) -> acc ^ (Format.asprintf "| Int %d -> k%d %a " n kt (pp_args ~subs ~empty: "()" ~split: " ") argst)) " " matchs) kf (pp_args ~subs ~empty: "()" ~split: " ") argsf
  | Match_pattern (var, matchs, (kf, argsf)) -> Format.fprintf fmt "\tmatch %s with%s | _ -> k%d %a" (gen_name var subs) (List.fold_left (fun acc (n, kt, argst) -> acc ^ (Format.asprintf "| Int %d -> f%d %a " n kt (pp_args ~subs ~empty: "()" ~split: " ") argst)) " " matchs) kf (pp_args ~subs ~empty: "()" ~split: " ") argsf
  | Return x -> Format.fprintf fmt "\t%s" (gen_name x subs)
  | Call (x, args, (k, kargs)) -> Format.fprintf fmt "\tk%d (%s %a) %a" k (gen_name x subs) (pp_args ~split:" " ~subs ~empty: "()") args (pp_args ~split:" " ~subs ~empty: "()") kargs

let pp_block (subs: string VarMap.t) (fmt: Format.formatter) (block : block) : unit =
  match block with
  | Cont (args, e1) -> Format.fprintf fmt "%a =\n%a" (pp_args ~subs ~empty: "()" ~split: " ") args (pp_expr subs) e1
  | Clos (env, args, e1) -> Format.fprintf fmt "%a %a =\n%a" (pp_args ~subs ~empty: "()" ~split: " ") env (pp_args ~subs ~empty: "()" ~split: " ") args (pp_expr subs) e1
  | Return (arg, args, e1) -> Format.fprintf fmt "%s %a =\n%a" (gen_name arg subs) (pp_args ~subs ~empty: "()" ~split: " ") args (pp_expr subs) e1
  | If_branch (args, fvs, e1) -> Format.fprintf fmt "%a %a =\n%a" (pp_args ~subs ~empty: "()" ~split: " ") args (pp_args ~subs ~empty: "()" ~split: " ") fvs (pp_expr subs) e1
  | If_join (arg, args, e1) -> Format.fprintf fmt "%s %a =\n%a" (gen_name arg subs) (pp_args ~subs ~empty: "()" ~split: " ") args (pp_expr subs) e1
  | Match_branch (env, args, fvs, e1) -> Format.fprintf fmt "%a %a %a =\n%a" (pp_args ~subs ~empty: "()" ~split: " ") env (pp_args ~subs ~empty: "()" ~split: " ") args (pp_args ~subs ~empty: "()" ~split: " ") fvs (pp_expr subs) e1
  | Match_join (arg, args, e1) -> Format.fprintf fmt "%s %a =\n%a" (gen_name arg subs) (pp_args ~subs ~empty: "()" ~split: " ") args (pp_expr subs) e1
  
let pp_blocks (subs: string VarMap.t) (fmt: Format.formatter) (block : blocks) : unit = BlockMap.iter (fun k block -> Format.fprintf fmt "k%d %a\n" k (pp_block subs) block) block

let update_var (var: var) (alias: var VarMap.t): var = if VarMap.mem var alias then VarMap.find var alias else var
let update_vars (vars: var list) (alias: var VarMap.t): var list = List.map (fun var -> update_var var alias) vars

let clean_named (named: named) (alias: var VarMap.t): named =
  match named with
  | Prim (prim, vars) -> Prim (prim, update_vars vars alias)
  | Var var -> Var (update_var var alias)
  | Tuple vars -> Tuple (update_vars vars alias)
  | Get (record, pos) -> Get (update_var record alias, pos)
  | Closure (k, vars) -> Closure (k, update_vars vars alias)
  | Constructor (tag, vars) -> Constructor (tag, update_vars vars alias)

let rec clean_expr (expr: expr) (alias: var VarMap.t): expr =
  match expr with
  | Let (var, Var var', expr') -> clean_expr expr' (VarMap.add var var' alias)
  | Let (var, named, expr) -> Let (var, clean_named named alias, clean_expr expr alias)
  | Apply_block (k, args) -> Apply_block (k, update_vars args alias)
  | If (var, matchs, (kf, argsf)) -> If (update_var var alias, List.map (fun (n, k, args) -> n, k, update_vars args alias) matchs, (kf, update_vars argsf alias))
  | Match_pattern (pattern_id, matchs, (kf, argsf)) -> Match_pattern (update_var pattern_id alias, List.map (fun (n, k, args) -> n, k, update_vars args alias) matchs, (kf, update_vars argsf alias))
  | Return var -> Return (update_var var alias)
  | Call (x, args, (k, kargs)) -> Call (update_var x alias, update_vars args alias, (k, update_vars kargs alias))

let clean_block (block: block): block =
  match block with
  | Cont (args', e1) -> Cont (args', clean_expr e1 (VarMap.empty))
  | Clos (body_free_variables, args', e1) -> Clos (body_free_variables, args', clean_expr e1 (VarMap.empty))
  | Return (result, args', e1) -> Return (result, args', clean_expr e1 (VarMap.empty))
  | If_branch (args, fvs, e1) -> If_branch (args, fvs, clean_expr e1 (VarMap.empty))
  | If_join (arg, args, e1) -> If_join (arg, args, clean_expr e1 (VarMap.empty))
  | Match_branch (env, args, fvs, e1) -> Match_branch (env, args, fvs, clean_expr e1 (VarMap.empty))
  | Match_join (arg, args, e1) -> Match_join (arg, args, clean_expr e1 (VarMap.empty))
  
let clean_blocks: blocks -> blocks = BlockMap.map clean_block

let inc (vars: var Seq.t): var * var Seq.t =
  match Seq.uncons vars with
  | None -> assert false
  | Some (i, vars') -> i, vars'

let rec named_to_asm (var: var) (named: named) (expr: expr) (vars: var Seq.t): Asm.expr * var Seq.t =
  match named with
  | Prim (prim, args) -> begin
      let asm, vars = expr_to_asm expr vars in
      Let (var, Prim (prim, args), asm), vars
    end
  | Var x -> begin
      let asm, vars = expr_to_asm expr vars in
      Let (var, Var x, asm), vars
    end
  | Tuple args -> begin
      let asm, vars = expr_to_asm expr vars in
      Let (var, Tuple args, asm), vars
    end
  | Get (record, pos) -> begin
      let asm, vars = expr_to_asm expr vars in
      Let (var, Get (record, pos), asm), vars
    end
  | Closure (k, env) -> begin
      let k_id, vars = inc vars in
      let env_id, vars = inc vars in
      let asm, vars = expr_to_asm expr vars in
      Let (k_id, Pointer k, Let (env_id, Tuple env, Let (var, Tuple [k_id; env_id], asm))), vars
    end
  | Constructor (tag, env) -> begin
      let tag_id, vars = inc vars in
      let env_id, vars = inc vars in
      let asm, vars = expr_to_asm expr vars in
      Let (tag_id, Prim (Const tag, []), Let (env_id, Tuple env, Let (var, Tuple [tag_id; env_id], asm))), vars
    end

and expr_to_asm (block: expr) (vars: var Seq.t): Asm.expr * int Seq.t =
  match block with
  | Let (var, named, expr) -> named_to_asm var named expr vars
  | Apply_block (k, args) -> Apply_direct (k, args, []), vars
  | If (var, matchs, (kf, argsf)) -> If (var, matchs, (kf, argsf), []), vars
  | Match_pattern (cons, matchs, (kf, argsf)) -> begin
      let tag_id, vars = inc vars in
      let payload_id, vars = inc vars in
      Asm.Let (tag_id, Get (cons, 0), (Asm.Let (payload_id, Get (cons, 1), If (tag_id, List.map (fun (n, k, args) -> (n, k, payload_id :: args)) matchs, (kf, argsf), [])))), vars
    end
  | Return var -> Return var, vars
  | Call (clos, args, frame) -> begin
      let k_id, vars = inc vars in
      let env_id, vars = inc vars in
      Let (k_id, Get (clos, 0), Let (env_id, Get (clos, 1), Apply_indirect (k_id, env_id :: args, [frame]))), vars
    end

let block_to_asm (block: block) (vars: var Seq.t): Asm.block * var Seq.t =
  match block with
  | Cont (args', e1) -> begin
      let asm1, vars = expr_to_asm e1 vars in
      (args', asm1), vars
    end
  | Return (arg, args', e1) -> begin
      let asm1, vars = expr_to_asm e1 vars in
      (arg :: args', asm1), vars
    end
  | Clos (body_free_variables, args', e1) -> begin
      let environment_id, vars = inc vars in
      let asm1, vars = expr_to_asm e1 vars in
      let body = List.fold_left (fun block' (pos, body_free_variable) -> Asm.Let (body_free_variable, Asm.Get (environment_id, pos), block')) asm1 (List.mapi (fun i fv -> i, fv) body_free_variables) in
      (environment_id :: args', body), vars
    end
  | If_branch (args, fvs, e) -> begin
      let asm1, vars = expr_to_asm e vars in
      (args @ fvs, asm1), vars
    end
  | If_join (arg, args, e) -> begin
      let asm1, vars = expr_to_asm e vars in
      (arg :: args, asm1), vars
    end
  | Match_branch (body_free_variables, args', fvs, e) -> begin
      let environment_id, vars = inc vars in
      let asm1, vars = expr_to_asm e vars in
      let body = List.fold_left (fun block' (pos, body_free_variable) -> Asm.Let (body_free_variable, Asm.Get (environment_id, pos), block')) asm1 (List.mapi (fun i fv -> i, fv) body_free_variables) in
      (environment_id :: args' @ fvs, body), vars
    end
  | Match_join (arg, args, e) -> begin
      let asm1, vars = expr_to_asm e vars in
      (arg :: args, asm1), vars
    end

let blocks_to_asm (blocks: blocks) (vars: var Seq.t): Asm.blocks * var Seq.t = BlockMap.fold (fun k block (blocks, vars) -> begin
    let block, vars = block_to_asm block vars in
    Asm.BlockMap.add k block blocks, vars
  end) blocks (Asm.BlockMap.empty, vars)


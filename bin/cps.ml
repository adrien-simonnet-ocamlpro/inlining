type var = int
type pointer = int
type tag = int
type frame = pointer * var list

module VarMap = Map.Make (Int)
module BlockMap = Map.Make (Int)
module BlockSet = Set.Make (Int)

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
| Call_direct of pointer * var * var list * frame
| Call of var * var list * frame
| If of var * (int * pointer * var list) list * (pointer * var list) * var list
| Match_pattern of var * (tag * pointer * var list * var list) list * (pointer * var list) * var list
| Return of var
| If_return of pointer * var * var list
| Match_return of pointer * var * var list

type block =
| Cont of var list
| Clos of var list * var list
| Return of var * var list
| If_branch of var list * var list
| If_join of var * var list
| Match_branch of var list * var list * var list
| Match_join of var * var list

type blocks = (block * expr) BlockMap.t

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
  | If (var, matchs, (kf, argsf), fvs) -> Format.fprintf fmt "\tmatch %s with%s | _ -> k%d %a" (gen_name var subs) (List.fold_left (fun acc (n, kt, argst) -> acc ^ (Format.asprintf "| Int %d -> k%d %a " n kt (pp_args ~subs ~empty: "()" ~split: " ") (argst @ fvs))) " " matchs) kf (pp_args ~subs ~empty: "()" ~split: " ") (argsf @ fvs)
  | Match_pattern (var, matchs, (kf, argsf), fvs) -> Format.fprintf fmt "\tmatch %s with%s | _ -> k%d %a" (gen_name var subs) (List.fold_left (fun acc (n, kt, pld, argst) -> acc ^ (Format.asprintf "| Int %d (%a) -> f%d %a " n (pp_args ~subs ~empty: "()" ~split: " ") pld kt (pp_args ~subs ~empty: "()" ~split: " ") (argst @ fvs))) " " matchs) kf (pp_args ~subs ~empty: "()" ~split: " ") (argsf @ fvs)
  | Return x -> Format.fprintf fmt "\t%s" (gen_name x subs)
  | If_return (k, arg, args) -> Format.fprintf fmt "\tk%d %s %a" k (gen_name arg subs) (pp_args ~subs ~split: " " ~empty: "") args
  | Match_return (k, arg, args) -> Format.fprintf fmt "\tk%d %s %a" k (gen_name arg subs) (pp_args ~subs ~split: " " ~empty: "") args
  | Call (x, args, (k, kargs)) -> Format.fprintf fmt "\tk%d (%s %a) %a" k (gen_name x subs) (pp_args ~split:" " ~subs ~empty: "()") args (pp_args ~split:" " ~subs ~empty: "()") kargs
  | Call_direct (k', x, args, (k, kargs)) -> Format.fprintf fmt "\tk%d (k%d %s %a) %a" k k' (gen_name x subs) (pp_args ~split:" " ~subs ~empty: "()") args (pp_args ~split:" " ~subs ~empty: "()") kargs

let pp_block (subs: string VarMap.t) (fmt: Format.formatter) (block : block) : unit =
  match block with
  | Cont args -> Format.fprintf fmt "Cont %a" (pp_args ~subs ~empty: "()" ~split: " ") args
  | Clos (env, args) -> Format.fprintf fmt "Closure %a %a" (pp_args ~subs ~empty: "()" ~split: " ") env (pp_args ~subs ~empty: "()" ~split: " ") args
  | Return (arg, args) -> Format.fprintf fmt "Return %s %a" (gen_name arg subs) (pp_args ~subs ~empty: "()" ~split: " ") args
  | If_branch (args, fvs) -> Format.fprintf fmt "If_branch %a %a" (pp_args ~subs ~empty: "()" ~split: " ") args (pp_args ~subs ~empty: "()" ~split: " ") fvs
  | If_join (arg, args) -> Format.fprintf fmt "If_join %s %a" (gen_name arg subs) (pp_args ~subs ~empty: "()" ~split: " ") args
  | Match_branch (env, args, fvs) -> Format.fprintf fmt "Match_branch %a %a %a" (pp_args ~subs ~empty: "()" ~split: " ") env (pp_args ~subs ~empty: "()" ~split: " ") args (pp_args ~subs ~empty: "()" ~split: " ") fvs
  | Match_join (arg, args) -> Format.fprintf fmt "Match_join %s %a" (gen_name arg subs) (pp_args ~subs ~empty: "()" ~split: " ") args
  
let pp_blocks (subs: string VarMap.t) (fmt: Format.formatter) (block : blocks) : unit = BlockMap.iter (fun k (block, expr) -> Format.fprintf fmt "k%d %a =\n%a\n%!" k (pp_block subs) block (pp_expr subs) expr) block

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
  | If (var, matchs, (kf, argsf), fvs) -> If (update_var var alias, List.map (fun (n, k, args) -> n, k, update_vars args alias) matchs, (kf, update_vars argsf alias), update_vars fvs alias)
  | Match_pattern (pattern_id, matchs, (kf, argsf), fvs) -> Match_pattern (update_var pattern_id alias, List.map (fun (n, k, pld, args) -> n, k, pld, update_vars args alias) matchs, (kf, update_vars argsf alias), update_vars fvs alias)
  | Return var -> Return (update_var var alias)
  | If_return (k, arg, args) -> If_return (k, update_var arg alias, update_vars args alias)
  | Match_return (k, arg, args) -> Match_return (k, update_var arg alias, update_vars args alias)
  | Call (x, args, (k, kargs)) -> Call (update_var x alias, update_vars args alias, (k, update_vars kargs alias))
  | Call_direct (k', x, args, (k, kargs)) -> Call_direct (k', update_var x alias, update_vars args alias, (k, update_vars kargs alias))

let clean_blocks: blocks -> blocks = BlockMap.map (fun (block, expr) -> block, clean_expr expr VarMap.empty)

let inc (vars: var Seq.t): var * var Seq.t =
  match Seq.uncons vars with
  | None -> assert false
  | Some (i, vars') -> i, vars'

let rec copy_expr (expr: expr) (vars: var Seq.t) (alias: var VarMap.t): expr * var Seq.t=
  match expr with
  | Let (var, named, expr) -> begin
      let var_id, vars = inc vars in
      let expr, vars = copy_expr expr vars (VarMap.add var var_id alias) in
      Let (var_id, clean_named named alias, expr), vars
    end
  | Apply_block (k, args) -> Apply_block (k, update_vars args alias), vars
  | If (var, matchs, (kf, argsf), fvs) -> If (update_var var alias, List.map (fun (n, k, args) -> n, k, update_vars args alias) matchs, (kf, update_vars argsf alias), update_vars fvs alias), vars
  | Match_pattern (pattern_id, matchs, (kf, argsf), fvs) -> Match_pattern (update_var pattern_id alias, List.map (fun (n, k, pld, args) -> n, k, pld, update_vars args alias) matchs, (kf, update_vars argsf alias), update_vars fvs alias), vars
  | Return var -> Return (update_var var alias), vars
  | If_return (k, arg, args) -> If_return (k, update_var arg alias, update_vars args alias), vars
  | Match_return (k, arg, args) -> Match_return (k, update_var arg alias, update_vars args alias), vars
  | Call (x, args, (k, kargs)) -> Call (update_var x alias, update_vars args alias, (k, update_vars kargs alias)), vars
  | Call_direct (k', x, args, (k, kargs)) -> Call_direct (k', update_var x alias, update_vars args alias, (k, update_vars kargs alias)), vars

let rec copy_callee (expr: expr) (vars: var Seq.t) (pointers: pointer Seq.t) (blocks: blocks): expr * blocks * var Seq.t * pointer Seq.t =
  match expr with
  | Let (var, named, expr) -> begin
      let expr, blocks', vars, pointers = copy_callee expr vars pointers blocks in
      Let (var, named, expr), blocks', vars, pointers
    end
  | Apply_block (k, args) when BlockMap.mem k blocks -> begin
      let block, expr = BlockMap.find k blocks in
      let expr', vars = copy_expr expr vars VarMap.empty in
      let k_id, pointers = inc pointers in
      Apply_block (k_id, args), BlockMap.singleton k_id (block, expr'), vars, pointers
    end
  | Apply_block (k, args) -> Apply_block (k, args), BlockMap.empty, vars, pointers
  | If (var, matchs, (kf, argsf), fvs) -> If (var, matchs, (kf, argsf), fvs), BlockMap.empty, vars, pointers
  | Match_pattern (pattern_id, matchs, (kf, argsf), fvs) -> Match_pattern (pattern_id, matchs, (kf, argsf), fvs), BlockMap.empty, vars, pointers
  | Return var -> Return var, BlockMap.empty, vars, pointers
  | If_return (k, arg, args) when BlockMap.mem k blocks -> begin
      let block, expr = BlockMap.find k blocks in
      let expr', vars = copy_expr expr vars VarMap.empty in
      let k_id, pointers = inc pointers in
      If_return (k_id, arg, args), BlockMap.singleton k_id (block, expr'), vars, pointers
    end
  | If_return (k, arg, args) -> If_return (k, arg, args), BlockMap.empty, vars, pointers
  | Match_return (k, arg, args) when BlockMap.mem k blocks -> begin
      let block, expr = BlockMap.find k blocks in
      let expr', vars = copy_expr expr vars VarMap.empty in
      let k_id, pointers = inc pointers in
      Match_return (k_id, arg, args), BlockMap.singleton k_id (block, expr'), vars, pointers
    end
  | Match_return (k, arg, args) -> Match_return (k, arg, args), BlockMap.empty, vars, pointers
  | Call (x, args, (k, kargs)) -> Call (x, args, (k, kargs)), BlockMap.empty, vars, pointers
  | Call_direct (k', x, args, (k, kargs)) when BlockMap.mem k' blocks -> begin
      let block, expr = BlockMap.find k' blocks in
      let expr', vars = copy_expr expr vars VarMap.empty in
      let k_id, pointers = inc pointers in
      Call_direct (k_id, x, args, (k, kargs)), BlockMap.singleton k_id (block, expr'), vars, pointers
    end
  | Call_direct (k', x, args, (k, kargs)) -> Call_direct (k', x, args, (k, kargs)), BlockMap.empty, vars, pointers

let copy_blocks (blocks: blocks) (targets: BlockSet.t) (vars: var Seq.t) (pointers: pointer Seq.t): blocks * var Seq.t * pointer Seq.t =
  let targets = BlockMap.filter (fun k _ -> BlockSet.mem k targets) blocks in
  BlockMap.fold (fun k (block, expr) (blocks, vars, pointers) -> begin
    let expr', blocks', vars, pointers = copy_callee expr vars pointers targets in
    BlockMap.union (fun _ _ _ -> assert false) blocks' (BlockMap.add k (block, expr') blocks), vars, pointers
  end) blocks (BlockMap.empty, vars, pointers)

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
  | If (var, matchs, (kf, argsf), fvs) -> If (var, List.map (fun (n, k, argst) -> n, k, argst @ fvs) matchs, (kf, argsf @ fvs), []), vars
  | Match_pattern (cons, matchs, (kf, argsf), fvs) -> begin
      let tag_id, vars = inc vars in
      let payload_id, vars = inc vars in
      Asm.Let (tag_id, Get (cons, 0), (Asm.Let (payload_id, Get (cons, 1), If (tag_id, List.map (fun (n, k, _, args) -> (n, k, payload_id :: args @ fvs)) matchs, (kf, payload_id :: argsf @ fvs), [])))), vars
    end
  | Return var -> Return var, vars
  | If_return (k, arg, args) -> Apply_direct (k, arg :: args, []), vars
  | Match_return (k, arg, args) -> Apply_direct (k, arg :: args, []), vars
  | Call (clos, args, frame) -> begin
      let k_id, vars = inc vars in
      let env_id, vars = inc vars in
      Let (k_id, Get (clos, 0), Let (env_id, Get (clos, 1), Apply_indirect (k_id, env_id :: args, [frame]))), vars
    end
  | Call_direct (k, clos, args, frame) -> begin
      let env_id, vars = inc vars in
      Let (env_id, Get (clos, 1), Apply_direct (k, env_id :: args, [frame])), vars
    end

let block_to_asm (block: block) (asm1: Asm.expr) (vars: Asm.var Seq.t) (pointers: Asm.pointer Seq.t): Asm.block * var Seq.t * Asm.pointer Seq.t * Asm.blocks =
  match block with
  | Cont (args') -> (args', asm1), vars, pointers, Asm.BlockMap.empty
  | Return (arg, args') -> (arg :: args', asm1), vars, pointers, Asm.BlockMap.empty
  | Clos (body_free_variables, args') -> begin
      let function_id, pointers = inc pointers in
      let environment_id, vars = inc vars in
      let body = List.fold_left (fun block' (pos, body_free_variable) -> Asm.Let (body_free_variable, Asm.Get (environment_id, pos), block')) (Apply_direct (function_id, args' @ body_free_variables, [])) (List.mapi (fun i fv -> i, fv) body_free_variables) in
      (environment_id :: args', body), vars, pointers, Asm.BlockMap.singleton function_id (args' @ body_free_variables, asm1)
    end
  | If_branch (args, fvs) -> (args @ fvs, asm1), vars, pointers, Asm.BlockMap.empty
  | If_join (arg, args) -> (arg :: args, asm1), vars, pointers, Asm.BlockMap.empty
  | Match_branch (body_free_variables, args', fvs) -> begin
      let environment_id, vars = inc vars in
      let body = List.fold_left (fun block' (pos, body_free_variable) -> Asm.Let (body_free_variable, Asm.Get (environment_id, pos), block')) asm1 (List.mapi (fun i fv -> i, fv) body_free_variables) in
      (environment_id :: args' @ fvs, body), vars, pointers, Asm.BlockMap.empty
    end
  | Match_join (arg, args) -> (arg :: args, asm1), vars, pointers, Asm.BlockMap.empty

let blocks_to_asm (blocks: blocks) (vars: Asm.var Seq.t) (pointers: Asm.pointer Seq.t): Asm.blocks * Asm.var Seq.t * Asm.pointer Seq.t =
  BlockMap.fold (fun k (block, expr) (blocks, vars, pointers) -> begin
    let asm, vars = expr_to_asm expr vars in  
    let block, vars, pointers, blocks' = block_to_asm block asm vars pointers in
    Asm.BlockMap.add k block (Asm.BlockMap.union (fun _ -> assert false) blocks blocks'), vars, pointers
  end) blocks (Asm.BlockMap.empty, vars, pointers)

let size_named (named : named): int =
  match named with
  | Prim (_, args) -> 1 + List.length args
  | Var _ -> 1
  | Tuple args -> List.length args
  | Get (_, _) -> 2
  | Closure (_, args) -> List.length args
  | Constructor (_, args) -> List.length args

let rec size_expr (cps : expr): int =
  match cps with
  | Let (_, named, expr) -> 1 + size_named named + size_expr expr
  | Apply_block (_, args) -> 1 + List.length args
  | If (_, matchs, (_, argsf), fvs) -> List.fold_left (fun size (_, _, args) -> size + 1 + List.length args + List.length fvs) 0 matchs + 1 + List.length argsf + List.length fvs
  | Match_pattern (_, matchs, (_, argsf), fvs) -> List.fold_left (fun size (_, _, pld, args) -> size + 1 + List.length pld + List.length args + List.length fvs) 0 matchs + 1 + List.length argsf + List.length fvs
  | Return _ -> 1
  | If_return (_, _, args) -> 2 + List.length args
  | Match_return (_, _, args) -> 2 + List.length args
  | Call (_, args, (_, args')) -> 2 + List.length args + 1 + List.length args'
  | Call_direct (_, _, args, (_, args')) -> 2 + List.length args + 1 + List.length args'

let size_block (block: block): int =
  match block with
  | Cont (args') -> List.length args'
  | Return (_, args') -> 1 + List.length args'
  | Clos (body_free_variables, args') -> List.length body_free_variables + List.length args'
  | If_branch (args, fvs) -> List.length fvs + List.length args
  | If_join (_, args) -> 1 + List.length args
  | Match_branch (body_free_variables, args', fvs) -> List.length body_free_variables + List.length fvs + List.length args'
  | Match_join (_, args) -> 1 + List.length args

let size_blocks (blocks: blocks): int =
  BlockMap.fold (fun _ (block, expr) size -> size + size_block block + size_expr expr) blocks 0

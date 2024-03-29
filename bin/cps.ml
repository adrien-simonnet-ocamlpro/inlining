open Utils

(* Identifier for variables *)
type var = int

(* Identifier for constructor tags *)
type tag = int

(* Identifier for pointers *)
type pointer = int

(* Module for variables map. *)
module VarMap = Map.Make (Int)

(* Module for variables set. *)
module VarSet = Set.Make (Int)

(* Module for pointers map. *)
module PointerMap = Map.Make (Int)

(* Module for pointers set. *)
module PointerSet = Set.Make (Int)

(* Expressions *)
type expr =
| Int of int (* Integer *)
| Add of var * var (* Addition *)
| Sub of var * var (* *)
| Var of var (* Alias *)
| Tuple of var list (* Tuple *)
| Get of var * int (* Read tuple field *)
| Closure of pointer * VarSet.t (* Closure construction *)
| Constructor of tag * var list (* Data construction *)

(* Instructions *)
type instr =
| Let of var * expr * instr (* Declaration of variable with associated value *)
| Call_direct of pointer * var * var list * (pointer * VarSet.t)
| Call of var * var list * (pointer * VarSet.t) (* Closure call with parameters and callback. *)
| If of var * (pointer * VarSet.t) * (pointer * VarSet.t) * VarSet.t (* If condition Then Else *)
| Match_pattern of var * (tag * var list * pointer * VarSet.t) list * (pointer * VarSet.t) * VarSet.t (* Pattern matching *)
| Return of var (* Return value to caller *)
| If_return of pointer * var * VarSet.t (* Return value from if *)
| Match_return of pointer * var * VarSet.t (* Return value from pattern matching *)

(* Blocks *)
type block =
| Clos of VarSet.t * var list (* Closure of environment (free variables) and parameters *)
| Return of var * VarSet.t (* Block called when function returns *)
| If_branch of VarSet.t * VarSet.t (* Then or Else branch *)
| If_join of var * VarSet.t (* Block joining Then and Else branchs *)
| Match_branch of var list * VarSet.t * VarSet.t (* Branch of pattern matching *)
| Match_join of var * VarSet.t (* Block joining branchs of pattern matching *)

(* Program *)
type blocks = (block * instr) PointerMap.t

let pp_args (subs: string VarMap.t) (fmt: Format.formatter) (args: var list): unit =
  match args with
  | [] -> Format.fprintf fmt "[]"
  | [ arg ] -> Format.fprintf fmt "[ %s ]" (string_of_sub arg subs)
  | vars -> Format.fprintf fmt "[ "; List.iter (fun var -> Format.fprintf fmt "%s; " (string_of_sub var subs)) vars; Format.fprintf fmt "]"

let pp_env (subs: string VarMap.t) (fmt: Format.formatter) (env: VarSet.t): unit =
  match (VarSet.elements env) with
  | [] -> Format.fprintf fmt "{}"
  | [ arg ] -> Format.fprintf fmt "{ %s }" (string_of_sub arg subs)
  | vars -> Format.fprintf fmt "{ "; List.iter (fun var -> Format.fprintf fmt "%s, " (string_of_sub var subs)) vars; Format.fprintf fmt "}"

let pp_expr (subs: string VarMap.t) (fmt: Format.formatter) expr =
  match expr with
  | Int x -> Format.fprintf fmt "Int %d" x
  | Add (x1, x2) -> Format.fprintf fmt "add %s %s" (string_of_sub x1 subs) (string_of_sub x2 subs)
  | Sub (x1, x2) -> Format.fprintf fmt "sub %s %s" (string_of_sub x1 subs) (string_of_sub x2 subs)
  | Var x -> Format.fprintf fmt "%s" (string_of_sub x subs)
  | Tuple (args) -> Format.fprintf fmt "Tuple [%a]" (pp_args subs) args
  | Get (record, pos) -> Format.fprintf fmt "Get (%s, %d)" (string_of_sub record subs) pos
  | Closure (k, env) -> Format.fprintf fmt "Closure (f%d, %a)" k (pp_env subs) env
  | Constructor (tag, env) -> Format.fprintf fmt "Constructor (%d, [%a])" tag (pp_args subs) env

let rec pp_instr (subs: string VarMap.t) (fmt: Format.formatter) (block : instr) : unit =
  match block with
  | Let (var, expr, instr) -> Format.fprintf fmt "\tlet %s = %a in\n%a" (string_of_sub var subs) (pp_expr subs) expr (pp_instr subs) instr
  | If (var, (kt, argst), (kf, argsf), fvs) -> Format.fprintf fmt "\tif %s = 0 then k%d %a else k%d %a" (string_of_sub var subs) kt (pp_env subs) (VarSet.union argst fvs) kf (pp_env subs) (VarSet.union argsf fvs)
  | Match_pattern (var, matchs, (kf, argsf), fvs) -> Format.fprintf fmt "\tmatch %s with%s | _ -> k%d %a" (string_of_sub var subs) (List.fold_left (fun acc (n, pld, kt, argst) -> acc ^ (Format.asprintf "| Int %d (%a) -> f%d %a " n (pp_args subs) pld kt (pp_env subs) (VarSet.union argst fvs))) " " matchs) kf (pp_env subs) (VarSet.union argsf fvs)
  | Return x -> Format.fprintf fmt "\t%s" (string_of_sub x subs)
  | If_return (k, arg, args) -> Format.fprintf fmt "\tk%d %s %a" k (string_of_sub arg subs) (pp_env subs) args
  | Match_return (k, arg, args) -> Format.fprintf fmt "\tk%d %s %a" k (string_of_sub arg subs) (pp_env subs) args
  | Call (x, args, (k, kargs)) -> Format.fprintf fmt "\tk%d (%s %a) %a" k (string_of_sub x subs) (pp_args subs) args (pp_env subs) kargs
  | Call_direct (k', x, args, (k, kargs)) -> Format.fprintf fmt "\tk%d (k%d %s %a) %a" k k' (string_of_sub x subs) (pp_args subs) args (pp_env subs) kargs

let pp_block (subs: string VarMap.t) (fmt: Format.formatter) (block : block) : unit =
  match block with
  | Clos (env, args) -> Format.fprintf fmt "Closure %a %a" (pp_args subs) args (pp_env subs) env
  | Return (arg, args) -> Format.fprintf fmt "Return %s %a" (string_of_sub arg subs) (pp_env subs) args
  | If_branch (args, fvs) -> Format.fprintf fmt "If_branch %a %a" (pp_env subs) args (pp_env subs) fvs
  | If_join (arg, args) -> Format.fprintf fmt "If_join %s %a" (string_of_sub arg subs) (pp_env subs) args
  | Match_branch (env, args, fvs) -> Format.fprintf fmt "Match_branch %a %a %a" (pp_env subs) args (pp_args subs) env (pp_env subs) fvs
  | Match_join (arg, args) -> Format.fprintf fmt "Match_join %s %a" (string_of_sub arg subs) (pp_env subs) args
  
let pp_blocks (subs: string VarMap.t) (fmt: Format.formatter) (block : blocks) : unit = PointerMap.iter (fun k (block, instr) -> Format.fprintf fmt "k%d %a =\n%a\n%!" k (pp_block subs) block (pp_instr subs) instr) block

let update_var (var: var) (alias: var VarMap.t): var = if VarMap.mem var alias then VarMap.find var alias else var
let update_vars (vars: var list) (alias: var VarMap.t): var list = List.map (fun var -> update_var var alias) vars
let update_env (vars: VarSet.t) (alias: var VarMap.t): VarSet.t =
  VarSet.map (fun var -> update_var var alias) vars

(* Rename variables in an expression. *)
let clean_expr (expr: expr) (alias: var VarMap.t): expr =
  match expr with
  | Int x -> Int x
  | Add (x1, x2) -> Add (update_var x1 alias, update_var x2 alias)
  | Sub (x1, x2) -> Sub (update_var x1 alias, update_var x2 alias)
  | Var var -> Var (update_var var alias)
  | Tuple vars -> Tuple (update_vars vars alias)
  | Get (record, pos) -> Get (update_var record alias, pos)
  | Closure (k, vars) -> Closure (k, update_env vars alias)
  | Constructor (tag, vars) -> Constructor (tag, update_vars vars alias)

(* Rename variables in an instruction. *)
let rec clean_instr (instr: instr) (aliases: var VarMap.t): instr =
  match instr with
  | Let (var, expr, instr) -> Let (var, clean_expr expr aliases, clean_instr instr aliases)
  | If (var, (kt, argst), (kf, argsf), fvs) -> If (update_var var aliases, (kt, update_env argst aliases), (kf, update_env argsf aliases), update_env fvs aliases)
  | Match_pattern (pattern_id, matchs, (kf, argsf), fvs) -> Match_pattern (update_var pattern_id aliases, List.map (fun (n, k, pld, args) -> n, k, pld, update_env args aliases) matchs, (kf, update_env argsf aliases), update_env fvs aliases)
  | Return var -> Return (update_var var aliases)
  | If_return (k, arg, args) -> If_return (k, update_var arg aliases, update_env args aliases)
  | Match_return (k, arg, args) -> Match_return (k, update_var arg aliases, update_env args aliases)
  | Call (x, args, (k, kargs)) -> Call (update_var x aliases, update_vars args aliases, (k, update_env kargs aliases))
  | Call_direct (k', x, args, (k, kargs)) -> Call_direct (k', update_var x aliases, update_vars args aliases, (k, update_env kargs aliases))

(* Rename variables in a block. *)
let clean_block (block: block) (aliases: var VarMap.t): block =
  match block with
  | Clos (env, args) -> Clos (update_env env aliases, args)
  | Return (arg, args) -> Return (update_var arg aliases, update_env args aliases)
  | If_branch (args, fvs) -> If_branch (update_env args aliases, update_env fvs aliases)
  | If_join (arg, args) -> If_join (update_var arg aliases, update_env args aliases)
  | Match_branch (env, args, fvs) -> Match_branch (update_vars env aliases, update_env args aliases, update_env fvs aliases)
  | Match_join (arg, args) -> Match_join (update_var arg aliases, update_env args aliases)
  
let rec get_aliases (instr: instr) (aliases: var VarMap.t): var VarMap.t =
  match instr with
  | Let (var, Var var', instr') -> get_aliases instr' (VarMap.add var var' aliases)
  | _ -> aliases

let clean_blocks (blocks: blocks): blocks = 
  let aliases = PointerMap.fold (fun _ (_, instr) aliases -> begin
    let aliases = get_aliases instr aliases in
    aliases
  end) blocks VarMap.empty in
  PointerMap.map (fun (block, instr) -> clean_block block aliases, clean_instr instr aliases) blocks

let rec copy_instr (instr: instr) (vars: var Seq.t) (alias: var VarMap.t): instr * var Seq.t=
  match instr with
  | Let (var, expr, instr) -> begin
      let var_id, vars = inc vars in
      let instr, vars = copy_instr instr vars (VarMap.add var var_id alias) in
      Let (var_id, clean_expr expr alias, instr), vars
    end
  | If (var, (kt, argst), (kf, argsf), fvs) -> If (update_var var alias, (kt, update_env argst alias), (kf, update_env argsf alias), update_env fvs alias), vars
  | Match_pattern (pattern_id, matchs, (kf, argsf), fvs) -> Match_pattern (update_var pattern_id alias, List.map (fun (n, k, pld, args) -> n, k, pld, update_env args alias) matchs, (kf, update_env argsf alias), update_env fvs alias), vars
  | Return var -> Return (update_var var alias), vars
  | If_return (k, arg, args) -> If_return (k, update_var arg alias, update_env args alias), vars
  | Match_return (k, arg, args) -> Match_return (k, update_var arg alias, update_env args alias), vars
  | Call (x, args, (k, kargs)) -> Call (update_var x alias, update_vars args alias, (k, update_env kargs alias)), vars
  | Call_direct (k', x, args, (k, kargs)) -> Call_direct (k', update_var x alias, update_vars args alias, (k, update_env kargs alias)), vars

let rec copy_callee (instr: instr) (vars: var Seq.t) (pointers: pointer Seq.t) (blocks: blocks) (should_copy: pointer -> bool): instr * blocks * var Seq.t * pointer Seq.t =
  match instr with
  | Let (var, expr, instr) -> begin
      let instr, blocks', vars, pointers = copy_callee instr vars pointers blocks should_copy in
      Let (var, expr, instr), blocks', vars, pointers
    end
  | If (var, matchs, (kf, argsf), fvs) -> If (var, matchs, (kf, argsf), fvs), PointerMap.empty, vars, pointers
  | Match_pattern (pattern_id, matchs, (kf, argsf), fvs) -> Match_pattern (pattern_id, matchs, (kf, argsf), fvs), PointerMap.empty, vars, pointers
  | Return var -> Return var, PointerMap.empty, vars, pointers
  | If_return (k, arg, args) when should_copy k -> begin
      let block, instr = PointerMap.find k blocks in
      let instr', vars = copy_instr instr vars VarMap.empty in
      let k_id, pointers = inc pointers in
      If_return (k_id, arg, args), PointerMap.add k_id (block, instr') PointerMap.empty, vars, pointers
    end
  | If_return (k, arg, args) -> If_return (k, arg, args), PointerMap.empty, vars, pointers
  | Match_return (k, arg, args) when should_copy k -> begin
      let block, instr = PointerMap.find k blocks in
      let instr', vars = copy_instr instr vars VarMap.empty in
      let k_id, pointers = inc pointers in
      Match_return (k_id, arg, args), PointerMap.add k_id (block, instr') PointerMap.empty, vars, pointers
    end
  | Match_return (k, arg, args) -> Match_return (k, arg, args), PointerMap.empty, vars, pointers
  | Call (x, args, (k, kargs)) -> Call (x, args, (k, kargs)), PointerMap.empty, vars, pointers
  | Call_direct (k', x, args, (k, kargs)) when should_copy k && should_copy k' -> begin
      let block2, instr2 = PointerMap.find k blocks in
      let return_id, pointers = inc pointers in
      let instr2', vars = copy_instr instr2 vars VarMap.empty in
      let block, instr = PointerMap.find k' blocks in
      let instr', vars = copy_instr instr vars VarMap.empty in
      let k_id, pointers = inc pointers in
      Call_direct (k_id, x, args, (return_id, kargs)), PointerMap.add return_id (block2, instr2') (PointerMap.add k_id (block, instr') PointerMap.empty), vars, pointers
    end
  | Call_direct (k', x, args, (k, kargs)) -> Call_direct (k', x, args, (k, kargs)), PointerMap.empty, vars, pointers

let copy_blocks (blocks0: blocks) (vars: var Seq.t) (pointers: pointer Seq.t) (should_copy: pointer -> bool): blocks * var Seq.t * pointer Seq.t =
  PointerMap.fold (fun k (block, instr) (blocks, vars, pointers) -> begin
    let instr', blocks', vars, pointers = copy_callee instr vars pointers blocks0 should_copy in
    PointerMap.union (fun _ _ _ -> assert false) blocks' (PointerMap.add k (block, instr') blocks), vars, pointers
  end) blocks0 (PointerMap.empty, vars, pointers)

let fvs_to_list fvs = VarSet.elements fvs

let expr_to_asm (var: var) (expr: expr) (asm: Asm.instr) (vars: var Seq.t): Asm.instr * var Seq.t =
  match expr with
  | Int x -> Let (var, Int x, asm), vars
  | Add (x1, x2) -> Let (var, Add (x1, x2), asm), vars
  | Sub (x1, x2) -> Let (var, Sub (x1, x2), asm), vars
  | Var x -> Let (var, Var x, asm), vars
  | Tuple args -> Let (var, Tuple args, asm), vars
  | Get (record, pos) -> Let (var, Get (record, pos), asm), vars
  | Closure (k, env) -> begin
      let k_id, vars = inc vars in
      let env_id, vars = inc vars in
      Let (k_id, Pointer k, Let (env_id, Tuple (fvs_to_list env), Let (var, Tuple [k_id; env_id], asm))), vars
    end
  | Constructor (tag, env) -> begin
      let tag_id, vars = inc vars in
      let env_id, vars = inc vars in
      Let (tag_id, Int tag, Let (env_id, Tuple env, Let (var, Tuple [tag_id; env_id], asm))), vars
    end

let rec instr_to_asm (block: instr) (vars: var Seq.t) (pointers: Asm.pointer Seq.t): Asm.instr * Asm.var Seq.t * Asm.pointer Seq.t * Asm.blocks =
  match block with
  | Let (var, expr, instr) -> begin
      let asm, vars, pointers, blocks = instr_to_asm instr vars pointers in
      let asm, vars = expr_to_asm var expr asm vars in
      asm, vars, pointers, blocks
    end
  | If (var, (kt, argst), (kf, argsf), fvs) -> Switch (var, [ 0, kf, (fvs_to_list argsf) @ (fvs_to_list fvs) ], (kt, (fvs_to_list argst) @ (fvs_to_list fvs)), []), vars, pointers, Asm.PointerMap.empty
  | Match_pattern (cons, matchs, (kf, argsf), fvs) -> begin
      let tag_id, vars = inc vars in
      let payload_id, vars = inc vars in
      Asm.Let (tag_id, Get (cons, 0), (Asm.Let (payload_id, Get (cons, 1), Switch (tag_id, List.map (fun (n, _, k, args) -> (n, k, payload_id :: (fvs_to_list args) @ (fvs_to_list fvs))) matchs, (kf, payload_id :: (fvs_to_list argsf) @ (fvs_to_list fvs)), [])))), vars, pointers, Asm.PointerMap.empty
    end
  | Return var -> Return var, vars, pointers, Asm.PointerMap.empty
  | If_return (k, arg, args) -> Apply_direct (k, arg :: (fvs_to_list args), []), vars, pointers, Asm.PointerMap.empty
  | Match_return (k, arg, args) -> Apply_direct (k, arg :: (fvs_to_list args), []), vars, pointers, Asm.PointerMap.empty
  | Call (clos, args, (p, args')) -> begin
      let k_id, vars = inc vars in
      let env_id, vars = inc vars in
      Let (k_id, Get (clos, 0), Let (env_id, Get (clos, 1), Apply_indirect (k_id, env_id :: args, [p, fvs_to_list args']))), vars, pointers, Asm.PointerMap.empty
    end
  | Call_direct (k, clos, args, (p, args')) -> begin
      let env_id, vars = inc vars in
      Let (env_id, Get (clos, 1), Apply_direct (k, env_id :: args, [p, fvs_to_list args'])), vars, pointers, Asm.PointerMap.empty
    end

let block_to_asm (block: block) (asm1: Asm.instr) (vars: Asm.var Seq.t) (pointers: Asm.pointer Seq.t): Asm.block * var Seq.t * Asm.pointer Seq.t * Asm.blocks =
  match block with
  | Return (arg, args') -> (arg :: fvs_to_list args', asm1), vars, pointers, Asm.PointerMap.empty
  | Clos (body_free_variables, args') -> begin
      let function_id, pointers = inc pointers in
      let environment_id, vars = inc vars in
      let body = List.fold_left (fun block' (pos, body_free_variable) -> Asm.Let (body_free_variable, Asm.Get (environment_id, pos), block')) (Apply_direct (function_id, args' @ fvs_to_list body_free_variables, [])) (List.mapi (fun i fv -> i, fv) (fvs_to_list body_free_variables)) in
      (environment_id :: args', body), vars, pointers, Asm.PointerMap.singleton function_id (args' @ fvs_to_list body_free_variables, asm1)
    end
  | If_branch (args, fvs) -> (fvs_to_list args @ fvs_to_list fvs, asm1), vars, pointers, Asm.PointerMap.empty
  | If_join (arg, args) -> (arg :: fvs_to_list args, asm1), vars, pointers, Asm.PointerMap.empty
  | Match_branch (body_free_variables, args', fvs) -> begin
      let environment_id, vars = inc vars in
      let body = List.fold_left (fun block' (pos, body_free_variable) -> Asm.Let (body_free_variable, Asm.Get (environment_id, pos), block')) asm1 (List.mapi (fun i fv -> i, fv) body_free_variables) in
      (environment_id :: fvs_to_list args' @ fvs_to_list fvs, body), vars, pointers, Asm.PointerMap.empty
    end
  | Match_join (arg, args) -> (arg :: fvs_to_list args, asm1), vars, pointers, Asm.PointerMap.empty

let blocks_to_asm (blocks: blocks) (vars: Asm.var Seq.t) (pointers: Asm.pointer Seq.t): Asm.blocks * Asm.var Seq.t * Asm.pointer Seq.t =
  PointerMap.fold (fun k (block, instr) (blocks, vars, pointers) -> begin
    let asm, vars, pointers, blocks' = instr_to_asm instr vars pointers in  
    let block, vars, pointers, blocks'' = block_to_asm block asm vars pointers in
    Asm.PointerMap.add k block (Asm.PointerMap.union (fun _ -> assert false) blocks (Asm.PointerMap.union (fun _ -> assert false) blocks' blocks'')), vars, pointers
  end) blocks (Asm.PointerMap.empty, vars, pointers)

let size_expr (expr : expr): int =
  match expr with
  | Int _ -> 1
  | Add (_, _) -> 2
  | Sub (_, _) -> 2
  | Var _ -> 1
  | Tuple args -> List.length args
  | Get (_, _) -> 2
  | Closure (_, args) -> VarSet.cardinal args
  | Constructor (_, args) -> List.length args

let rec size_instr (cps : instr): int =
  match cps with
  | Let (_, expr, instr) -> 1 + size_expr expr + size_instr instr
  | If (_, (_, argst), (_, argsf), fvs) -> 1 + VarSet.cardinal argst + VarSet.cardinal fvs + 1 + VarSet.cardinal argsf + VarSet.cardinal fvs
  | Match_pattern (_, matchs, (_, argsf), fvs) -> List.fold_left (fun size (_, pld, _, args) -> size + 1 + List.length pld + VarSet.cardinal args + VarSet.cardinal fvs) 0 matchs + 1 + VarSet.cardinal argsf + VarSet.cardinal fvs
  | Return _ -> 1
  | If_return (_, _, args) -> 2 + VarSet.cardinal args
  | Match_return (_, _, args) -> 2 + VarSet.cardinal args
  | Call (_, args, (_, args')) -> 2 + List.length args + 1 + VarSet.cardinal args'
  | Call_direct (_, _, args, (_, args')) -> 2 + List.length args + 1 + VarSet.cardinal args'

let size_block (block: block): int =
  match block with
  | Return (_, args') -> 1 + VarSet.cardinal args'
  | Clos (body_free_variables, args') -> VarSet.cardinal body_free_variables + List.length args'
  | If_branch (args, fvs) -> VarSet.cardinal fvs + VarSet.cardinal args
  | If_join (_, args) -> 1 + VarSet.cardinal args
  | Match_branch (body_free_variables, args', fvs) -> List.length body_free_variables + VarSet.cardinal fvs + VarSet.cardinal args'
  | Match_join (_, args) -> 1 + VarSet.cardinal args

let size_blocks (blocks: blocks): int =
  PointerMap.fold (fun _ (block, instr) size -> size + size_block block + size_instr instr) blocks 0

let count_vars_expr (expr : expr) (vars : int array) (pointers: int array): unit =
  match expr with
  | Int x -> Array.set vars x (Array.get vars x + 1)
  | Add (x1, x2) -> Array.set vars x1 (Array.get vars x1 + 1); Array.set vars x2 (Array.get vars x2 + 1)
  | Sub (x1, x2) -> Array.set vars x1 (Array.get vars x1 + 1); Array.set vars x2 (Array.get vars x2 + 1)
  | Var x -> Array.set vars x (Array.get vars x + 1)
  | Tuple args -> List.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args
  | Get (arg, _) -> Array.set vars arg (Array.get vars arg + 1)
  | Closure (k, args) -> begin
      Array.set pointers k (Array.get pointers k + 1);
      VarSet.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args
    end
  | Constructor (_, args) -> List.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args

let rec count_vars_instr (instr : instr) (vars : int array) (conts: int array): instr =
  match instr with
  | Let (var, expr, instr) -> begin
      let instr' = count_vars_instr instr vars conts in
      if Array.get vars var > 0
      then begin
        count_vars_expr expr vars conts;
        Let (var, expr, instr')
      end
      else instr'
    end
  | Call_direct (k, clos, args, (k', args')) -> begin
      Array.set conts k (Array.get conts k + 1);
      Array.set vars clos (Array.get vars clos + 1);
      List.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args;
      Array.set conts k' (Array.get conts k' + 1);
      VarSet.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args';
      Call_direct (k, clos, args, (k', args'))
    end
  | Call (clos, args, (k', args')) -> begin
    Array.set vars clos (Array.get vars clos + 1);
    List.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args;
    Array.set conts k' (Array.get conts k' + 1);
    VarSet.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args';
    Call (clos, args, (k', args'))
  end
  | If (var, (kt, argst), (kf, argsf), fvs) -> begin
      Array.set vars var (Array.get vars var + 1);
      VarSet.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) fvs;
      Array.set conts kt (Array.get conts kt + 1);
      VarSet.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) argst;
      Array.set conts kf (Array.get conts kf + 1);
      VarSet.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) argsf;
      If (var, (kt, argst), (kf, argsf), fvs)
    end
  | Return x -> begin
      Array.set vars x (Array.get vars x + 1);
      Return x
    end
  | If_return (k, arg, args) -> begin
      Array.set conts k (Array.get conts k + 1);
      Array.set vars arg (Array.get vars arg + 1);
      VarSet.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args;
      If_return (k, arg, args)
    end
  | Match_return (k, arg, args) -> begin
      Array.set conts k (Array.get conts k + 1);
      Array.set vars arg (Array.get vars arg + 1);
      VarSet.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args;
      Match_return (k, arg, args)
    end
  | Match_pattern (var, matchs, (kf, argsf), fvs) -> begin
      Array.set vars var (Array.get vars var + 1);
      VarSet.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) fvs;
      List.iter (fun (_, _, kt, args) -> VarSet.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args; Array.set conts kt (Array.get conts kt + 1)) matchs;
      Array.set conts kf (Array.get conts kf + 1);
      VarSet.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) argsf;
      Match_pattern (var, matchs, (kf, argsf), fvs)
    end

(*
let rec accessible_blocks_expr (expr : expr) (blocks: blocks) (visited: int PointerMap.t): int PointerMap.t =
  match expr with
  | Prim (_, _) -> visited
  | Var _ -> visited
  | Tuple _ -> visited
  | Get (_, _) -> visited
  | Closure (k, _) when PointerMap.mem k blocks -> PointerMap.add k (PointerMap.find k visited + 1) visited
  | Closure (k, _) -> begin
      let (_, instr) = PointerMap.find k blocks in
      accessible_blocks_instr instr blocks (PointerMap.add k 1 visited)
    end
  | Constructor (_, _) -> visited

and accessible_blocks_instr (instr : instr) (blocks: blocks) (visited: int PointerMap.t): int PointerMap.t =
  match instr with
  | Let (_, expr, instr) -> accessible_blocks_instr instr blocks (accessible_blocks_expr expr blocks visited)
  | Call_direct (k, _, _, (_, _)) when PointerMap.mem k blocks -> PointerMap.add k (PointerMap.find k visited + 1) visited
  | Call_direct (k, _, _, (_, _)) -> begin
      let (_, instr) = PointerMap.find k blocks in
      accessible_blocks_instr instr blocks (PointerMap.add k 1 visited)
    end
  | Call (_, _, (_, _)) -> visited
  | If (var, matchs, (kf, argsf), fvs) -> begin
      Array.set vars var (Array.get vars var + 1);
      List.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) fvs;
      List.iter (fun (_, kt, args) -> List.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args; Array.set conts kt (Array.get conts kt + 1)) matchs;
      Array.set conts kf (Array.get conts kf + 1);
      List.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) argsf;
      If (var, matchs, (kf, argsf), fvs)
    end
  | Return x -> begin
      Array.set vars x (Array.get vars x + 1);
      Return x
    end
  | If_return (k, arg, args) -> begin
      Array.set conts k (Array.get conts k + 1);
      Array.set vars arg (Array.get vars arg + 1);
      List.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args;
      If_return (k, arg, args)
    end
  | Match_return (k, arg, args) -> begin
      Array.set conts k (Array.get conts k + 1);
      Array.set vars arg (Array.get vars arg + 1);
      List.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args;
      Match_return (k, arg, args)
    end
  | Apply_block (k, args) -> begin
      Array.set conts k (Array.get conts k + 1);
      List.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args;
      Apply_block (k, args)
    end
  | Match_pattern (var, matchs, (kf, argsf), fvs) -> begin
      Array.set vars var (Array.get vars var + 1);
      List.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) fvs;
      List.iter (fun (_, kt, _, args) -> List.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args; Array.set conts kt (Array.get conts kt + 1)) matchs;
      Array.set conts kf (Array.get conts kf + 1);
      List.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) argsf;
      Match_pattern (var, matchs, (kf, argsf), fvs)
    end
    *)

let count_vars_instr_blocks (blocks : blocks): blocks * var array * pointer array =
  let vars = Array.make 10000 0 in
  let conts = Array.make 10000 0 in
  PointerMap.map (fun (block, instr) -> block, count_vars_instr instr vars conts) blocks, vars, conts

let elim_unused_blocks (blocks : blocks) (conts : int array): blocks = PointerMap.filter (fun k _ -> Array.get conts k > 0) blocks

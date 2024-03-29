open Utils

(* Identifier for variables *)
type var = int

(* Identifier for constructor tags *)
type tag = int

(* Module for variables. *)
module VarMap = Map.Make (Int)

(* Module for tags. *)
module TagMap = Map.Make (Int)

(* Module for generating Cps blocks *)
module Blocks = struct
  type t = Cps.blocks

  let empty = Cps.PointerMap.empty
  let add (pointer: Cps.pointer) (block, expr: Cps.block * Cps.instr) (blocks: t): t =
    Cps.PointerMap.add pointer (block, expr) blocks
  
  exception PointerNotUnique of Cps.pointer * (Cps.block * Cps.instr) * (Cps.block * Cps.instr)
  let join (bs1: t) (bs2: t): t =
    Cps.PointerMap.union (fun p b1 b2 -> raise (PointerNotUnique (p, b1, b2))) bs1 bs2
end

(* Module for manipulation of Cps free variables. *)
module FreeVariables = struct
  type t = Cps.VarSet.t
  let empty = Cps.VarSet.empty
  let add fv (fvs: t): t =
    Cps.VarSet.add fv fvs
  let union (fvs1: t) (fvs2: t): t =
    Cps.VarSet.union fvs1 fvs2
  let fold (l: t list): t =
    List.fold_left (fun fvs fv -> union fvs fv) Cps.VarSet.empty l
  let singleton (fv: Cps.var): t =
    Cps.VarSet.singleton fv
  let remove (fvs: t) (fv: Cps.var): t =
    Cps.VarSet.remove fv fvs
  let remove_from_list (fvs: t) (l: Cps.var list): t =
    List.fold_left (fun fvs fv -> Cps.VarSet.remove fv fvs) fvs l
  let from_list (l: Cps.var list): t =
    List.fold_left (fun fvs fv -> Cps.VarSet.add fv fvs) Cps.VarSet.empty l
end

(* Binary operators *)
type binary_operator =
| Add (* + *)
| Sub (* - *)

(* Expressions *)
type expr =
| Var of var (* Variable *)
| Fun of var list * expr (* Abstraction *)
| App of expr * expr (* Application *)
| Tuple of expr list (* Tuple of expressions *)
| Let of var * expr * expr (* Let *)
| Let_tuple of var list * expr * expr (* Let unpack tuple *)
| Let_rec of (var * expr) list * expr (* Let rec of functions *)
| Int of int (* Integer *)
| Binary of binary_operator * expr * expr (* Binary operation *)
| If of expr * expr * expr (* If cond then else *)
| Constructor of tag * expr list (* Constructor application *)
| Match of expr * (tag * var list * expr) list * expr (* Pattern matching *)
| Get of expr * int (* Read tuple field *)

(* Pretty printer for binary operators. *)
let pp_binary_operator (fmt: Format.formatter) (bop: binary_operator): unit =
  match bop with
  | Add -> Format.fprintf fmt "+"
  | Sub -> Format.fprintf fmt "-"

(* Pretty printer for expressions. *)
let rec pp_expr (subs: string VarMap.t) (fmt: Format.formatter) (expr: expr): unit =
  let pp_expr = pp_expr subs in
  match expr with
  | Int i -> Format.fprintf fmt "%d%!" i
  | Binary (op, a, b) -> Format.fprintf fmt "%a %a %a%!" pp_expr a pp_binary_operator op pp_expr b
  | Fun ([], e) -> Format.fprintf fmt "(fun () -> %a)%!" pp_expr e
  | Fun ([arg], e) -> Format.fprintf fmt "(fun %s -> %a)%!" (string_of_sub arg subs) pp_expr e
  | Fun (arg :: args, e) -> begin
      Format.fprintf fmt "(fun %s" (string_of_sub arg subs);
      List.iter (fun arg' -> Format.fprintf fmt " %s" (string_of_sub arg' subs)) args;
      Format.fprintf fmt "-> %a)%!" pp_expr e
    end
  | Var x -> Format.fprintf fmt "%s%!" (string_of_sub x subs)
  | Let (var, e1, e2) -> Format.fprintf fmt "let %s = %a in \n\n%a%!" (string_of_sub var subs) pp_expr e1 pp_expr e2
  | Let_tuple ([], e1, e2) -> Format.fprintf fmt "let () = %a in\n\n%a%!" pp_expr e1 pp_expr e2
  | Let_tuple ([var], e1, e2) -> Format.fprintf fmt "let %s = %a in\n\n%a%!" (string_of_sub var subs) pp_expr e1 pp_expr e2
  | Let_tuple (var :: vars, e1, e2) -> Format.fprintf fmt "let %s" (string_of_sub var subs); List.iter (fun var -> Format.fprintf fmt ", %s" (string_of_sub var subs)) vars; Format.fprintf fmt " = %a in\n\n%a%!"  pp_expr e1 pp_expr e2
  | Let_rec (bindings, expr) -> begin
      Format.fprintf fmt "let rec"; begin
        match bindings with
        | [] -> Format.fprintf fmt "\n"
        | [var, e] -> Format.fprintf fmt " %s = %a" (string_of_sub var subs) pp_expr e
        | (var, e) :: bindings' -> begin
            Format.fprintf fmt " %s = %a" (string_of_sub var subs) pp_expr e;
            List.iter (fun (var, expr) -> Format.fprintf fmt "\nand %s = %a" (string_of_sub var subs) pp_expr expr) bindings'
          end;
      end;
      Format.fprintf fmt " in\n\n%a%!" pp_expr expr
    end
  | If (cond, t, f) -> Format.fprintf fmt "(if %a = 0 then %a else %a)%!" pp_expr cond pp_expr t pp_expr f
  | App (e1, e2) -> Format.fprintf fmt "(%a %a)" pp_expr e1 pp_expr e2
  | Constructor (name, []) -> Format.fprintf fmt "%d" name
  | Constructor (name, [ e ]) -> Format.fprintf fmt "%d %a" name pp_expr e
  | Constructor (name, e :: exprs') -> Format.fprintf fmt "%d (%a" name pp_expr e; List.iter (fun e' -> Format.fprintf fmt ", %a" pp_expr e') exprs'; Format.fprintf fmt ")"
  | Match (e, matchs, default) -> begin
      Format.fprintf fmt "(match %a with" pp_expr e;
      List.iter (fun (t, args, e) -> begin
        match args with
        | [] -> Format.fprintf fmt "\n%d -> %a" t pp_expr e
        | [var] -> Format.fprintf fmt "\n%d %s -> %a" t (string_of_sub var subs) pp_expr e
        | var :: vars -> begin
            Format.fprintf fmt "\n%d (%s" t (string_of_sub var subs);
            List.iter (fun var' -> Format.fprintf fmt ", %s" (string_of_sub var' subs)) vars;
            Format.fprintf fmt ") -> %a" pp_expr e
          end
      end) matchs;
      Format.fprintf fmt "\n_ -> %a)%!" pp_expr default
    end
  | Tuple [] -> Format.fprintf fmt "()"
  | Tuple [expr] -> Format.fprintf fmt "%a" pp_expr expr
  | Tuple (expr :: exprs) -> Format.fprintf fmt "(%a" pp_expr expr; List.iter (fun e' -> Format.fprintf fmt ", %a" pp_expr e') exprs; Format.fprintf fmt ")"
  | Get (e, i) -> Format.fprintf fmt "%a.%d%!" pp_expr e i

(* Converts binary operators with arguments to a Cps expression. *)
let binary_operator_to_cps (binary: binary_operator) (x1: Cps.var) (x2: Cps.var): Cps.expr =
  match binary with
  | Add -> Add (x1, x2)
  | Sub -> Sub (x1, x2)

(* *)
let rec expr_to_cps (vars: Cps.var Seq.t) (pointers: Cps.pointer Seq.t) (fvs: Cps.VarSet.t) (ast : expr) var (expr : Cps.instr): Cps.instr * Cps.var Seq.t * Cps.pointer Seq.t * Cps.VarSet.t * Cps.blocks =
  match ast with
  | Int i -> Let (var, Int i, expr), vars, pointers, FreeVariables.empty, Blocks.empty
  | Binary (binary_operator, e1, e2) -> begin
      let e1_id, vars = inc vars in
      let e2_id, vars = inc vars in

      let expr: Cps.instr = Let (var, binary_operator_to_cps binary_operator e1_id e2_id, expr) in
      let expr', vars, pointers, fv', conts' = expr_to_cps vars pointers (FreeVariables.add e1_id fvs) e2 e2_id expr in
      let expr, vars, pointers, fvs', conts = expr_to_cps vars pointers (FreeVariables.union fv' fvs) e1 e1_id expr' in
      expr, vars, pointers, FreeVariables.union fvs' fv', Blocks.join conts conts'
    end
  (*
      let closure_environment_id = Environment body_free_variables in
      let var = Closure (closure_id, closure_environment_id) in
      expr
    and closure_id environment_id body_argument_id =
      let body_free_variable_1 = Get (environment_id, 0) in
      ...
      let body_free_variable_n = Get (environment_id, n - 1) in
      function_id body_argument_id body_free_variable_1 ... body_free_variable_n
    and function_id body_argument_id body_free_variable_1 ... body_free_variable_n =
      body_cps
  *)
  | Fun (argument_name, body) -> begin
      let body_return_id, vars = inc vars in
      let body_cps, vars, pointers, body_free_variables, body_continuations = expr_to_cps vars pointers FreeVariables.empty body body_return_id (Return body_return_id) in
      let body_free_variables = Cps.VarSet.filter (fun body_free_variable -> not (List.mem body_free_variable argument_name)) body_free_variables in
      let closure_id, pointers = inc pointers in
      Let (var, Closure (closure_id, body_free_variables), expr), vars, pointers, body_free_variables, Blocks.add closure_id (Clos (body_free_variables, argument_name), body_cps) body_continuations
    end
  (*
      let var = variable_name in
      expr
  *)
  | Var variable_name -> Let (var, Var variable_name, expr), vars, pointers, FreeVariables.singleton variable_name, Blocks.empty
  | Let (var', e1, e2) -> begin
      let cps1, vars, pointers, fv1, conts1 = expr_to_cps vars pointers fvs e2 var expr in
      let cps2, vars, pointers, fv2, conts2 = expr_to_cps vars pointers (FreeVariables.remove (FreeVariables.union fv1 fvs) var') e1 var' cps1 in
      cps2, vars, pointers, FreeVariables.union fv2 (FreeVariables.remove fv1 var'), Blocks.join conts1 conts2
    end
  | Let_tuple (vars', e1, e2) -> begin
      let tuple_id, vars = inc vars in
      let cps1, vars, pointers, fv1, conts1 = expr_to_cps vars pointers fvs e2 var expr in
      let cps2 = List.fold_left (fun cps (i, var) ->  Cps.Let (var, Cps.Get (tuple_id, i), cps)) cps1 (List.mapi (fun i v -> i, v) vars') in
      let cps3, vars, pointers, fv2, conts2 = expr_to_cps vars pointers (FreeVariables.remove_from_list (FreeVariables.union fv1 fvs) vars') e1 tuple_id cps2 in
      cps3, vars, pointers, FreeVariables.union fv2 (FreeVariables.remove_from_list fv1 vars'), Blocks.join conts1 conts2
    end
    (*
        let env = fvs_1 ∪ ... ∪ fvs_n
        let var1 = Closure (f1, env) in
        ...
        let varn = Closure (fn, env) in
        e2
      and f1 env arg1 =
        let var1_1 = Closure (f1, env) in
        ...
        let varn_1 = Closure (fn, env) in
        let fv1_1 = Get_env (env, fv1_1_index) in
        ...
        let fvm_1 = Get_env (env, fvm_1_index) in
        f1_impl arg1 var1_1 ... varn_1 fv1_1 ... fvn_1
      and f1_impl arg1 var1_1 ... varn_1 fv1_1 ... fvn_1 =
        expr1
      ...
      and fn env argn =
        let var1_n = Closure (f1, env) in
        ...
        let varn_n = Closure (fn, env) in
        let fv1_n = Get_env (env, fv1_n_index) in
        ...
        let fvm_n = Get_env (env, fvm_n_index) in
        f1_impl argn var1_n ... varn_n fv1_n ... fvm_n
      and fn_impl argn var1_n ... varn_n fv1_n ... fvm_n =
        exprn
  *)
  | Let_rec (bindings, scope) ->
    let scope_cps, vars, pointers, scope_free_variables, scope_conts = expr_to_cps vars pointers fvs scope var expr in
    
    (* Substitued binding variables in scope. *)
    let scope_binding_variable_ids = List.map (fun (var', _) -> var') bindings in

    (* Scope free variables without whose who are in bindings. *)
    let scope_free_variables_no_bindings = List.fold_left (fun fvs' fv -> FreeVariables.remove fvs' fv) scope_free_variables scope_binding_variable_ids in
    
    (*  *)
    let (vars, pointers, scope_and_closures_conts), closures = List.fold_left_map (fun (vars, pointers, scope_conts') (_, binding_expr) -> begin
      match binding_expr with
      | Fun (arg, binding_body_expr) ->
          let return_variable, vars = inc vars in
          let binding_body_cps, vars, pointers, binding_body_free_variables, binding_body_conts = expr_to_cps vars pointers FreeVariables.empty binding_body_expr return_variable (Return return_variable) in
          (vars, pointers, Blocks.join binding_body_conts scope_conts'), (arg, binding_body_cps, binding_body_free_variables)
      | _ -> assert false
    end) (vars, pointers, scope_conts) bindings in

    let (vars, pointers), closures2 = List.fold_left_map (fun (vars, pointers) (scope_binding_variable_id, (arg, binding_body_cps, binding_body_free_variables)) -> begin
      (* Substitued binding variables in body. *)
      let bindind_body_bindind_variable_ids = List.map (fun (binding_name, _) -> binding_name) bindings in
      
      let clos_id, pointers = inc pointers in

      (* Body free variables without whose who are in bindings. *)
      let binding_body_free_variables_no_arg_no_bindings = (Cps.VarSet.filter (fun binding_body_free_variable -> not (List.mem binding_body_free_variable arg) && not (List.mem binding_body_free_variable bindind_body_bindind_variable_ids)) binding_body_free_variables) in
      (vars, pointers), (scope_binding_variable_id, clos_id, binding_body_cps, bindind_body_bindind_variable_ids, arg, binding_body_free_variables_no_arg_no_bindings)
    end) (vars, pointers) (List.combine scope_binding_variable_ids closures) in

    (* Free variable ids (caller). *)
    let closures_caller_free_variable_ids = List.map (fun (_, _, _, _, _, binding_body_free_variables_no_arg_no_bindings) -> binding_body_free_variables_no_arg_no_bindings) closures2 in

    let all_binding_bodies_free_variables = FreeVariables.fold closures_caller_free_variable_ids in

    let vars, pointers, scope_cps, _scope_free_variables_no_bindings'', scope_and_closures_conts = List.fold_left (fun (vars, pointers, scope_cps', _scope_free_variables_no_bindings', scope_and_closures_conts') ((scope_binding_variable_id, binding_body_closure_continuation_id, binding_body_cps, bindind_body_bindind_variable_ids, binding_body_arg_id, binding_body_free_variables_no_arg_no_bindings), _caller_free_variable_ids) ->
                
      (* *)
      let bindind_body_bindind_closures_ids = List.map2 (fun bindind_body_bindind_variable_id (_, binding_body_binding_closure_continuation, _, _, _, _) -> (bindind_body_bindind_variable_id, binding_body_binding_closure_continuation)) bindind_body_bindind_variable_ids closures2 in
      
      (* TODO MUST FIX closure_continuation_id -> need Closure_rec *)
      (* *)
      let binding_body_with_free_and_binding_variables = List.fold_left (fun binding_body_with_free_variables' (bindind_body_bindind_variable_id, bindind_body_bindind_closures_id) -> Cps.Let (bindind_body_bindind_variable_id, Closure (bindind_body_bindind_closures_id, all_binding_bodies_free_variables), binding_body_with_free_variables')) binding_body_cps bindind_body_bindind_closures_ids in

      (* *)
      vars, pointers, Cps.Let (scope_binding_variable_id, Closure (binding_body_closure_continuation_id, all_binding_bodies_free_variables), scope_cps'), (FreeVariables.remove binding_body_free_variables_no_arg_no_bindings var), Blocks.add binding_body_closure_continuation_id (Clos (all_binding_bodies_free_variables, binding_body_arg_id), binding_body_with_free_and_binding_variables) scope_and_closures_conts'
    
    ) (vars, pointers, scope_cps, scope_free_variables_no_bindings, scope_and_closures_conts) (List.combine closures2 closures_caller_free_variable_ids) in
    scope_cps, vars, pointers, FreeVariables.union all_binding_bodies_free_variables scope_free_variables_no_bindings, scope_and_closures_conts
    (*
        let e1_id = e1 in
        if e1_id then e2_kid fv1_2 ... fvn_2 else e3_kid fv1_3 ... fvm_3
      and e2_kid fv1_2 ... fvn_2 =
        let e2_id = e2 in
        merge_kid e2_id fv1 ... fvi
      and e3_kid fv1_3 ... fvm_3 =
        let e3_id = e3 in
        merge_kid e3_id fv1 ... fvi
      and merge_kid var fv1 ... fvi =
        expr
    *)
  | If (e1, e2, e3) -> begin
      (* Varialbes that will hold expression result. *)
      let e1_id, vars = inc vars in
      let e2_id, vars = inc vars in
      let e3_id, vars = inc vars in

      (* Continuations IDs. *)
      let e2_kid, pointers = inc pointers in
      let e3_kid, pointers = inc pointers in
      let merge_kid, pointers = inc pointers in

      let true_cps, vars, pointers, true_free_variables_id, true_continuations = expr_to_cps vars pointers fvs e2 e2_id (If_return (merge_kid, e2_id, fvs)) in
      let false_cps, vars, pointers, false_free_variables_id, false_continuations = expr_to_cps vars pointers fvs e3 e3_id (If_return (merge_kid, e3_id, fvs)) in
      
      let cps1, vars, pointers, fv1, conts1 = expr_to_cps vars pointers (FreeVariables.union fvs (FreeVariables.union true_free_variables_id false_free_variables_id)) e1 e1_id (If (e1_id, (e2_kid, true_free_variables_id), (e3_kid, false_free_variables_id), fvs)) in

      cps1, vars, pointers, FreeVariables.union fv1 (FreeVariables.union true_free_variables_id false_free_variables_id), Blocks.add merge_kid (If_join (var, fvs), expr) (Blocks.add e3_kid (If_branch (false_free_variables_id, fvs), false_cps) (Blocks.add e2_kid (If_branch (true_free_variables_id, fvs), true_cps) (Blocks.join true_continuations (Blocks.join false_continuations conts1))))
    end
    (*
          let closure_id = e1 in
          let argument_id = e2 in
          let closure_continuation_id = Get (closure_id, 0) in
          let closure_environment_id = Get (closure_id, 1) in
          return_kid (closure_continuation_id closure_environment_id argument_id) fv_1 ... fv_n
        let return_kid var fv_1 ... fv_n =
          expr
    *)
  | App (e1, e2) -> begin
      (* Variables that will hold expression result. *)
      let e1_id, vars = inc vars in
      let e2_id, vars = inc vars in

      (* Continuations IDs. *)
      let return_kid, pointers = inc pointers in

      let cps1, vars, pointers, fv1, conts1 = expr_to_cps vars pointers (FreeVariables.add e1_id fvs) e2 e2_id (Call (e1_id, [e2_id], (return_kid, fvs))) in
      let cps2, vars, pointers, fv2, conts2 = expr_to_cps vars pointers (FreeVariables.union fv1 fvs) e1 e1_id cps1 in
      cps2, vars, pointers, FreeVariables.union fv2 fv1, Blocks.add return_kid (Return (var, fvs), expr) (Blocks.join conts2 conts1)
    end
  (*

   *)
  | Match (pattern_expr, branchs, default_branch_expr) -> begin
      (* Return continuation after matching. *)
      let return_continuation_id, pointers = inc pointers in

      (* Default branch cps generation. *)
      let default_branch_return_id, vars = inc vars in
      let default_branch_cps, vars, pointers, default_branch_free_variables, default_branch_continuations = expr_to_cps vars pointers fvs default_branch_expr default_branch_return_id (Match_return (return_continuation_id, default_branch_return_id, fvs)) in
      
      (* Default branch continuation. *)
      let default_continuation_id, pointers = inc pointers in

      (* Branchs continuations and bodies informations. *)
      let (vars, pointers, branchs_continuations), branchs_bodies = List.fold_left_map (fun (vars, pointers, default_continuation') (branch_index, branch_arguments_names, branch_expr) -> begin
        (* Branch cps generation. *)
        let branch_return_id, vars = inc vars in
        let branch_cps, vars, pointers, branch_free_variables, branch_continuations = expr_to_cps vars pointers fvs branch_expr branch_return_id (Match_return (return_continuation_id, branch_return_id, fvs)) in
                
        (* Branch free variables that are not passed in arguments. *)
        let branch_free_variables = Cps.VarSet.filter (fun branch_free_variable -> not (List.mem branch_free_variable branch_arguments_names)) branch_free_variables in
        
        (* Branch cps reading arguments from payload. *)
        let branch_payload_id, vars = inc vars in
        let _branch_cps_with_payload = List.fold_left (fun branch_cps' (branch_argument_payload_index, branch_argument_id) -> Cps.Let (branch_argument_id, Get (branch_payload_id, branch_argument_payload_index), branch_cps')) branch_cps (List.mapi (fun i v -> i, v) branch_arguments_names) in
        
        (* Branch continuation and body informations. *)
        let branch_continuation_id, pointers = inc pointers in
        (vars, pointers, Blocks.add branch_continuation_id (Match_branch (branch_arguments_names, branch_free_variables, fvs), branch_cps) (Blocks.join branch_continuations default_continuation')), (branch_index, branch_continuation_id, branch_free_variables)
      end) (vars, pointers, Blocks.empty) branchs in
      
      (* Substitued free variables. *)
      let free_variables_branchs = List.map (fun (_, _, fv2) -> fv2) branchs_bodies in

      (* Pattern matching. *)
      let pattern_id, vars = inc vars in
      let expr'', vars, pointers, fvs', conts = expr_to_cps vars pointers (FreeVariables.union fvs (FreeVariables.union default_branch_free_variables (FreeVariables.fold free_variables_branchs))) pattern_expr pattern_id (Match_pattern (pattern_id, List.map (fun ((_, branch_arguments_names, _), ((n, k, _), fvs')) -> n, branch_arguments_names, k, fvs') (List.combine branchs (List.combine branchs_bodies free_variables_branchs)), (default_continuation_id, default_branch_free_variables), fvs)) in

      expr'', vars, pointers, FreeVariables.union fvs' (FreeVariables.union default_branch_free_variables (FreeVariables.fold free_variables_branchs)), Blocks.add default_continuation_id (Match_branch ([], default_branch_free_variables, fvs), default_branch_cps) (Blocks.add return_continuation_id (Match_join (var, fvs), expr) (Blocks.join conts (Blocks.join default_branch_continuations branchs_continuations)))
    end
    (*
    
    *)
  | Tuple args -> begin
      let vars, args_ids = List.fold_left_map (fun vars arg -> begin
        let var_id, vars = inc vars in
        vars, (var_id, arg)
      end) vars args in
      List.fold_left (fun (expr', vars, pointers, fv', conts') (var, e) -> begin
        let expr'', vars, pointers, fv'', conts'' = expr_to_cps vars pointers (FreeVariables.union (FreeVariables.remove fv' var) fvs) e var expr' in
        expr'', vars, pointers, FreeVariables.union fv'' (FreeVariables.remove fv' var), Blocks.join conts' conts''
      end) (Let (var, Tuple (List.map (fun (var, _) -> var) args_ids), expr), vars, pointers, FreeVariables.from_list (List.map (fun (var, _) -> var) args_ids), Blocks.empty) args_ids
    end
    (*
    
    *)
  | Constructor (tag, args) -> begin
      let vars, args_ids = List.fold_left_map (fun vars arg -> begin
        let var_id, vars = inc vars in
        vars, (var_id, arg)
      end) vars args in
      List.fold_left (fun (expr', vars, pointers, fv', conts') (var, e) -> begin
        let expr'', vars, pointers, fv'', conts'' = expr_to_cps vars pointers (FreeVariables.union (FreeVariables.remove fv' var) fvs) e var expr' in
        expr'', vars, pointers, FreeVariables.union fv'' (FreeVariables.remove fv' var), Blocks.join conts' conts''
      end) (Let (var, Constructor (tag, List.map (fun (var, _) -> var) args_ids), expr), vars, pointers, FreeVariables.from_list (List.map (fun (var, _) -> var) args_ids), Blocks.empty) args_ids
    end
  | Get (e, i) -> begin
      let e_id, vars = inc vars in
      expr_to_cps vars pointers fvs e e_id (Let (var, Get (e_id, i), expr))
    end

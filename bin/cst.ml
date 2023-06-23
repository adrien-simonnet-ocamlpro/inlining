type var = int

type binary_operator =
| Add
| Sub

type expr =
| Int of int
| Binary of binary_operator * expr * expr
| Var of var
| Let of var * expr * expr
| Let_rec of (var * expr) list * expr
| Fun of var * expr
| App of expr * expr
| If of expr * expr * expr
| Match of expr * (int * var list * expr) list * expr
| Tuple of expr list
| Constructor of int * expr list

module VarMap = Map.Make (Int)

let gen_name (var: var) (subs: string VarMap.t): string =
  match VarMap.find_opt var subs with
  | Some str -> str ^ "_" ^ (string_of_int var)
  | None -> "_" ^ (string_of_int var)

let pp_binary fmt operator =
  match operator with
  | Add -> Format.fprintf fmt "+"
  | Sub -> Format.fprintf fmt "-"

let rec pp_expr subs fmt expr =
  let pp_expr = pp_expr subs in
  match expr with
  | Int i -> Format.fprintf fmt "%d%!" i
  | Binary (op, a, b) -> Format.fprintf fmt "%a %a %a%!" pp_expr a pp_binary op pp_expr b
  | Fun (x, e) -> Format.fprintf fmt "(fun %s -> %a)%!" (gen_name x subs) pp_expr e
  | Var x -> Format.fprintf fmt "%s%!" (gen_name x subs)
  | Let (var, e1, e2) -> Format.fprintf fmt "(let %s = %a in %a)%!" (gen_name var subs) pp_expr e1 pp_expr e2
  | Let_rec (_bindings, expr) -> Format.fprintf fmt "(let rec %s in %a)%!" (List.fold_left (fun str (var, e) -> str ^ Format.asprintf "%s = %a" (gen_name var subs) pp_expr e) "" _bindings) pp_expr expr
  | If (cond, t, f) -> Format.fprintf fmt "(if %a = 0 then %a else %a)%!" pp_expr cond pp_expr t pp_expr f
  | App (e1, e2) -> Format.fprintf fmt "(%a %a)" pp_expr e1 pp_expr e2
  | Constructor (_, _) -> Format.fprintf fmt "constructor%!"
  | Match (_, _, _) -> Format.fprintf fmt "match%!"
  | Tuple _ -> Format.fprintf fmt "tuple%!"

let conts = ref 0
let inc_conts () =
  conts := !conts + 1;
  !conts
;;

let remove_var fvs var = List.filter (fun fv -> not (fv = var)) fvs

module FreeVars = Set.Make (Int)

let join_fv fv1 fv2 = FreeVars.elements (FreeVars.union (FreeVars.of_list fv1) (FreeVars.of_list fv2))
let join_fvs fvs = List.fold_left (fun fvs fv -> join_fv fvs fv) [] fvs

exception Failure of string

let rec find x lst =
    match lst with
    | [] -> raise (Failure "Not Found")
    | h :: t -> if x = h then 0 else 1 + find x t
;;

let add_var env var va = (var, va) :: env
let has_var_name = Env.has_var
let get_var_name = Env.get_var

let has_var_id = Env.has_value
let get_var_id env var =
  match List.find_opt (fun (v, _) -> var = v) env with
  | Some (_, v) -> v
  | None -> assert false

let binary_operator_to_prim (binary: binary_operator): Cps.prim =
  match binary with
  | Add -> Add
  | Sub -> Sub

let inc (vars: Cps.var Seq.t): Cps.var * Cps.var Seq.t =
  match Seq.uncons vars with
  | None -> assert false
  | Some (i, vars') -> i, vars'

let rec to_cps (vars: Cps.var Seq.t) conts fv0 (ast : expr) var (expr : Cps.expr) (substitutions : (var * int) list) : Cps.expr * Cps.var Seq.t * int list * Cps.cont =
  match ast with
  | Int i -> Let (var, Prim (Const i, []), expr), vars, [], conts
  | Binary (binary_operator, e1, e2) -> begin
      let e1_id, vars = inc vars in
      let e2_id, vars = inc vars in

      let expr: Cps.expr = Let (var, Prim (binary_operator_to_prim binary_operator, [e1_id; e2_id]), expr) in
      let expr', vars, fv', conts' = to_cps vars conts (e1_id :: fv0) e2 e2_id expr substitutions in
      let expr, vars, fvs, conts = to_cps vars conts' (fv' @ fv0) e1 e1_id expr' substitutions in
      expr, vars, (fvs @ fv'), conts
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
      let body_cps, vars, body_free_variables, body_continuations = to_cps vars conts [] body body_return_id (Return body_return_id) [] in
      let body_free_variables = List.filter (fun body_free_variable -> body_free_variable <> argument_name) body_free_variables in
      let function_id = inc_conts () in
      let closure_id = inc_conts () in
      Let (var, Closure (closure_id, body_free_variables), expr), vars, body_free_variables, (Let_clos (closure_id, body_free_variables, [argument_name], Apply_cont (function_id, argument_name :: body_free_variables), Let_cont (function_id, argument_name :: body_free_variables, body_cps, body_continuations)))
    end
  (*
      let var = variable_name in
      expr
  *)
  | Var variable_name -> begin
      if has_var_name substitutions variable_name
      then Let (var, Var (get_var_id substitutions variable_name), expr), vars, [], conts
      else begin
        Let (var, Var variable_name, expr), vars, [variable_name], conts
      end
    end
  | Let (var', e1, e2) -> begin
      let cps1, vars, fv1, conts1 = to_cps vars conts fv0 e2 var expr substitutions in
      let cps2, vars, fv2, conts2 = to_cps vars conts1 (remove_var (fv1 @ fv0) var') e1 var' cps1 (List.filter (fun (_, v) -> not (v = var')) substitutions) in
      cps2, vars, (fv2 @ (remove_var fv1 var')), conts2
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
    let scope_cps, vars, scope_free_variables, scope_conts = to_cps vars conts fv0 scope var expr substitutions in

    (*let scope_substitutions = scope_substitutions @ substitutions in*)
    
    (* Substitued binding variables in scope. *)
    let scope_binding_variable_ids = List.map (fun (var', _) -> var') bindings in

    (* Scope free variables without whose who are in bindings. *)
    let scope_free_variables_no_bindings = List.fold_left (fun fvs fv -> remove_var fvs fv) scope_free_variables scope_binding_variable_ids in
    
    (*  *)
    let (vars, scope_and_closures_conts), closures = List.fold_left_map (fun (vars, scope_conts') (_, binding_expr) -> begin
      match binding_expr with
      | Fun (arg, binding_body_expr) ->
          let return_variable, vars = inc vars in
          let binding_body_cps, vars, binding_body_free_variables, binding_body_conts = to_cps vars scope_conts' [] binding_body_expr return_variable (Return return_variable) [] in
          (vars, binding_body_conts), (arg, binding_body_cps, binding_body_free_variables)
      | _ -> assert false
    end) (vars, scope_conts) bindings in

    let vars, closures2 = List.fold_left_map (fun vars (scope_binding_variable_id, (arg, binding_body_cps, binding_body_free_variables)) ->
      (* Substitued binding variables in body. *)
      let bindind_body_bindind_variable_ids = List.map (fun (binding_name, _) -> binding_name) bindings in
      
      (* Body free variables without whose who are in bindings. *)
      let binding_body_free_variables_no_arg_no_bindings = (List.filter (fun binding_body_free_variable -> not (binding_body_free_variable = arg) && not (List.mem binding_body_free_variable bindind_body_bindind_variable_ids)) binding_body_free_variables) in
      vars, (scope_binding_variable_id, inc_conts (), binding_body_cps, bindind_body_bindind_variable_ids, arg, binding_body_free_variables_no_arg_no_bindings)) vars (List.combine scope_binding_variable_ids closures) in

    (* Free variable ids (caller). *)
    let closures_caller_free_variable_ids = List.map (fun (_, _, _, _, _, binding_body_free_variables_no_arg_no_bindings) -> binding_body_free_variables_no_arg_no_bindings) closures2 in

    let all_binding_bodies_free_variables = join_fvs closures_caller_free_variable_ids in

    let vars, scope_cps, _scope_free_variables_no_bindings'', scope_and_closures_conts = List.fold_left (fun (vars, scope_cps', _scope_free_variables_no_bindings', scope_and_closures_conts') ((scope_binding_variable_id, binding_body_closure_continuation_id, binding_body_cps, bindind_body_bindind_variable_ids, binding_body_arg_id, binding_body_free_variables_no_arg_no_bindings), caller_free_variable_ids) ->
      (* *)
      let binding_body_function_continuation_id = inc_conts () in
      
      (* *)
      let local_environment_id, vars = inc vars in
      
      (* *)
      let _binding_body_with_free_variables = List.fold_left (fun binding_body_function_continuation binding_body_free_variable_no_arg_no_bindings -> Cps.Let (binding_body_free_variable_no_arg_no_bindings, Cps.Get (local_environment_id, (find binding_body_free_variable_no_arg_no_bindings all_binding_bodies_free_variables)), binding_body_function_continuation)) (Apply_cont (binding_body_function_continuation_id, bindind_body_bindind_variable_ids @ (binding_body_arg_id :: caller_free_variable_ids))) caller_free_variable_ids in
    
      (* *)
      let bindind_body_bindind_closures_ids = List.map2 (fun bindind_body_bindind_variable_id (_, binding_body_binding_closure_continuation, _, _, _, _) -> (bindind_body_bindind_variable_id, binding_body_binding_closure_continuation)) bindind_body_bindind_variable_ids closures2 in
      
      (* TODO MUST FIX closure_continuation_id -> need Closure_rec *)
      (* *)
      let binding_body_with_free_and_binding_variables = List.fold_left (fun binding_body_with_free_variables' (bindind_body_bindind_variable_id, bindind_body_bindind_closures_id) -> Cps.Let (bindind_body_bindind_variable_id, Cps.Closure (bindind_body_bindind_closures_id, all_binding_bodies_free_variables), binding_body_with_free_variables')) (Apply_cont (binding_body_function_continuation_id, bindind_body_bindind_variable_ids @ (binding_body_arg_id :: all_binding_bodies_free_variables))) bindind_body_bindind_closures_ids in

      (* *)
      vars, Cps.Let (scope_binding_variable_id, Closure (binding_body_closure_continuation_id, all_binding_bodies_free_variables), scope_cps'), (remove_var (binding_body_free_variables_no_arg_no_bindings) var), (Cps.Let_clos (binding_body_closure_continuation_id, all_binding_bodies_free_variables, [binding_body_arg_id], binding_body_with_free_and_binding_variables, Let_cont (binding_body_function_continuation_id, bindind_body_bindind_variable_ids @ (binding_body_arg_id :: binding_body_free_variables_no_arg_no_bindings), binding_body_cps, scope_and_closures_conts')))
    
    ) (vars, scope_cps, scope_free_variables_no_bindings, scope_and_closures_conts) (List.combine closures2 closures_caller_free_variable_ids) in
    scope_cps, vars, join_fv all_binding_bodies_free_variables scope_free_variables_no_bindings, scope_and_closures_conts
    (*
        let condition_id = condition_expr in
        if condition_id then true_continuation_id fv1_2 ... fvn_2 else false_continuation_id fv1_3 ... fvm_3
      and true_continuation_id fv1_2 ... fvn_2 =
        let true_id = true_expr in
        merge_continuation_id true_id fv1 ... fvi
      and false_continuation_id fv1_3 ... fvm_3 =
        let false_id = false_expr in
        merge_continuation_id false_id fv1 ... fvi
      and merge_continuation_id var fv1 ... fvi =
        expr
    *)
  | If (condition_expr, true_expr, false_expr) -> begin
      let condition_id, vars = inc vars in

      let true_id, vars = inc vars in
      let false_id, vars = inc vars in

      let merge_continuation_id = inc_conts () in
      let true_continuation_id = inc_conts () in
      let false_continuation_id = inc_conts () in
      let true_cps, vars, true_free_variables_id, true_continuations = to_cps vars (Let_cont (merge_continuation_id, var :: fv0, expr, conts)) fv0 true_expr true_id (Apply_cont (merge_continuation_id, true_id :: fv0)) [] in
      let false_cps, vars, false_free_variables_id, true_and_false_continuations = to_cps vars true_continuations fv0 false_expr false_id (Apply_cont (merge_continuation_id, false_id :: fv0)) [] in
      
      let cps1, vars, fv1, conts1 = to_cps vars (Let_cont (true_continuation_id, true_free_variables_id @ fv0, true_cps, Let_cont (false_continuation_id, false_free_variables_id @ fv0, false_cps, true_and_false_continuations))) (join_fv true_free_variables_id false_free_variables_id) condition_expr condition_id (If (condition_id, [(0, false_continuation_id, false_free_variables_id @ fv0)], (true_continuation_id, true_free_variables_id  @ fv0))) substitutions in

      cps1, vars, fv1 @ (join_fv true_free_variables_id false_free_variables_id), conts1
    end
    (*
          let closure_id = closure_expr in
          let argument_id = argument_expr in
          let closure_continuation_id = Get (closure_id, 0) in
          let closure_environment_id = Get (closure_id, 1) in
          return_continuation (closure_continuation_id closure_environment_id argument_id) fv_1 ... fv_n
        let return_continuation var fv_1 ... fv_n =
          expr
    *)
  | App (closure_expr, argument_expr) -> begin
      let return_continuation = inc_conts () in
      let closure_id, vars = inc vars in
      let argument_id, vars = inc vars in
      let cps1, vars, fv1, conts1 = to_cps vars (Let_cont (return_continuation, var :: fv0, expr, conts)) (closure_id :: fv0) argument_expr argument_id (Call (closure_id, [argument_id], (return_continuation, fv0))) substitutions in
      let cps2, vars, fv2, conts2 = to_cps vars conts1 (fv1 @ fv0) closure_expr closure_id cps1 substitutions in
      cps2, vars,fv2 @ fv1, conts2
    end
  (*

   *)
  | Match (pattern_expr, branchs, default_branch_expr) -> begin
      (* Return continuation after matching. *)
      let return_continuation_id = inc_conts () in
      let conts = Cps.Let_cont (return_continuation_id, var :: fv0, expr, conts) in

      (* Default branch cps generation. *)
      let default_branch_return_id, vars = inc vars in
      let default_branch_cps, vars, default_branch_free_variables, default_branch_continuations = to_cps vars conts fv0 default_branch_expr default_branch_return_id (Apply_cont (return_continuation_id, default_branch_return_id :: fv0)) [] in
      
      (* Default branch continuation. *)
      let default_continuation_id = inc_conts () in
      let default_continuation = Cps.Let_cont (default_continuation_id, default_branch_free_variables @ fv0, default_branch_cps, default_branch_continuations) in

      (* Branchs continuations and bodies informations. *)
      let (vars, branchs_continuations), branchs_bodies = List.fold_left_map (fun (vars, default_continuation') (branch_index, branch_arguments_names, branch_expr) -> begin
        (* Branch cps generation. *)
        let branch_return_id, vars = inc vars in
        let branch_cps, vars, branch_free_variables, branch_continuations = to_cps vars default_continuation' fv0 branch_expr branch_return_id (Apply_cont (return_continuation_id, branch_return_id::fv0)) [] in
                
        (* Branch free variables that are not passed in arguments. *)
        let branch_free_variables = List.filter (fun branch_free_variable -> not (List.mem branch_free_variable branch_arguments_names)) branch_free_variables in
        
        (* Branch cps reading arguments from payload. *)
        let branch_payload_id, vars = inc vars in
        let _branch_cps_with_payload = List.fold_left (fun branch_cps' (branch_argument_payload_index, branch_argument_id) -> Cps.Let (branch_argument_id, Get (branch_payload_id, branch_argument_payload_index), branch_cps')) branch_cps (List.mapi (fun i v -> i, v) branch_arguments_names) in
        
        (* Branch continuation and body informations. *)
        let branch_continuation_id = inc_conts () in
        (vars, Let_clos (branch_continuation_id, branch_arguments_names, branch_free_variables @ fv0, branch_cps, branch_continuations)), (branch_index, branch_continuation_id, branch_free_variables)
      end) (vars, default_continuation) branchs in
      
      (* Substitued free variables. *)
      let free_variables_branchs = List.map (fun (_, _, fv2) -> fv2) branchs_bodies in

      (* Pattern matching. *)
      let pattern_id, vars = inc vars in
      let expr, vars, fvs, conts = to_cps vars branchs_continuations (join_fv default_branch_free_variables (join_fvs free_variables_branchs)) pattern_expr pattern_id (Match_pattern (pattern_id, List.map (fun ((n, k, _), fvs) -> n, k, fvs @ fv0) (List.combine branchs_bodies free_variables_branchs), (default_continuation_id, default_branch_free_variables @ fv0))) substitutions in

      expr, vars, fvs @ (join_fv default_branch_free_variables (join_fvs free_variables_branchs)), conts
    end
    (*
    
    *)
  | Tuple args -> begin
     let vars, args_ids = List.fold_left_map (fun vars arg -> begin
      let var_id, vars = inc vars in
      vars, (var_id, arg)
     end) vars args in
     List.fold_left (fun (expr', vars, fv', conts') (var, e) -> let expr'', vars, fv'', conts'' = to_cps vars conts' ((remove_var fv' var) @ fv0) e var expr' substitutions in expr'', vars, fv'' @ (remove_var fv' var), conts'') (Let (var, Tuple (List.map (fun (var, _) -> var) args_ids), expr), vars, (List.map (fun (var, _) -> var) args_ids), conts) args_ids
    end
    (*
    
    *)
  | Constructor (tag, args) -> begin
    let vars, args_ids = List.fold_left_map (fun vars arg -> begin
      let var_id, vars = inc vars in
      vars, (var_id, arg)
     end) vars args in
     List.fold_left (fun (expr', vars, fv', conts') (var, e) -> let expr'', vars, fv'', conts'' = to_cps vars conts' ((remove_var fv' var) @ fv0) e var expr' substitutions in expr'', vars, fv'' @ (remove_var fv' var), conts'') (Let (var, Constructor (tag, List.map (fun (var, _) -> var) args_ids), expr), vars, (List.map (fun (var, _) -> var) args_ids), conts) args_ids
    end

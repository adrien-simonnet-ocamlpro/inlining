type var = int
type tag = int

module VarMap = Map.Make (Int)
module TagMap = Map.Make (Int)

type binary_operator =
| Add
| Sub

type expr =
| Var of var
| Fun of var list * expr
| App of expr * expr
| Let of var * expr * expr
| Let_rec of (var * expr) list * expr
| Int of int
| Binary of binary_operator * expr * expr
| If of expr * expr * expr
| Constructor of tag * expr list
| Match of expr * (tag * var list * expr) list * expr
| Tuple of expr list

let gen_name (var: var) (subs: string VarMap.t): string =
  match VarMap.find_opt var subs with
  | Some str -> str ^ "_" ^ (string_of_int var)
  | None -> "_" ^ (string_of_int var)

let pp_binary fmt operator =
  match operator with
  | Add -> Format.fprintf fmt "+"
  | Sub -> Format.fprintf fmt "-"

let rec pp_vars ?(empty=(" ": string)) ?(split=(" ": string)) (subs: string VarMap.t) (fmt: Format.formatter) (exprs: var list): unit =
  match exprs with
  | [] -> Format.fprintf fmt "%s" empty
  | [ e ] -> Format.fprintf fmt "%s" (gen_name e subs)
  | e :: exprs' -> Format.fprintf fmt "%s%s%a" (gen_name e subs) split (pp_vars ~split ~empty subs) exprs'

let rec pp_expr subs fmt expr =
  let pp_expr = pp_expr subs in
  match expr with
  | Int i -> Format.fprintf fmt "%d%!" i
  | Binary (op, a, b) -> Format.fprintf fmt "%a %a %a%!" pp_expr a pp_binary op pp_expr b
  | Fun (args, e) -> Format.fprintf fmt "(fun %a -> %a)%!" (pp_vars ~empty: "" ~split: " " subs) args pp_expr e
  | Var x -> Format.fprintf fmt "%s%!" (gen_name x subs)
  | Let (var, e1, e2) -> Format.fprintf fmt "(let %s = %a in %a)%!" (gen_name var subs) pp_expr e1 pp_expr e2
  | Let_rec (_bindings, expr) -> Format.fprintf fmt "(let rec %s in %a)%!" (List.fold_left (fun str (var, e) -> str ^ Format.asprintf "%s = %a" (gen_name var subs) pp_expr e) "" _bindings) pp_expr expr
  | If (cond, t, f) -> Format.fprintf fmt "(if %a = 0 then %a else %a)%!" pp_expr cond pp_expr t pp_expr f
  | App (e1, e2) -> Format.fprintf fmt "(%a %a)" pp_expr e1 pp_expr e2
  | Constructor (_, _) -> Format.fprintf fmt "constructor%!"
  | Match (_, _, _) -> Format.fprintf fmt "match%!"
  | Tuple _ -> Format.fprintf fmt "tuple%!"

let binary_operator_to_prim (binary: binary_operator) (x1: Cps.var) (x2: Cps.var): Cps.expr =
  match binary with
  | Add -> Add (x1, x2)
  | Sub -> Sub (x1, x2)

let inc (vars: Cps.var Seq.t): Cps.var * Cps.var Seq.t =
  match Seq.uncons vars with
  | None -> assert false
  | Some (i, vars') -> i, vars'

let empty_blocks = Cps.PointerMap.empty
let add_block (pointer: Cps.pointer) (block, expr: Cps.block * Cps.instr) (blocks: Cps.blocks) = Cps.PointerMap.add pointer (block, expr) blocks
let join_blocks = Cps.PointerMap.union (fun k _ _ -> failwith ("Cst.join_blocks: " ^ (string_of_int k) ^ " not uniques"))

let empty_fvs = Cps.VarSet.empty
let add_fv fv fvs = Cps.VarSet.add fv fvs
let union_fvs fv1 fv2 = Cps.VarSet.union fv1 fv2
let fold_fvs fvs = List.fold_left (fun fvs fv -> union_fvs fvs fv) Cps.VarSet.empty fvs
let fvs_to_list fvs = Cps.VarSet.elements fvs
let singleton_fv fv = Cps.VarSet.singleton fv
let remove_fv fvs fv = Cps.VarSet.remove fv fvs
let fvs_from_list l = List.fold_left (fun fvs fv -> Cps.VarSet.add fv fvs) Cps.VarSet.empty l

let rec to_cps (vars: Cps.var Seq.t) (pointers: Cps.pointer Seq.t) (fvs: Cps.VarSet.t) (ast : expr) var (expr : Cps.instr): Cps.instr * Cps.var Seq.t * Cps.pointer Seq.t * Cps.VarSet.t * Cps.blocks =
  match ast with
  | Int i -> Let (var, Const i, expr), vars, pointers, empty_fvs, empty_blocks
  | Binary (binary_operator, e1, e2) -> begin
      let e1_id, vars = inc vars in
      let e2_id, vars = inc vars in

      let expr: Cps.instr = Let (var, binary_operator_to_prim binary_operator e1_id e2_id, expr) in
      let expr', vars, pointers, fv', conts' = to_cps vars pointers (add_fv e1_id fvs) e2 e2_id expr in
      let expr, vars, pointers, fvs', conts = to_cps vars pointers (union_fvs fv' fvs) e1 e1_id expr' in
      expr, vars, pointers, union_fvs fvs' fv', join_blocks conts conts'
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
      let body_cps, vars, pointers, body_free_variables, body_continuations = to_cps vars pointers empty_fvs body body_return_id (Return body_return_id) in
      let body_free_variables = Cps.VarSet.filter (fun body_free_variable -> not (List.mem body_free_variable argument_name)) body_free_variables in
      let closure_id, pointers = inc pointers in
      Let (var, Closure (closure_id, fvs_to_list body_free_variables), expr), vars, pointers, body_free_variables, add_block closure_id (Clos (fvs_to_list body_free_variables, argument_name), body_cps) body_continuations
    end
  (*
      let var = variable_name in
      expr
  *)
  | Var variable_name -> Let (var, Var variable_name, expr), vars, pointers, singleton_fv variable_name, empty_blocks
  | Let (var', e1, e2) -> begin
      let cps1, vars, pointers, fv1, conts1 = to_cps vars pointers fvs e2 var expr in
      let cps2, vars, pointers, fv2, conts2 = to_cps vars pointers (remove_fv (union_fvs fv1 fvs) var') e1 var' cps1 in
      cps2, vars, pointers, union_fvs fv2 (remove_fv fv1 var'), join_blocks conts1 conts2
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
    let scope_cps, vars, pointers, scope_free_variables, scope_conts = to_cps vars pointers fvs scope var expr in

    (*let scope_substitutions = scope_substitutions @ in*)
    
    (* Substitued binding variables in scope. *)
    let scope_binding_variable_ids = List.map (fun (var', _) -> var') bindings in

    (* Scope free variables without whose who are in bindings. *)
    let scope_free_variables_no_bindings = List.fold_left (fun fvs' fv -> remove_fv fvs' fv) scope_free_variables scope_binding_variable_ids in
    
    (*  *)
    let (vars, pointers, scope_and_closures_conts), closures = List.fold_left_map (fun (vars, pointers, scope_conts') (_, binding_expr) -> begin
      match binding_expr with
      | Fun (arg, binding_body_expr) ->
          let return_variable, vars = inc vars in
          let binding_body_cps, vars, pointers, binding_body_free_variables, binding_body_conts = to_cps vars pointers empty_fvs binding_body_expr return_variable (Return return_variable) in
          (vars, pointers, join_blocks binding_body_conts scope_conts'), (arg, binding_body_cps, binding_body_free_variables)
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

    let all_binding_bodies_free_variables = fold_fvs closures_caller_free_variable_ids in

    let vars, pointers, scope_cps, _scope_free_variables_no_bindings'', scope_and_closures_conts = List.fold_left (fun (vars, pointers, scope_cps', _scope_free_variables_no_bindings', scope_and_closures_conts') ((scope_binding_variable_id, binding_body_closure_continuation_id, binding_body_cps, bindind_body_bindind_variable_ids, binding_body_arg_id, binding_body_free_variables_no_arg_no_bindings), _caller_free_variable_ids) ->
      (* *)
      let binding_body_function_continuation_id, pointers = inc pointers in
                
      (* *)
      let bindind_body_bindind_closures_ids = List.map2 (fun bindind_body_bindind_variable_id (_, binding_body_binding_closure_continuation, _, _, _, _) -> (bindind_body_bindind_variable_id, binding_body_binding_closure_continuation)) bindind_body_bindind_variable_ids closures2 in
      
      (* TODO MUST FIX closure_continuation_id -> need Closure_rec *)
      (* *)
      let binding_body_with_free_and_binding_variables = List.fold_left (fun binding_body_with_free_variables' (bindind_body_bindind_variable_id, bindind_body_bindind_closures_id) -> Cps.Let (bindind_body_bindind_variable_id, Closure (bindind_body_bindind_closures_id, fvs_to_list all_binding_bodies_free_variables), binding_body_with_free_variables')) (Apply_block (binding_body_function_continuation_id, fvs_to_list (union_fvs (fvs_from_list bindind_body_bindind_variable_ids) (union_fvs (fvs_from_list binding_body_arg_id) all_binding_bodies_free_variables)))) bindind_body_bindind_closures_ids in

      (* *)
      vars, pointers, Cps.Let (scope_binding_variable_id, Closure (binding_body_closure_continuation_id, fvs_to_list all_binding_bodies_free_variables), scope_cps'), (remove_fv binding_body_free_variables_no_arg_no_bindings var), add_block binding_body_closure_continuation_id (Clos (fvs_to_list all_binding_bodies_free_variables, binding_body_arg_id), binding_body_with_free_and_binding_variables) (add_block binding_body_function_continuation_id (Cont (fvs_to_list (union_fvs (fvs_from_list bindind_body_bindind_variable_ids) (union_fvs (fvs_from_list binding_body_arg_id) binding_body_free_variables_no_arg_no_bindings))), binding_body_cps) scope_and_closures_conts')
    
    ) (vars, pointers, scope_cps, scope_free_variables_no_bindings, scope_and_closures_conts) (List.combine closures2 closures_caller_free_variable_ids) in
    scope_cps, vars, pointers, union_fvs all_binding_bodies_free_variables scope_free_variables_no_bindings, scope_and_closures_conts
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

      let true_cps, vars, pointers, true_free_variables_id, true_continuations = to_cps vars pointers fvs e2 e2_id (If_return (merge_kid, e2_id, fvs_to_list fvs)) in
      let false_cps, vars, pointers, false_free_variables_id, false_continuations = to_cps vars pointers fvs e3 e3_id (If_return (merge_kid, e3_id, fvs_to_list fvs)) in
      
      let cps1, vars, pointers, fv1, conts1 = to_cps vars pointers (union_fvs fvs (union_fvs true_free_variables_id false_free_variables_id)) e1 e1_id (If (e1_id, [(0, e3_kid, fvs_to_list false_free_variables_id)], (e2_kid, fvs_to_list true_free_variables_id), fvs_to_list fvs)) in

      cps1, vars, pointers, union_fvs fv1 (union_fvs true_free_variables_id false_free_variables_id), add_block merge_kid (If_join (var, fvs_to_list fvs), expr) (add_block e3_kid (If_branch (fvs_to_list false_free_variables_id, fvs_to_list fvs), false_cps) (add_block e2_kid (If_branch (fvs_to_list true_free_variables_id, fvs_to_list fvs), true_cps) (join_blocks true_continuations (join_blocks false_continuations conts1))))
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
      (* Varialbes that will hold expression result. *)
      let e1_id, vars = inc vars in
      let e2_id, vars = inc vars in

      (* Continuations IDs. *)
      let return_kid, pointers = inc pointers in

      let cps1, vars, pointers, fv1, conts1 = to_cps vars pointers (add_fv e1_id fvs) e2 e2_id (Call (e1_id, [e2_id], (return_kid, fvs_to_list fvs))) in
      let cps2, vars, pointers, fv2, conts2 = to_cps vars pointers (union_fvs fv1 fvs) e1 e1_id cps1 in
      cps2, vars, pointers, union_fvs fv2 fv1, add_block return_kid (Return (var, fvs_to_list fvs), expr) (join_blocks conts2 conts1)
    end
  (*

   *)
  | Match (pattern_expr, branchs, default_branch_expr) -> begin
      (* Return continuation after matching. *)
      let return_continuation_id, pointers = inc pointers in

      (* Default branch cps generation. *)
      let default_branch_return_id, vars = inc vars in
      let default_branch_cps, vars, pointers, default_branch_free_variables, default_branch_continuations = to_cps vars pointers fvs default_branch_expr default_branch_return_id (Match_return (return_continuation_id, default_branch_return_id, fvs_to_list fvs)) in
      
      (* Default branch continuation. *)
      let default_continuation_id, pointers = inc pointers in

      (* Branchs continuations and bodies informations. *)
      let (vars, pointers, branchs_continuations), branchs_bodies = List.fold_left_map (fun (vars, pointers, default_continuation') (branch_index, branch_arguments_names, branch_expr) -> begin
        (* Branch cps generation. *)
        let branch_return_id, vars = inc vars in
        let branch_cps, vars, pointers, branch_free_variables, branch_continuations = to_cps vars pointers fvs branch_expr branch_return_id (Match_return (return_continuation_id, branch_return_id, fvs_to_list fvs)) in
                
        (* Branch free variables that are not passed in arguments. *)
        let branch_free_variables = Cps.VarSet.filter (fun branch_free_variable -> not (List.mem branch_free_variable branch_arguments_names)) branch_free_variables in
        
        (* Branch cps reading arguments from payload. *)
        let branch_payload_id, vars = inc vars in
        let _branch_cps_with_payload = List.fold_left (fun branch_cps' (branch_argument_payload_index, branch_argument_id) -> Cps.Let (branch_argument_id, Get (branch_payload_id, branch_argument_payload_index), branch_cps')) branch_cps (List.mapi (fun i v -> i, v) branch_arguments_names) in
        
        (* Branch continuation and body informations. *)
        let branch_continuation_id, pointers = inc pointers in
        (vars, pointers, add_block branch_continuation_id (Match_branch (branch_arguments_names, fvs_to_list branch_free_variables, fvs_to_list fvs), branch_cps) (join_blocks branch_continuations default_continuation')), (branch_index, branch_continuation_id, branch_free_variables)
      end) (vars, pointers, empty_blocks) branchs in
      
      (* Substitued free variables. *)
      let free_variables_branchs = List.map (fun (_, _, fv2) -> fv2) branchs_bodies in

      (* Pattern matching. *)
      let pattern_id, vars = inc vars in
      let expr'', vars, pointers, fvs', conts = to_cps vars pointers (union_fvs fvs (union_fvs default_branch_free_variables (fold_fvs free_variables_branchs))) pattern_expr pattern_id (Match_pattern (pattern_id, List.map (fun ((_, branch_arguments_names, _), ((n, k, _), fvs')) -> n, k, branch_arguments_names, fvs_to_list fvs') (List.combine branchs (List.combine branchs_bodies free_variables_branchs)), (default_continuation_id, fvs_to_list default_branch_free_variables), fvs_to_list fvs)) in

      expr'', vars, pointers, union_fvs fvs' (union_fvs default_branch_free_variables (fold_fvs free_variables_branchs)), add_block default_continuation_id (Match_branch ([], fvs_to_list default_branch_free_variables, fvs_to_list fvs), default_branch_cps) (add_block return_continuation_id (Match_join (var, fvs_to_list fvs), expr) (join_blocks conts (join_blocks default_branch_continuations branchs_continuations)))
    end
    (*
    
    *)
  | Tuple args -> begin
      let vars, args_ids = List.fold_left_map (fun vars arg -> begin
        let var_id, vars = inc vars in
        vars, (var_id, arg)
      end) vars args in
      List.fold_left (fun (expr', vars, pointers, fv', conts') (var, e) -> begin
        let expr'', vars, pointers, fv'', conts'' = to_cps vars pointers (union_fvs (remove_fv fv' var) fvs) e var expr' in
        expr'', vars, pointers, union_fvs fv'' (remove_fv fv' var), join_blocks conts' conts''
      end) (Let (var, Tuple (List.map (fun (var, _) -> var) args_ids), expr), vars, pointers, fvs_from_list (List.map (fun (var, _) -> var) args_ids), empty_blocks) args_ids
    end
    (*
    
    *)
  | Constructor (tag, args) -> begin
      let vars, args_ids = List.fold_left_map (fun vars arg -> begin
        let var_id, vars = inc vars in
        vars, (var_id, arg)
      end) vars args in
      List.fold_left (fun (expr', vars, pointers, fv', conts') (var, e) -> begin
        let expr'', vars, pointers, fv'', conts'' = to_cps vars pointers (union_fvs (remove_fv fv' var) fvs) e var expr' in
        expr'', vars, pointers, union_fvs fv'' (remove_fv fv' var), join_blocks conts' conts''
      end) (Let (var, Constructor (tag, List.map (fun (var, _) -> var) args_ids), expr), vars, pointers, fvs_from_list (List.map (fun (var, _) -> var) args_ids), empty_blocks) args_ids
    end

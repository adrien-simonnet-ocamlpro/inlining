type var = string

type prim =
  | Add
  | Const of int
  | Print

type match_pattern =
| Int of int
| Joker

type 'var expr =
  | Var of 'var
  | Let of 'var * 'var expr * 'var expr
  | Let_rec of ('var * 'var expr) list * 'var expr
  | Fun of 'var * 'var expr
  | App of 'var expr * 'var expr
  | Prim of Cps.prim * 'var expr list
  | If of 'var expr * 'var expr * 'var expr
  | Match of 'var expr * (match_pattern * 'var expr) list
  | Match_pattern of 'var expr * (int * 'var list * 'var expr) list * 'var expr
  | Tuple of 'var expr list
  | Constructor of int * 'var expr list

(*
  type ('var, 'e) expr' =
  | Var of 'var
  | Let of 'var * 'e * 'e
  | Fun of 'var * 'e
  | App of 'e * 'e
  | Prim of Cps.prim * 'e list
  | If of 'e * 'e * 'e

type 'var expr = T of ('var, 'var expr) expr'

type ('var, 'info) expr' =
  | Var of 'var
  | Let of 'var * 'var expr * 'var expr
  | Fun of 'var * 'var expr
  | App of 'var expr * 'var expr
  | Prim of Cps.prim * 'var expr list
  | If of 'var expr * 'var expr * 'var expr

  and ('var, 'info) expr = {
info : 'info;
expr : ('var, 'info) expr'

  }
  and ('var, 'info) expr = expr'
*)

(* let rec sprintf_prim (prim : prim) args =
  match prim, args with
  | Const x, _ -> string_of_int x
  | Add, x1 :: x2 :: _ -> Printf.sprintf "(%s + %s)" (sprintf x1) (sprintf x2)
  | Print, x1 :: _ -> Printf.sprintf "(print %s)" (sprintf x1)
  | _ -> failwith "invalid args"

and sprintf (ast : expr) : string =
  match ast with
  | Fun (x, e) -> Printf.sprintf "(fun %s -> %s)" x (sprintf e)
  | Var x -> x
  | Prim (prim, args) -> sprintf_prim prim args
  | Let (var, e1, e2) ->
    Printf.sprintf "(let %s = %s in\n%s)" var (sprintf e1) (sprintf e2)
  | If (cond, t, f) ->
    Printf.sprintf "(if %s = 0 then %s else %s)" (sprintf cond) (sprintf t) (sprintf f)
  | App (e1, e2) -> Printf.sprintf "(%s %s)" (sprintf e1) (sprintf e2)
;; *)

let rec pp_expr fmt = function
  | Fun (x, e) -> Format.fprintf fmt "(fun %s -> %a)" x pp_expr e
  | Var x -> Format.fprintf fmt "%s" x
  | Prim (Const x, _) -> Format.fprintf fmt "%d" x
  | Prim (Add, x1 :: x2 :: _) -> Format.fprintf fmt "(%a + %a)" pp_expr x1 pp_expr x2
  | Prim (Sub, x1 :: x2 :: _) -> Format.fprintf fmt "(%a - %a)" pp_expr x1 pp_expr x2
  | Prim (Print, x1 :: _) -> Format.fprintf fmt "(print %a)" pp_expr x1
  | Let (var, e1, e2) ->
    Format.fprintf fmt "(let %s = %a in\n%a)" var pp_expr e1 pp_expr e2
  | If (cond, t, f) ->
    Format.fprintf fmt "(if %a = 0 then %a else %a)" pp_expr cond pp_expr t pp_expr f
  | App (e1, e2) -> Format.fprintf fmt "(%a %a)" pp_expr e1 pp_expr e2
  | _ -> failwith "invalid args"
;;



let print_expr e = pp_expr Format.std_formatter e



let sprintf e = Format.asprintf "%a" pp_expr e

(* let through_buf e =
  let buf = Buffer.create 512 in 
  let fmt = Format.formatter_of_buffer buf in 
  print_expr fmt "%a" pp_expr e *)



let vars = ref 0
let conts = ref 0

let inc vars =
  vars := !vars + 1;
  !vars
;;

let inc_conts () =
  conts := !conts + 1;
  !conts
;;

let add_subs env var va = (var, va) :: env

let get_subs env var =
  match List.find_opt (fun (var', _) -> var = var') env with
  | Some (_, v) -> v
  | None ->
    failwith
      (var
       ^ " not found in "
       ^ List.fold_left (fun str (x, _) -> str ^ " x" ^ x) "[" env
       ^ " ].")
;;

let remove_var fvs var = List.filter (fun fv -> not (fv = var)) fvs

let has_fv fv fvs = List.mem fv fvs

let rec has_free_subs var subs fvs =
  match subs with
  | [] -> false
  | (var', sub)::_ when var = var' && has_fv sub fvs -> true
  | _::subs' -> has_free_subs var subs' fvs

let rec get_free_subs var subs fvs =
  match subs with
  | [] -> assert false
  | (var', sub)::_ when var = var' && has_fv sub fvs -> sub
  | _::subs' -> get_free_subs var subs' fvs

module FreeVars = Set.Make (Int)

let join_fv fv1 fv2 = FreeVars.elements (FreeVars.union (FreeVars.of_list fv1) (FreeVars.of_list fv2))
let join_fvs fvs = List.fold_left (fun fvs fv -> join_fv fvs fv) [] fvs


exception Failure of string

let rec find x lst =
    match lst with
    | [] -> raise (Failure "Not Found")
    | h :: t -> if x = h then 0 else 1 + find x t

let has_var_name = Env.has_var
let get_var_name = Env.get_var

let has_var_id = Env.has_value
let get_var_id = Env.get_value

let rec to_cps conts fv0 (ast : 'var expr) var (expr : Cps.expr) (substitutions : (string * int) list) : Cps.expr * (string * int) list * int list * Cps.cont =
  match ast with
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
      let body_return_id = inc vars in
      let body_cps, body_substitutions, body_free_variables, body_continuations = to_cps conts [] body body_return_id (Return body_return_id) [] in
      let body_argument_id = if has_var_name body_substitutions argument_name then get_var_id body_substitutions argument_name else inc vars in
      let body_free_variables = List.filter (fun body_free_variable -> body_free_variable <> body_argument_id) body_free_variables in
      let environment_id = inc vars in
      let function_id = inc_conts () in
      let _, body = List.fold_left (fun (pos, cps') body_free_variable -> pos + 1, Cps.Let (body_free_variable, Cps.Get (environment_id, pos), cps')) (0, Apply_cont (function_id, body_argument_id :: body_free_variables, [])) body_free_variables in
      let closure_environment_id = inc vars in
      let closure_id = inc_conts () in
      Let (closure_environment_id, Environment body_free_variables, (Let (var, Closure (closure_id, closure_environment_id), expr))), body_substitutions @ substitutions, (remove_var (body_free_variables @ fv0) var), (Let_cont (closure_id, [environment_id; body_argument_id], body, Let_cont (function_id, body_argument_id :: body_free_variables, body_cps, body_continuations)))
    end
  (*
      let var = variable_name in
      expr
  *)
  | Var variable_name -> begin
      if has_var_name substitutions variable_name
      then Let (var, Var (get_subs substitutions variable_name), expr), substitutions, (remove_var fv0 var), conts
      else begin
        let variable_id = inc vars in
        Let (var, Var variable_id, expr), (variable_name, variable_id) :: substitutions, variable_id :: (remove_var fv0 var), conts
      end
    end
  (*
        let argument_id_1 = argument_1 in
        ...
        let argument_id_n = argument_n in
        let var = prim argument_id_1 ... argument_id_n in
        expr
  *)
  | Prim (prim, arguments) -> begin
      let arguments_ids = List.map (fun _ -> inc vars) arguments in
      List.fold_left (fun (expr', substitutions', fv', conts') (argument_id, argument) -> to_cps conts' fv' argument argument_id expr' substitutions') (Let (var, Prim (prim, arguments_ids), expr), substitutions, arguments_ids @ (remove_var fv0 var), conts) (List.combine arguments_ids arguments)
    end
    (*
        let v1 = e1 in
        let var = e2 in
        expr
    *)
  | Let (var', e1, e2) -> begin
      let cps1, substitutions1, fv1, conts1 = to_cps conts fv0 e2 var expr substitutions in
      let v1 = if has_var_name substitutions1 var' then get_var_id substitutions1 var' else inc vars in
      let cps2, substitutions2, fv2, conts2 = to_cps conts1 (remove_var fv1 v1) e1 v1 cps1 (List.filter (fun (_, v) -> not (v = v1)) substitutions1) in
      cps2, (if has_var_name substitutions1 var' then substitutions2 else add_subs substitutions2 var' v1), fv2, conts2
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
    let scope_cps, scope_substitutions, scope_free_variables, scope_conts = to_cps conts fv0 scope var expr substitutions in

    (* Substitued binding variables in scope. *)
    let scope_binding_variable_ids = List.map (fun (var', _) -> if has_var_name scope_substitutions var' then get_var_id scope_substitutions var' else inc vars) bindings in

    (* Scope free variables without whose who are in bindings. *)
    let scope_free_variables_no_bindings = List.fold_left (fun fvs fv -> remove_var fvs fv) scope_free_variables scope_binding_variable_ids in

    (* Scope substitutions without whose who are in bindings. *)
    let scope_substitutions_no_bindings = List.fold_left (fun scope_substitutions' scope_binding_variable_id -> (List.filter (fun (_, v) -> not (v = scope_binding_variable_id)) scope_substitutions')) scope_substitutions scope_binding_variable_ids in
    
    (*let s = List.map2 (fun (var', _) v1 -> (if has_var_name substitutions1 var' then substitutions1 else add_subs substitutions1 var' v1)) bindings v1s in*)

    (*  *)
    let scope_and_closures_conts, closures = List.fold_left_map (fun scope_conts' (_, binding_expr) -> begin
      match binding_expr with
      | Fun (arg, binding_body_expr) ->
          let return_variable = inc vars in
          let binding_body_cps, binding_body_substitutions, binding_body_free_variables, binding_body_conts = to_cps scope_conts' [] binding_body_expr return_variable (Return return_variable) [] in
          binding_body_conts, (arg, binding_body_cps, binding_body_substitutions, binding_body_free_variables)
      | _ -> assert false
    end) scope_conts bindings in
    
    
    (* let closures = List.rev closures in *)


    let closures2 = List.map2 (fun scope_binding_variable_id (arg, binding_body_cps, binding_body_substitutions, binding_body_free_variables) ->
      (* Substitued binding variables in body. *)
      let bindind_body_bindind_variable_ids = List.map (fun (binding_name, _) -> if has_var_name binding_body_substitutions binding_name then get_var_id binding_body_substitutions binding_name else inc vars) bindings in
      
      (* Substitued arg in body. *)
      let binding_body_arg_id = if has_var_name binding_body_substitutions arg then get_var_id binding_body_substitutions arg else inc vars in
      
      (* Body free variables without whose who are in bindings. *)
      let binding_body_free_variables_no_arg_no_bindings = (List.filter (fun binding_body_free_variable -> not (binding_body_free_variable = binding_body_arg_id) && not (List.mem binding_body_free_variable bindind_body_bindind_variable_ids)) binding_body_free_variables) in
      scope_binding_variable_id, inc_conts (), binding_body_cps, binding_body_substitutions, bindind_body_bindind_variable_ids, binding_body_arg_id, binding_body_free_variables_no_arg_no_bindings) scope_binding_variable_ids closures in


    (* Free variable names inside closures. *)
    let closures_free_variable_names = List.map (fun (_, _, _, binding_body_substitutions, _, _, binding_body_free_variables_no_arg_no_bindings) -> List.map (fun fv -> get_var_name binding_body_substitutions fv) (List.filter (fun fv -> has_var_id binding_body_substitutions fv && not (has_var_name substitutions (get_var_name binding_body_substitutions fv))) binding_body_free_variables_no_arg_no_bindings)) closures2 in

    (* Substitution of all free variables inside closures. *)
    let all_binding_bodies_substitutions = List.fold_left (fun substitutions' fv -> if has_var_name substitutions fv then add_subs substitutions' fv (get_var_id substitutions fv) else if has_var_name substitutions' fv then substitutions' else add_subs substitutions' fv (inc vars)) [] (List.concat closures_free_variable_names) in

    (* Free variable ids (caller). *)
    let closures_caller_free_variable_ids = List.map (fun (_, _, _, binding_body_substitutions, _, _, binding_body_free_variables_no_arg_no_bindings) -> List.map (fun fv -> if has_var_id binding_body_substitutions fv then let fval = get_var_name binding_body_substitutions fv in get_var_id all_binding_bodies_substitutions fval else fv) binding_body_free_variables_no_arg_no_bindings) closures2 in

    let _all_binding_bodies_free_variables = List.fold_left (fun fvs' (_, _, _, _, _, _, binding_body_free_variables_no_arg_no_bindings) -> join_fv fvs' binding_body_free_variables_no_arg_no_bindings) [] closures2 in

    let all_binding_bodies_free_variables = join_fvs closures_caller_free_variable_ids in

    let environment_id = inc vars in

    let scope_cps, substitutions'', _scope_free_variables_no_bindings'', scope_and_closures_conts = List.fold_left (fun (scope_cps', substitutions', _scope_free_variables_no_bindings', scope_and_closures_conts') ((scope_binding_variable_id, binding_body_closure_continuation_id, binding_body_cps, _binding_body_substitutions, bindind_body_bindind_variable_ids, binding_body_arg_id, binding_body_free_variables_no_arg_no_bindings), caller_free_variable_ids) ->
      (* *)
      let binding_body_function_continuation_id = inc_conts () in
      
      (* *)
      let local_environment_id = inc vars in
      
      (* *)
      let binding_body_with_free_variables = List.fold_left (fun binding_body_function_continuation binding_body_free_variable_no_arg_no_bindings -> Cps.Let (binding_body_free_variable_no_arg_no_bindings, Cps.Get (local_environment_id, (find binding_body_free_variable_no_arg_no_bindings all_binding_bodies_free_variables)), binding_body_function_continuation)) (Apply_cont (binding_body_function_continuation_id, bindind_body_bindind_variable_ids @ (binding_body_arg_id :: caller_free_variable_ids), [])) caller_free_variable_ids in
    
      (* *)
      let bindind_body_bindind_closures_ids = List.map2 (fun bindind_body_bindind_variable_id (_, binding_body_binding_closure_continuation, _, _, _, _, _) -> (bindind_body_bindind_variable_id, binding_body_binding_closure_continuation)) bindind_body_bindind_variable_ids closures2 in
      
      (* *)
      let binding_body_with_free_and_binding_variables = List.fold_left (fun binding_body_with_free_variables' (bindind_body_bindind_variable_id, bindind_body_bindind_closures_id) -> Cps.Let (bindind_body_bindind_variable_id, Cps.Closure (bindind_body_bindind_closures_id, local_environment_id), binding_body_with_free_variables')) binding_body_with_free_variables bindind_body_bindind_closures_ids in

      (* *)
      Cps.Let (scope_binding_variable_id, Closure (binding_body_closure_continuation_id, environment_id), scope_cps'), substitutions', (remove_var (binding_body_free_variables_no_arg_no_bindings) var), (Cps.Let_cont (binding_body_closure_continuation_id, [local_environment_id; binding_body_arg_id], binding_body_with_free_and_binding_variables, Let_cont (binding_body_function_continuation_id, bindind_body_bindind_variable_ids @ (binding_body_arg_id :: binding_body_free_variables_no_arg_no_bindings), binding_body_cps, scope_and_closures_conts')))
    
    ) (scope_cps, substitutions, scope_free_variables_no_bindings, scope_and_closures_conts) (List.combine closures2 closures_caller_free_variable_ids) in
    Cps.Let (environment_id, Environment all_binding_bodies_free_variables, scope_cps), all_binding_bodies_substitutions @ scope_substitutions_no_bindings @ substitutions'', join_fv all_binding_bodies_free_variables scope_free_variables_no_bindings, scope_and_closures_conts
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
        let condition_id = inc vars in

        let true_id = inc vars in
        let false_id = inc vars in

        let merge_continuation_id = inc_conts () in
        let true_continuation_id = inc_conts () in
        let false_continuation_id = inc_conts () in
        let true_cps, true_substitutions, true_free_variables_id, true_continuations = to_cps (Let_cont (merge_continuation_id, var :: fv0, expr, conts)) fv0 true_expr true_id (Apply_cont (merge_continuation_id, true_id :: fv0, [])) [] in
        let false_cps, false_substitutions, false_free_variables_id, true_and_false_continuations = to_cps true_continuations fv0 false_expr false_id (Apply_cont (merge_continuation_id, false_id :: fv0, [])) [] in
        
        (* Var names in branchs that are not substitued in the beginning of If statement (free variables). *)
        let true_expr_free_variables_names = List.map (fun fv -> get_var_name true_substitutions fv) (List.filter (fun fv -> has_var_id true_substitutions fv && not (has_var_name substitutions (get_var_name true_substitutions fv))) true_free_variables_id) in
        let false_expr_free_variables_names = List.map (fun fv -> get_var_name false_substitutions fv) (List.filter (fun fv -> has_var_id false_substitutions fv && not (has_var_name substitutions (get_var_name false_substitutions fv))) false_free_variables_id) in

        (* Substitution of free variables. *)
        let true_false_substitutions = List.fold_left (fun substitutions' fv -> if has_var_name substitutions' fv then substitutions' else add_subs substitutions' fv (inc vars)) [] (true_expr_free_variables_names @ false_expr_free_variables_names) in

        (* Substitued free variables. *)
        let true_caller_arguments_id = List.map (fun fv -> if has_var_id true_substitutions fv then let fval = get_var_name true_substitutions fv in get_var_id true_false_substitutions fval else fv) true_free_variables_id in
        let false_caller_arguments_id = List.map (fun fv -> if has_var_id false_substitutions fv then let fval = get_var_name false_substitutions fv in get_var_id true_false_substitutions fval else fv) false_free_variables_id in

        let cps1, substitutions1, fv1, conts1 = to_cps (Let_cont (true_continuation_id, true_free_variables_id, true_cps, Let_cont (false_continuation_id, false_free_variables_id, false_cps, true_and_false_continuations))) (condition_id :: (join_fv true_caller_arguments_id false_caller_arguments_id)) condition_expr condition_id (If (condition_id, [(0, false_continuation_id, false_caller_arguments_id)], (true_continuation_id, true_caller_arguments_id), [])) (true_false_substitutions @ substitutions) in
        cps1, substitutions1 @ true_substitutions @ false_substitutions, fv1, conts1
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
      let closure_id = inc vars in
      let argument_id = inc vars in
      let closure_continuation_id = inc vars in
      let closure_environment_id = inc vars in
      let fv0 = remove_var fv0 var in
      let cps1, substitutions1, fv1, conts1 = to_cps (Let_cont (return_continuation, var :: fv0, expr, conts)) (closure_id :: fv0) argument_expr argument_id (Let (closure_continuation_id, Get (closure_id, 0), Let (closure_environment_id, Get (closure_id, 1), Call (closure_continuation_id, [closure_environment_id; argument_id], [(return_continuation, fv0)])))) substitutions in
      let cps2, substitutions2, fv2, conts2 = to_cps conts1 (remove_var fv1 closure_id) closure_expr closure_id cps1 substitutions1 in
      cps2, substitutions2, fv2, conts2
    end

  | Match (expr', matchs) ->
    let fv0 = remove_var fv0 var in
      let k_return = inc_conts () in
      let conts = Cps.Let_cont (k_return, var :: fv0, expr, conts) in
      
      let kdefault, substitutions_default, free_variables_default, conts3 =
      if List.exists (fun (t, _) -> match t with
      | Joker -> true | _ -> false) matchs then 

        let _, expr' = List.find (fun (t, _) -> match t with
        | Joker -> true | _ -> false) matchs in
        let k = inc_conts () in
        let v2 = inc vars in
        let cps1, substitutions1, fv, conts1 =
          to_cps conts fv0 expr' v2 (Apply_cont (k_return, v2::fv0, [])) []
        in k, substitutions1, fv, Cps.Let_cont (k, fv, cps1, conts1)

      else let k = inc_conts () in k, [], [], Let_cont (k, [], Let (0, Prim (Const (-1), []), Let (1, Prim (Print, [0]), Apply_cont (k_return, 1::fv0, []))), conts) in
      
      let matchs' = List.filter (fun (t, _) -> match t with
      | Int _ -> true | Joker -> false) matchs in

      let conts3, matchs'' = List.fold_left_map (fun conts3 (pattern, e) -> begin
        match pattern with
        | Int n ->
         let k' = inc_conts () in
         let v2 = inc vars in
         let cps1, substitutions1, fv, conts1 = to_cps conts3 fv0 e v2 (Apply_cont (k_return, v2::fv0, [])) [] in
         Cps.Let_cont (k', fv, cps1, conts1), (n, k', fv, substitutions1)
         
         | _ -> assert false
      end) conts3 matchs' in

      (* FVS NOT IMPLEMENTED *)

      (* Var names in branchs that are not substitued in the beginning of Match statement (free variables). *)
      let free_variable_names_branchs = List.map (fun (n, k, fv2, substitutions2) -> n, k, fv2, List.map (fun fv -> get_var_name substitutions2 fv) (List.filter (fun fv -> has_var_id substitutions2 fv && not (has_var_name substitutions (get_var_name substitutions2 fv))) fv2), substitutions2) matchs''
      in
      let free_variable_names_default = List.map (fun fv -> get_var_name substitutions_default fv) (List.filter (fun fv -> has_var_id substitutions_default fv && not (has_var_name substitutions (get_var_name substitutions_default fv))) free_variables_default)
    in


      (* Substitution of free variables. *)
      let substitutions_branchs = List.fold_left (fun substitutions' fv -> if has_var_name substitutions' fv then substitutions' else add_subs substitutions' fv (inc vars)) [] (List.concat ((List.map (fun (_, _, _, fv2, _) -> fv2) free_variable_names_branchs)) @ free_variable_names_default) in

      (* Substitued free variables. *)
      let free_variables_branchs = List.map (fun (n, k, fv2, _, substitutions_e2) -> n, k, List.map (fun fv -> if has_var_id substitutions_e2 fv then let fval = get_var_name substitutions_e2 fv in get_var_id substitutions_branchs fval else fv) fv2) free_variable_names_branchs in
      let free_variables_default = List.map (fun fv -> if has_var_id substitutions_default fv then let fval = get_var_name substitutions_default fv in get_var_id substitutions_branchs fval else fv) free_variables_default in


      let var_match = inc vars in
      to_cps conts3 ((List.fold_left (fun acc (_,_,fv) -> acc@fv) free_variables_default free_variables_branchs)) expr' var_match (If (var_match, free_variables_branchs, (kdefault, free_variables_default), [])) substitutions_branchs




      | Match_pattern (expr', matchs, default) ->
        let var_match = inc vars in
        let var_match_index = inc vars in
        let var_payload = inc vars in


        let fv0 = remove_var fv0 var in
          let k_return = inc_conts () in
          let conts = Cps.Let_cont (k_return, var :: fv0, expr, conts) in
          
          let kdefault, substitutions_default, free_variables_default, conts3 =

    
            let expr' = default in
            let k = inc_conts () in
            let v2 = inc vars in
            let cps1, substitutions1, fv, conts1 =
              to_cps conts fv0 expr' v2 (Apply_cont (k_return, v2::fv0, [])) []
            in k, substitutions1, fv, Cps.Let_cont (k, fv, cps1, conts1) in
              
          let matchs' = matchs in
    
          let conts3, matchs'' = List.fold_left_map (fun conts3 (n, payload_values, e) -> begin

             let k' = inc_conts () in
             let v2 = inc vars in

             let cps1, substitutions1, fv, conts1 = to_cps conts3 (v2::fv0) e v2 (Apply_cont (k_return, v2::fv0, [])) [] in

             let payload_substitutions = List.map (fun payload_value -> if has_var_name substitutions1 payload_value then get_var_id substitutions1 payload_value else inc vars) payload_values in

             let fv = List.filter (fun fv' -> not (List.mem fv' payload_substitutions)) fv in

             let branch_expr_with_payload = List.fold_left (fun cps ((_, pos), substition) -> Cps.Let (substition, Get (var_payload, pos), cps)) cps1 (List.combine (List.mapi (fun i v -> v, i) payload_values) payload_substitutions) in

             Let_cont (k', var_payload::fv, branch_expr_with_payload, conts1), (n, k', fv, substitutions1)

          end) conts3 matchs' in
    
          (* FVS NOT IMPLEMENTED *)
    
          (* Var names in branchs that are not substitued in the beginning of Match statement (free variables). *)
          let free_variable_names_branchs = List.map (fun (n, k, fv2, substitutions2) -> n, k, fv2, List.map (fun fv -> get_var_name substitutions2 fv) (List.filter (fun fv -> has_var_id substitutions2 fv && not (has_var_name substitutions (get_var_name substitutions2 fv))) fv2), substitutions2) matchs''
          in
          let free_variable_names_default = List.map (fun fv -> get_var_name substitutions_default fv) (List.filter (fun fv -> has_var_id substitutions_default fv && not (has_var_name substitutions (get_var_name substitutions_default fv))) free_variables_default)
        in
    
    
          (* Substitution of free variables. *)
          let substitutions_branchs = List.fold_left (fun substitutions' fv -> if has_var_name substitutions' fv then substitutions' else add_subs substitutions' fv (inc vars)) [] (List.concat ((List.map (fun (_, _, _, fv2, _) -> fv2) free_variable_names_branchs)) @ free_variable_names_default) in
    
          (* Substitued free variables. *)
          let free_variables_branchs = List.map (fun (n, k, fv2, _, substitutions_e2) -> n, k, List.map (fun fv -> if has_var_id substitutions_e2 fv then let fval = get_var_name substitutions_e2 fv in get_var_id substitutions_branchs fval else fv) fv2) free_variable_names_branchs in
          let free_variables_default = List.map (fun fv -> if has_var_id substitutions_default fv then let fval = get_var_name substitutions_default fv in get_var_id substitutions_branchs fval else fv) free_variables_default in
    


          to_cps conts3 ((List.fold_left (fun acc (_,_,fv) -> acc@fv) free_variables_default free_variables_branchs)) expr' var_match (Cps.Let (var_match_index, Get (var_match, 0), (Cps.Let (var_payload, Get (var_match, 1), If (var_match_index, List.map (fun (n, k, fvs) -> n, k, var_payload::fvs) free_variables_branchs, (kdefault, free_variables_default), []))))) substitutions_branchs
    
          | Tuple args -> let vars = List.map (fun arg -> inc vars, arg) args in
          List.fold_left
            (fun (expr', substitutions', fv', conts') (var, e) ->
              let cps1, substitutions1, fv1, conts1 = to_cps conts' fv' e var expr' substitutions' in
              cps1, substitutions1, fv1, conts1)
            (Let (var, Tuple (List.map (fun (var, _) -> var) vars), expr), substitutions, (List.map (fun (var, _) -> var) vars)@(remove_var fv0 var), conts)
            vars

            | Constructor (tag, args) -> let env_variable = inc vars in
              let vars = List.map (fun arg -> inc vars, arg) args in
          List.fold_left
            (fun (expr', substitutions', fv', conts') (var, e) ->
              let cps1, substitutions1, fv1, conts1 = to_cps conts' fv' e var expr' substitutions' in
              cps1, substitutions1, fv1, conts1)
              (Let (env_variable, Environment (List.map (fun (var, _) -> var) vars), (Let (var, Constructor (tag, env_variable), expr))), substitutions, (List.map (fun (var, _) -> var) vars)@(remove_var fv0 var), conts)
            vars

    ;;


let add_subs subs var = List.length subs, ( var, List.length subs )::subs
let is_subs = Env.has

let get_subst = get_subs

let add_fv fvs fv = fv::fvs
let has_subst vars var = List.exists (fun (var', _) -> var = var') vars
let rec unfree fvs fv = match fvs with
| (fv', _)::fvs' when fv = fv' -> fvs'
| (fv', v)::fvs' -> (fv', v)::(unfree fvs' fv)
| [] -> failwith "substitution not found"

let print_fv env =
  Printf.printf "%s ]\n%!" (List.fold_left (fun str v -> str ^ " " ^ v) "[" env)
;;

let rec alpha_conversion (fvs: (var * Cps.var) list) (ast : 'var expr) (substitutions : (string * int) list)
  : Cps.var expr * (var * Cps.var) list * (var * Cps.var) list
  =
  match ast with
  | Let (var, e1, e2) ->
    let e1', substitutions', fvs' = alpha_conversion fvs e1 substitutions in
    let var', substitutions'' = add_subs substitutions' var in
    let fvs'' = add_fv fvs' (var, var') in
    let e2', substitutions''', fvs''' = alpha_conversion fvs'' e2 substitutions'' in
    Let (get_subst substitutions'' var, e1', e2'), substitutions''', unfree fvs''' var
  | Let_rec ([var, e1], e2) ->
    let e1', substitutions', fvs' = alpha_conversion fvs e1 substitutions in
    let var', substitutions'' = add_subs substitutions' var in
    let fvs'' = add_fv fvs' (var, var') in
    let e2', substitutions''', fvs''' = alpha_conversion fvs'' e2 substitutions'' in
    Let_rec ([get_subst substitutions'' var, e1'], e2'), substitutions''', unfree fvs''' var
    | Let_rec (_, _) -> assert false
  | Var var -> if has_subst fvs var then Var (get_subst fvs var), substitutions, fvs else let var', substitutions = add_subs substitutions var in Var (var'), substitutions, add_fv fvs (var, var')
  | Prim (prim, exprs) ->
    let exprs''', substitutions''', fvs''' = List.fold_left (fun (expr', substitutions', fvs') expr ->
        let expr'', substitutions'', fvs'' = alpha_conversion fvs' expr substitutions' in
        expr'@[expr''], substitutions'', fvs'') ([], substitutions, fvs) exprs
    in Prim (prim, exprs'''), substitutions''', fvs'''
  | Fun (var, e) ->
    let var', substitutions' = add_subs substitutions var in
    let fvs' = add_fv fvs (var, var') in
    let e', substitutions'', fvs'' = alpha_conversion fvs' e substitutions' in
    Fun (var', e'), substitutions'', unfree fvs'' var
  | If (cond, t, f) ->
    let cond', substitutions', fvs' = alpha_conversion fvs cond substitutions in
    let t', substitutions'', fvs'' = alpha_conversion fvs' t substitutions' in
    let f', substitutions''', fvs''' = alpha_conversion fvs'' f substitutions'' in
    If (cond', t', f'), substitutions''', fvs'''
  | App (e1, e2) ->
    let e1', substitutions', fvs' = alpha_conversion fvs e1 substitutions in
    let e2', substitutions'', fvs'' = alpha_conversion fvs' e2 substitutions' in
    App (e1', e2'), substitutions'', fvs''
  | _ -> assert false
;;

let gen_name id env =
  match Env.get_name id env with
  | Some (v, _) -> v ^ "_" ^ (string_of_int id)
  | None -> "_" ^ (string_of_int id)

let rec pp_expr_int subs fmt = function
  | Fun (x, e) -> Format.fprintf fmt "(fun %s -> %a)" (gen_name x subs) (pp_expr_int subs) e
  | Var x -> Format.fprintf fmt "%s" (gen_name x subs)
  | Prim (Const x, _) -> Format.fprintf fmt "%d" x
  | Prim (Add, x1 :: x2 :: _) -> Format.fprintf fmt "(%a + %a)" (pp_expr_int subs) x1 (pp_expr_int subs) x2
  | Prim (Sub, x1 :: x2 :: _) -> Format.fprintf fmt "(%a - %a)" (pp_expr_int subs) x1 (pp_expr_int subs) x2
  | Prim (Print, x1 :: _) -> Format.fprintf fmt "(print %a)" (pp_expr_int subs) x1
  | Let (var, e1, e2) ->
    Format.fprintf fmt "(let %s = %a in\n%a)" (gen_name var subs) (pp_expr_int subs) e1 (pp_expr_int subs) e2
  | Let_rec ([var, e1], e2) ->
    Format.fprintf fmt "(let rec %s = %a in\n%a)" (gen_name var subs) (pp_expr_int subs) e1 (pp_expr_int subs) e2
  | If (cond, t, f) ->
    Format.fprintf fmt "(if %a = 0 then %a else %a)" (pp_expr_int subs) cond (pp_expr_int subs) t (pp_expr_int subs) f
  | App (e1, e2) -> Format.fprintf fmt "(%a %a)" (pp_expr_int subs) e1 (pp_expr_int subs) e2
  | _ -> failwith "invalid args"
;;

let print_expr_int e subs = (pp_expr_int subs) Format.std_formatter e
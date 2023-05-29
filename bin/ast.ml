type var = string

type prim =
  | Add
  | Const of int
  | Print

type 'var expr =
  | Var of 'var
  | Let of 'var * 'var expr * 'var expr
  | Let_rec of 'var * 'var expr * 'var expr
  | Fun of 'var * 'var expr
  | App of 'var expr * 'var expr
  | Prim of Cps.prim * 'var expr list
  | If of 'var expr * 'var expr * 'var expr
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

let rec to_cps ?(recursive = (None : var option)) conts fv0 (ast : 'var expr) var (expr : Cps.expr) (substitutions : (string * int) list) : Cps.expr * (string * int) list * int list * Cps.cont =
  match ast with
  | Fun (x, e) ->
    let k1 = inc_conts () in
    let k2 = inc_conts () in
    let v1 = inc vars in
    let v2 = inc vars in
    let cps1, substitutions1, fv, conts1 =
      to_cps conts [] e v2 (Return (v2)) []
    in
    begin match recursive with
    | None -> begin
    let v5 = if Env.has substitutions1 x then Env.get substitutions1 x else inc vars in
    let fv =  (List.filter (fun fv' -> fv' != v5) fv) in
    let _, body = List.fold_left (fun (pos, cps') fv' -> pos + 1, Cps.Let (fv', Cps.Get (v1, pos), cps')) (0, Apply_cont (k2, v5::fv, [])) fv in
        Let (var, Closure (k1, fv), expr), substitutions1 @ substitutions, (remove_var (fv @ fv0) var), (Let_cont (k1, [v1; v5], body, Let_cont (k2, v5::fv, cps1, conts1)))
    end
    | Some var' -> begin
      let v6 = if Env.has substitutions1 var' then Env.get substitutions1 var' else inc vars in

      let v5 = if Env.has substitutions1 x then Env.get substitutions1 x else inc vars in
    let fv =  (List.filter (fun fv' -> fv' != v5 && fv' != v6) fv) in
    let _, body = List.fold_left (fun (pos, cps') fv' -> pos + 1, Cps.Let (fv', Cps.Get (v1, pos), cps')) (0, Let (v6, Closure (k1, fv), Apply_cont (k2, v6::v5::fv, []))) fv in
        Let (var, Closure (k1, fv), expr), substitutions1 @ substitutions,  (remove_var (fv @ fv0) var), (Let_cont (k1, [v1; v5], body, Let_cont (k2, v6::v5::fv, cps1, conts1)))
    end
  end

  
        (*
      let var = x in (expr var fv...)
  *)
  | Var x ->
    if Env.has substitutions x
    then Let (var, Var (get_subs substitutions x), expr), substitutions, (remove_var fv0 var), conts
    else (
      let v1 = inc vars in
      Let (var, Var v1, expr), ( x, v1 )::substitutions, v1::(remove_var fv0 var), conts)
  
  (*
      let v1 = e1 in
      ...
      let vn = en in
      let var = prim v1 ... vn in
      expr var f0...
  *)
  | Prim (prim, args) ->
    let vars = List.map (fun arg -> inc vars, arg) args in
    List.fold_left
      (fun (expr', substitutions', fv', conts') (var, e) ->
        let cps1, substitutions1, fv1, conts1 = to_cps conts' fv' e var expr' substitutions' in
        cps1, substitutions1, fv1, conts1)
      (Let (var, Prim (prim, List.map (fun (var, _) -> var) vars), expr), substitutions, (List.map (fun (var, _) -> var) vars)@(remove_var fv0 var), conts)
      vars

    (*
       let v1 = e1 in
       let var = e2 in expr var fv0...
    *)
  | Let (var', e1, e2) ->
    let cps1, substitutions1, fv1, conts1 = to_cps conts fv0 e2 var expr substitutions in
    let v1 = if Env.has_var substitutions1 var' then Env.get_value substitutions1 var' else inc vars in
    let cps2, substitutions2, fv2, conts2 = to_cps conts1 (List.filter (fun fv -> not (fv = v1)) fv1) e1 v1 cps1 (List.filter (fun (_, v) -> not (v = v1)) substitutions1) in
    cps2, (if Env.has_var substitutions1 var' then substitutions2 else add_subs substitutions2 var' v1), fv2, conts2

    (*
       let rec v1 = e1 in
       let var = e2 in expr var fv0...
    *)
  | Let_rec (var', e1, e2) ->
    let cps1, substitutions1, fv1, conts1 = to_cps conts fv0 e2 var expr substitutions in
    let v1 = if Env.has_var substitutions1 var' then Env.get_value substitutions1 var' else inc vars in
    let s = (if Env.has_var substitutions1 var' then substitutions1 else add_subs substitutions1 var' v1) in
    let cps2, substitutions2, fv2, conts2 = to_cps ~recursive:(Some var') conts1 fv1 e1 v1 cps1 s in
    cps2, (if Env.has_var substitutions1 var' then substitutions2 else add_subs substitutions2 var' v1), fv2, conts2


    (*
       let v1 = cond in
       let k1 fv1 =
        let var = t in expr
       in
       let k2 fv2 =
        let var = t in expr
       in
        if v1 then k1 fv1 else k2 fv2
    *)
    | If (cond, t, f) ->
    let v1 = inc vars in
    let k0 = inc_conts () in
    let k1 = inc_conts () in
    let k2 = inc_conts () in
    let cps1, substitutions1, fv1, conts1 = to_cps (Let_cont (k0, var :: fv0, expr, conts)) fv0 t var (Apply_cont (k0, var :: fv0, [])) [] in
    let cps2, substitutions2, fv2, conts2 = to_cps conts1 fv0 f var (Apply_cont (k0, var :: fv0, [])) [] in
    let fv1' = List.filter (fun fv -> not (Env.has3 substitutions1 fv) || not (Env.has_var substitutions (Env.get_var substitutions1 fv))) fv1 in
    let fv2' = List.filter (fun fv -> not (Env.has3 substitutions2 fv) || not (Env.has_var substitutions (Env.get_var substitutions2 fv))) fv2 in
    let cps3, substitutions3, fv3, conts3 =
      to_cps (Let_cont
      (k1, fv1, cps1, Let_cont (k2, fv2, cps2, conts2))) fv0
        cond
        v1
        (If (v1, (k1, (List.map (fun fv -> if Env.has3 substitutions1 fv then let fval = Env.get_var substitutions1 fv in if Env.has_var substitutions fval then Env.get_value substitutions fval else fv else fv) fv1)), (k2, (List.map (fun fv -> if Env.has3 substitutions2 fv then let fval = Env.get_var substitutions2 fv in if Env.has_var substitutions fval then Env.get_value substitutions fval else fv else fv) fv2)), []))
        substitutions
    in
    cps3, substitutions1 @ substitutions2 @ substitutions3, fv1' @ fv2' @ fv3, conts3
  (*
     let k var [fv0] =
      expr
     in
      let v1 = e1 in
      let v2 = e2 in
      (v1 k v2)
  *)
  | App (e1, e2) ->
    let k = inc_conts () in
    let v1 = inc vars in
    let v2 = inc vars in
    let v3 = inc vars in
    let v4 = inc vars in
    let fv0 = remove_var fv0 var in
    let cps1, substitutions1, fv1, conts1 = to_cps (Let_cont (k, [var]@fv0, expr, conts)) (v1::fv0) e2 v2 (Let (v3, Get (v1, 0), Let (v4, Get (v1, 1), Call (v3, (List.map (fun fv -> if Env.has3 substitutions fv then let fval = Env.get_var substitutions fv in if Env.has_var substitutions fval then Env.get_value substitutions fval else fv else fv) [v4; v2]), [(k, fv0)])))) substitutions in
    let cps2, substitutions2, fv2, conts2 = to_cps conts1 (List.filter (fun fv -> not (fv = v1)) fv1) e1 v1 cps1 substitutions1 in
    cps2, substitutions2, fv2, conts2
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
  | Let_rec (var, e1, e2) ->
    let e1', substitutions', fvs' = alpha_conversion fvs e1 substitutions in
    let var', substitutions'' = add_subs substitutions' var in
    let fvs'' = add_fv fvs' (var, var') in
    let e2', substitutions''', fvs''' = alpha_conversion fvs'' e2 substitutions'' in
    Let (get_subst substitutions'' var, e1', e2'), substitutions''', unfree fvs''' var
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
  | If (cond, t, f) ->
    Format.fprintf fmt "(if %a = 0 then %a else %a)" (pp_expr_int subs) cond (pp_expr_int subs) t (pp_expr_int subs) f
  | App (e1, e2) -> Format.fprintf fmt "(%a %a)" (pp_expr_int subs) e1 (pp_expr_int subs) e2
  | _ -> failwith "invalid args"
;;

let print_expr_int e subs = (pp_expr_int subs) Format.std_formatter e
type var = string

type prim =
  | Add
  | Const of int
  | Print

type expr =
  | Var of var
  | Let of var * expr * expr
  | Fun of var * expr
  | App of expr * expr
  | Prim of Cps.prim * expr list
  | If of expr * expr * expr

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

let rec replace_var var new_var (ast : expr) : expr =
  match ast with
  | Fun (x, e) when x = var -> Fun (x, e)
  | Fun (x, e) -> Fun (x, replace_var var new_var e)
  | Var x when x = var -> Var new_var
  | Var x -> Var x
  | Prim (prim, args) ->
    Prim (prim, List.map (fun arg -> replace_var var new_var arg) args)
  | Let (var', e1, e2) when var' = var -> Let (var', replace_var var new_var e1, e2)
  | Let (var', e1, e2) ->
    Let (var', replace_var var new_var e1, replace_var var new_var e2)
  | If (cond, t, f) ->
    If (replace_var var new_var cond, replace_var var new_var t, replace_var var new_var f)
  | App (e1, e2) -> App (replace_var var new_var e1, replace_var var new_var e2)
;;

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

let rec to_cps conts fv0 (ast : expr) var (expr : Cps.expr) (substitutions : (string * int) list)
  : Cps.expr * (string * int) list * int list * Cps.cont
  =
  match ast with
  | Fun (x, e) ->
    let k1 = inc_conts () in
    let v1 = inc vars in
    let v2 = inc vars in
    let v3 = inc vars in
    let v4 = inc vars in
    let v6 = inc vars in
    let v7 = inc vars in
    let cps1, substitutions1, fv, conts1 =
      to_cps conts [v6; v7] e v2 (Apply_cont (v6, [v7; v2])) []
    in
    Env.print_subs substitutions1;
    Env.print_fv fv;
    let v5 = if Env.has substitutions1 x then Env.get substitutions1 x else inc vars in
    let fv =  (List.filter (fun fv' -> fv' != v5  && fv' != v6 && fv' != v7) fv) in
    let _, body = List.fold_left (fun (pos, cps') fv' -> pos + 1, Cps.Let (fv', Cps.Get (v1, pos), cps')) (0, cps1) fv in
    Let (v3, Prim (Const k1, []),
      Let (v4, Tuple fv,
        Let (var, Tuple [v3; v4], expr))), substitutions1 @ substitutions, (List.filter (fun fv' -> fv' != var) fv) @ fv0, (Let_cont (k1, [v6; v7; v1; v5], body, conts1))
  (*
      let var = x in (expr var fv...)
  *)
  | Var x ->
    if Env.has substitutions x
    then Let (var, Var (get_subs substitutions x), expr), substitutions, List.filter (fun fv' -> fv' != var) fv0, conts
    else (
      let v1 = inc vars in
      Let (var, Var v1, expr), ( x, v1 )::substitutions, v1::(List.filter (fun fv' -> fv' != var) fv0), conts)
  
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
      (Let (var, Prim (prim, List.map (fun (var, _) -> var) vars), expr), substitutions, fv0, conts)
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
    let v2 = inc vars in
    let v3 = inc vars in
    let v4 = inc vars in

    let k0 = inc_conts () in
    let k1 = inc_conts () in
    let k2 = inc_conts () in
    let cps1, substitutions1, fv1, conts1 = to_cps (Let_cont (k0, var :: fv0, expr, conts)) fv0 t var (Let (v2, Prim (Const k0, []), Apply_cont (v2, var :: fv0))) [] in
    let cps2, substitutions2, fv2, conts2 = to_cps conts1 fv0 f var (Let (v2, Prim (Const k0, []), Apply_cont (v2, var :: fv0))) [] in
    let fv1' = List.filter (fun fv -> not (Env.has3 substitutions1 fv) || not (Env.has_var substitutions (Env.get_var substitutions1 fv))) fv1 in
    let fv2' = List.filter (fun fv -> not (Env.has3 substitutions2 fv) || not (Env.has_var substitutions (Env.get_var substitutions2 fv))) fv2 in
    let cps3, substitutions3, fv3, conts3 =
      to_cps (Let_cont
      (k1, fv1, cps1, Let_cont (k2, fv2, cps2, conts2))) fv0
        cond
        v1
        ((Let (v3, Prim (Const k1, []), (Let (v4, Prim (Const k2, []), If (v1, (v3, (List.map (fun fv -> if Env.has3 substitutions1 fv then let fval = Env.get_var substitutions1 fv in if Env.has_var substitutions fval then Env.get_value substitutions fval else fv else fv) fv1)), (v4, (List.map (fun fv -> if Env.has3 substitutions2 fv then let fval = Env.get_var substitutions2 fv in if Env.has_var substitutions fval then Env.get_value substitutions fval else fv else fv) fv2))))))))
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
    let v5 = inc vars in
    let v6 = inc vars in
    let _, body = List.fold_left (fun (pos, cps') fv' -> pos + 1, Cps.Let (fv', Cps.Get (v1, pos), cps')) (0, expr) fv0 in
    let cps1, substitutions1, fv1, conts1 = to_cps (Let_cont (k, [v1; var], body, conts)) (v1::fv0) e2 v2 (Let (v3, Get (v1, 0), Let (v4, Get (v1, 1), Let (v5, Prim (Const k, []), Let (v6, Tuple (fv0), Apply_cont (v3, (List.map (fun fv -> if Env.has3 substitutions fv then let fval = Env.get_var substitutions fv in if Env.has_var substitutions fval then Env.get_value substitutions fval else fv else fv) [v5; v6; v4; v2]))))))) substitutions in
    let cps2, substitutions2, fv2, conts2 = to_cps conts1 (List.filter (fun fv -> not (fv = v1)) fv1) e1 v1 cps1 substitutions1 in
    cps2, substitutions2, fv2, conts2
;;

let rec from_cps_named (named : Cps.named) : expr =
  match named with
  | Prim (prim, args) ->
    Prim (prim, List.map (fun arg -> Var ("x" ^ string_of_int arg)) args)
  | Var x -> Var ("x" ^ string_of_int x)
  | _ -> failwith ""

and from_cps (cps : Cps.expr) : expr =
  match cps with
  | Let (var, named, expr) ->
    Let ("x" ^ string_of_int var, from_cps_named named, from_cps expr)
  | Apply_cont (_, [ arg ]) -> Var ("x" ^ string_of_int arg)
  | Apply_cont (k, _) -> App (Var ("k" ^ string_of_int k), Var "x0")
  | If (var, (kt, _), (kf, _)) ->
    If
      ( Var ("x" ^ string_of_int var)
      , App (Var ("k" ^ string_of_int kt), Prim (Const 0, []))
      , App (Var ("k" ^ string_of_int kf), Prim (Const 0, [])) )
  | Return x -> Var (string_of_int x)

and from_cps_cont (cps : Cps.cont) : expr =
  match cps with
  | Let_cont (k, [ arg ], e1, e2) ->
    Let ("k" ^ string_of_int k, Fun ("x" ^ string_of_int arg, from_cps e1), from_cps_cont e2)
  | Let_cont (k, _, e1, e2) ->
    Let ("k" ^ string_of_int k, Fun ("x", from_cps e1), from_cps_cont e2)
  | End -> Prim (Print, [])
;;

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
  | Prim of Cps2.prim * expr list
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

let rec to_cps fv0 (ast : expr) var (expr : Cps2.expr) (substitutions : (string * int) list)
  : Cps2.expr * (string * int) list * int list
  =
  match ast with
  | Fun (x, e) ->
    let k1 = inc_conts () in
    let v1 = inc vars in
    let v2 = inc vars in
    let cps1, substitutions1, fv =
      to_cps fv0 e v2 (Return v2) (add_subs substitutions x v1)
    in
    Let (var, Fun (v1, cps1, K k1), expr), substitutions1, fv
  | Var x ->
    if Env.has substitutions x
    then Let (var, Var (get_subs substitutions x), expr), [], []
    else (
      let v1 = inc vars in
      Let (var, Var v1, expr), [ x, v1 ], [ v1 ])
  | Prim (prim, args) ->
    let vars = List.map (fun arg -> inc vars, arg) args in
    List.fold_left
      (fun (expr, substitutions', fv') (var, e) ->
        let cps1, substitutions1, fv1 = to_cps fv0 e var expr substitutions in
        cps1, substitutions1 @ substitutions', fv1 @ fv')
      (Let (var, Prim (prim, List.map (fun (var, _) -> var) vars), expr), [], [])
      vars
  | Let (x1, Let (x2, e2, e2'), e1') ->
    let cps1, substitutions', fv =
      to_cps fv0 (Let (x2, e2, Let (x1, e2', e1'))) var expr substitutions
    in
    cps1, substitutions', fv
  | Let (x, Var x', e) ->
    if Env.has substitutions x'
    then (
      let cps1, substitutions', fv =
        to_cps fv0 e var expr (add_subs substitutions x (get_subs substitutions x'))
      in
      cps1, add_subs substitutions' x (get_subs substitutions x'), fv)
    else (
      let v1 = inc vars in
      let cps1, substitutions', fv = to_cps fv0 e var expr (add_subs substitutions x v1) in
      cps1, add_subs substitutions' x v1, v1 :: fv)
  (*
     let k1 v =
      let var = suite[x/v] in expr
     in
      let v1 = e1 in
      let v2 = e2 in
      (v1 k1 v2)
  *)
  | Let (x, App (e1, e2), suite) ->
    let v = inc vars in
    let v1 = inc vars in
    let v2 = inc vars in
    let k1 = inc_conts () in
    let cps1, substitutions1, fv1 = to_cps fv0 suite var expr (add_subs substitutions x v) in
    let cps2, substitutions2, fv2 = to_cps fv0 e2 v2 (Apply (v1, v2, K k1)) substitutions in
    let cps3, substitutions3, fv3 = to_cps fv0 e1 v1 cps2 substitutions in
    ( Let_cont (K k1, [ v ], cps1, cps3)
    , add_subs (substitutions1 @ substitutions2 @ substitutions3) x v, fv1 @fv2 @ fv3 )
    (*
       let v2 = cond in
       let k1 fv1 =
        let v1 = t in
        let var = e in expr
       in
       let k2 fv2 =
        let v2 = t in
        let var = e in expr
       in
        if v2 then k1 fv1 else k2 fv2


       let v2 = cond in
       let k0 fv =
        let var e in expr
       let k1 fv1 =
        let v1 = t in
        k0 ??
       in
       let k2 fv2 =
        let v2 = t in
        k0 ??
       in
        if v2 then k1 fv1 else k2 fv2
    *)
  | Let (var', If (cond, t, f), e) ->
    let v1 = inc vars in
    let cps1, substitutions1, fv1 = to_cps fv0 e var expr (add_subs [] var' v1) in
    (* let cps2, substitutions2, fv2 =  *)
    
    let k0 = inc_conts () in


    let v2 = inc vars in
    let k1 = inc_conts () in
    let k2 = inc_conts () in
    let cps3, substitutions3, fv3 = to_cps fv0 t v1 (Apply_cont (K k0, v1 :: fv1)) [] in
    let cps4, substitutions4, fv4 = to_cps fv0 f v1 (Apply_cont (K k0, v1 :: fv1)) [] in
    let fv3' = List.filter (fun fv -> not (Env.has_var substitutions (Env.get_var (substitutions3 @ substitutions1) fv))) (fv3 @ fv1) in
    let fv4' = List.filter (fun fv -> not (Env.has_var substitutions (Env.get_var (substitutions4 @ substitutions1) fv))) (fv4 @ fv1) in
    let cps5, substitutions5, fv5 =
      to_cps fv0
        cond
        v2
        (Let_cont
           (K k1, (fv3 @ fv1), cps3, Let_cont (K k2, (fv4 @ fv1), cps4, If (v2, (K k1, (List.map (fun fv -> let fval = Env.get_var (substitutions3 @ substitutions1) fv in if Env.has_var substitutions fval then Env.get_value substitutions fval else fv) (fv3 @ fv1))), (K k2, (List.map (fun fv -> let fval = Env.get_var (substitutions4 @ substitutions1) fv in if Env.has_var substitutions fval then Env.get_value substitutions fval else fv) (fv4 @ fv1)))))))
        substitutions
    in
    Let_cont (K k0, v1 :: fv1, cps1, cps5), add_subs (substitutions1 @ substitutions3 @ substitutions4 @ substitutions5) var' v1, fv3' @ fv4' @ fv5
    

    
  (* in *)
    
    
    (* cps2, add_subs (substitutions1 @ substitutions2) var' v1, fv2 *)




  (*
     let v0 = fun k1 v1 ->
      let v2 = e in (k1 v2)
     in
     let var = e2 in expr


     let v0 = fun v1 ->
      let v2 = e in v2
     in
     let var = e2 in expr
  *)
    | Let (var', Fun (x, e), e2) ->
    let v0 = inc vars in
    let k1 = inc_conts () in
    let v1 = inc vars in
    let v2 = inc vars in
    let cps1, substitutions1, fv1 =
      to_cps fv0
        e
        v2
        (Return v2)
        (add_subs (add_subs substitutions x v1) var' v0)
    in
    let cps2, substitutions2, fv2 = to_cps fv0 e2 var expr (add_subs substitutions var' v0) in
    ( Let (v0, Fun (v1, cps1, K k1), cps2)
    , add_subs (add_subs (substitutions1 @ substitutions2) x v1) var' v0, fv1 @ fv2 )
  | Let (var', e1, e2) ->
    let v1 = inc vars in
    let cps1, substitutions1, fv1 = to_cps fv0 e2 var expr (add_subs substitutions var' v1) in
    let cps2, substitutions2, fv2 = to_cps fv0 e1 v1 cps1 substitutions in
    cps2, add_subs (substitutions1 @ substitutions2) var' v1, fv1 @ fv2
    (*
       let v1 = cond in
       let k1 fv1 =
        let var = t in expr
       in
       let k2 fv2 =
        let var = t in expr
       in
        if v1 then k1 fv1 else k2 fv2
    
  | If (cond, t, f) ->
    let v1 = inc vars in
    let k1 = inc_conts () in
    let k2 = inc_conts () in
    let cps1, substitutions1, fv1 = to_cps fv0 t var expr [] in
    let cps2, substitutions2, fv2 = to_cps fv0 f var expr [] in
    let fv1' = List.filter (fun fv -> not (Env.has_var substitutions (Env.get_var substitutions1 fv))) fv1 in
    let fv2' = List.filter (fun fv -> not (Env.has_var substitutions (Env.get_var substitutions2 fv))) fv2 in
    let cps3, substitutions3, fv3 =
      to_cps fv0
        cond
        v1
        (Let_cont
           (K k1, fv1, cps1, Let_cont (K k2, fv2, cps2, If (v1, (K k1, (List.map (fun fv -> let fval = Env.get_var substitutions1 fv in if Env.has_var substitutions fval then Env.get_value substitutions fval else fv) fv1)), (K k2, (List.map (fun fv -> let fval = Env.get_var substitutions2 fv in if Env.has_var substitutions fval then Env.get_value substitutions fval else fv) fv2))))))
        substitutions
    in
    cps3, substitutions1 @ substitutions2 @ substitutions3, fv1' @ fv2' @ fv3*)
    | If (cond, t, f) ->
    let v1 = inc vars in
    let k1 = inc_conts () in
    let k2 = inc_conts () in
    let k0 = inc_conts () in

    let cps1, substitutions1, fv1 = to_cps fv0 t var (Return var) [] in
    let cps2, substitutions2, fv2 = to_cps fv0 f var (Return var) [] in
    let fv1' = List.filter (fun fv -> not (Env.has_var substitutions (Env.get_var substitutions1 fv))) fv1 in
    let fv2' = List.filter (fun fv -> not (Env.has_var substitutions (Env.get_var substitutions2 fv))) fv2 in
    let cps3, substitutions3, fv3 =
      to_cps fv0
        cond
        v1
        (Let_cont
           (K k1, fv0 @ fv1, cps1, Let_cont (K k2, fv0 @ fv2, cps2, If (v1, (K k1, fv0 @ (List.map (fun fv -> let fval = Env.get_var substitutions1 fv in if Env.has_var substitutions fval then Env.get_value substitutions fval else fv) fv1)), (K k2, fv0 @ (List.map (fun fv -> let fval = Env.get_var substitutions2 fv in if Env.has_var substitutions fval then Env.get_value substitutions fval else fv) fv2))))))
        substitutions
    in
    Let_cont (K k0, v1 :: fv1, cps1, cps3), substitutions1 @ substitutions2 @ substitutions3, fv1' @ fv2' @ fv3
    (*| App (e1, If (cond, t, f)) ->
      let k = inc_conts () in
      let v1 = inc vars in
      let v2 = inc vars in
      let cps1, substitutions1, fv1 = 
      
      

      let v3 = inc vars in
      let k1 = inc_conts () in
      let k2 = inc_conts () in
      let cps1, substitutions1, fv1 = to_cps fv0 t v2 (Apply (v1, v2, K k)) [] in
      let cps2, substitutions2, fv2 = to_cps fv0 f v2 (Apply (v1, v2, K k)) [] in
      let fv1' = List.filter (fun fv -> not (Env.has_var substitutions (Env.get_var substitutions1 fv))) fv1 in
      let fv2' = List.filter (fun fv -> not (Env.has_var substitutions (Env.get_var substitutions2 fv))) fv2 in
      let cps3, substitutions3, fv3 =
        to_cps fv0
          cond
          v3
          (Let_cont
             (K k1, v1 :: fv1, cps1, Let_cont (K k2, v1 :: fv2, cps2, If (v3, (K k1, v1 :: (List.map (fun fv -> let fval = Env.get_var substitutions1 fv in if Env.has_var substitutions fval then Env.get_value substitutions fval else fv) fv1)), (K k2, v1 :: (List.map (fun fv -> let fval = Env.get_var substitutions2 fv in if Env.has_var substitutions fval then Env.get_value substitutions fval else fv) fv2))))))
          substitutions
      in
      cps3, substitutions1 @ substitutions2 @ substitutions3, fv1' @ fv2' @ fv3
    
    in
      let cps2, substitutions2, fv2 = to_cps fv0 e1 v1 cps1 substitutions in
      Let_cont (K k, [ var ], expr, cps2), substitutions1 @ substitutions2, fv1 @ fv2
      *)
  (*
     let k var =
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
    let cps1, substitutions1, fv1 = to_cps fv0 e2 v2 (Apply (v1, v2, K k)) substitutions in
    let cps2, substitutions2, fv2 = to_cps fv0 e1 v1 cps1 substitutions in
    Let_cont (K k, [ var ], expr, cps2), substitutions1 @ substitutions2, fv1 @ fv2
;;

let rec from_cps_named (named : Cps2.named) : expr =
  match named with
  | Prim (prim, args) ->
    Prim (prim, List.map (fun arg -> Var ("x" ^ string_of_int arg)) args)
  | Fun (arg, expr, K _) -> Fun ("x" ^ string_of_int arg, from_cps expr)
  | Var x -> Var ("x" ^ string_of_int x)

and from_cps (cps : Cps2.expr) : expr =
  match cps with
  | Let (var, named, expr) ->
    Let ("x" ^ string_of_int var, from_cps_named named, from_cps expr)
  | Let_cont (K k, [ arg ], e1, e2) ->
    Let ("k" ^ string_of_int k, Fun ("x" ^ string_of_int arg, from_cps e1), from_cps e2)
  | Let_cont (K k, _, e1, e2) ->
    Let ("k" ^ string_of_int k, Fun ("x", from_cps e1), from_cps e2)
  | Apply_cont (K _, [ arg ]) -> Var ("x" ^ string_of_int arg)
  | Apply_cont (K k, _) -> App (Var ("k" ^ string_of_int k), Var "x0")
  | If (var, (K kt, _), (K kf, _)) ->
    If
      ( Var ("x" ^ string_of_int var)
      , App (Var ("k" ^ string_of_int kt), Prim (Const 0, []))
      , App (Var ("k" ^ string_of_int kf), Prim (Const 0, [])) )
  | Apply (x, arg, K k) ->
    App
      ( Var ("k" ^ string_of_int k)
      , App (Var ("x" ^ string_of_int x), Var ("x" ^ string_of_int arg)) )
  | Return x -> Var (string_of_int x)
;;

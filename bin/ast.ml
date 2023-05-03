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
  string_of_int !vars
;;

let inc_conts () =
  conts := !conts + 1;
  !conts
;;

let rec to_cps (ast : expr) var (expr : Cps.expr) : Cps.expr =
  match ast with
  (* | Fun (x, e) ->
    let k1 = inc_conts () in
    let v1 = inc vars in
    Let_cont (K k1, [v1], replace_var x v1 e, expr) *)
  | Fun (x, e) ->
    let k1 = inc_conts () in
    let v1 = inc vars in
    let v2 = inc vars in
    Let
      ( var
      , Fun (v1, to_cps (replace_var x v1 e) v2 (Apply_cont (K k1, [ v2 ])), K k1)
      , expr )
  | Var x -> Cps.replace_var var x expr
  | Prim (prim, args) ->
    let vars = List.map (fun arg -> inc vars, arg) args in
    List.fold_left
      (fun expr (var, e) -> to_cps e var expr)
      (Let (var, Prim (prim, List.map (fun (var, _) -> var) vars), expr))
      vars
  | Let (x1, Let (x2, e2, e2'), e1') -> to_cps (Let (x2, e2, Let (x1, e2', e1'))) var expr
  | Let (x, Var x', e) -> to_cps (replace_var x x' e) var expr
  | Let (x, App (e1, e2), suite) ->
    let v = inc vars in
    let v1 = inc vars in
    let v2 = inc vars in
    let k1 = inc_conts () in
    Let_cont
      ( K k1
      , [ v ]
      , to_cps (replace_var x v suite) var expr
      , to_cps e1 v1 (to_cps e2 v2 (Apply (v1, v2, K k1))) )
  | Let (var', If (cond, t, f), e) ->
    let v1 = inc vars in
    to_cps (If (cond, t, f)) v1 (to_cps (replace_var var' v1 e) var expr)
  | Let (var', Fun (x, e), e2) ->
    let v0 = inc vars in


    let k1 = inc_conts () in
    let v1 = inc vars in
    let v2 = inc vars in
    Let
      ( v0
      , Fun (v1, to_cps (replace_var var' v0 (replace_var x v1 e)) v2 (Apply_cont (K k1, [ v2 ])), K k1)
      , (to_cps (replace_var var' v0 e2) var expr) )
  | Let (var', e1, e2) ->
    let v1 = inc vars in
    to_cps e1 v1 (to_cps (replace_var var' v1 e2) var expr)
  | If (cond, t, f) ->
    let v1 = inc vars in
    let k1 = inc_conts () in
    let k2 = inc_conts () in
    to_cps
      cond
      v1
      (Let_cont
         ( K k1
         , []
         , to_cps t var expr
         , Let_cont (K k2, [], to_cps f var expr, If (v1, (K k1, []), (K k2, []))) ))
  | App (e1, e2) ->
    let k = inc_conts () in
    let v1 = inc vars in
    let v2 = inc vars in
    Let_cont (K k, [ var ], expr, to_cps e1 v1 (to_cps e2 v2 (Apply (v1, v2, K k))))
;;

let rec from_cps_named (named : Cps.named) : expr =
  match named with
  | Prim (prim, args) -> Prim (prim, List.map (fun arg -> Var ("x" ^ arg)) args)
  | Fun (arg, expr, K _) -> Fun ("x" ^ arg, from_cps expr)

and from_cps (cps : Cps.expr) : expr =
  match cps with
  | Let (var, named, expr) -> Let ("x" ^ var, from_cps_named named, from_cps expr)
  | Let_cont (K k, [ arg ], e1, e2) ->
    Let ("k" ^ string_of_int k, Fun ("x" ^ arg, from_cps e1), from_cps e2)
  | Let_cont (K k, _, e1, e2) ->
    Let ("k" ^ string_of_int k, Fun ("x", from_cps e1), from_cps e2)
  | Apply_cont (K _, [ arg ]) -> Var ("x" ^ arg)
  | Apply_cont (K k, _) -> App (Var ("k" ^ string_of_int k), Var "x0")
  | If (var, (K kt, _), (K kf, _)) ->
    If
      ( Var var
      , App (Var ("k" ^ string_of_int kt), Prim (Const 0, []))
      , App (Var ("k" ^ string_of_int kf), Prim (Const 0, [])) )
  | Apply (x, arg, K k) ->
    App (Var ("k" ^ string_of_int k), App (Var ("x" ^ x), Var ("x" ^ arg)))
  | Return x -> Var x
;;

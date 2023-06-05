type var = string

type prim =
  | Add
  | Const of int
  | Print

type match_pattern =
| Int of int
| Deconstructor of string * string list
| Joker of string

type 'var expr =
  | Var of 'var
  | Constructor of string
  | Let of 'var * 'var expr * 'var expr
  | Let_rec of ('var * 'var expr) list * 'var expr
  | Fun of 'var * 'var expr
  | App of 'var expr * 'var expr
  | Prim of Cps.prim * 'var expr list
  | If of 'var expr * 'var expr * 'var expr
  | Match of 'var expr * (match_pattern * 'var expr) list
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


module Constructors = Map.Make (String)

let rec pattern_to_cst pattern: Cst.match_pattern =
  match pattern with
  | Int x -> Int x
  | Deconstructor _ -> Joker
  | Joker _ -> Joker

and expr_to_cst (expr: var expr) (constructors: (int) Constructors.t): Cst.var Cst.expr =
  match expr with
  | Fun (x, e) -> Fun (x, expr_to_cst e constructors)
  | Var x -> Var x
  | Constructor str -> Prim (Const (Constructors.find str constructors), [])
  | Prim (prim, args) -> Prim (prim, List.map (fun e' -> expr_to_cst e' constructors) args)
  | Let (var, e1, e2) -> Let (var, expr_to_cst e1 constructors, expr_to_cst e2 constructors)
  | If (cond, t, f) -> If (expr_to_cst cond constructors, expr_to_cst t constructors, expr_to_cst f constructors)
  | App (e1, e2) -> App (expr_to_cst e1 constructors, expr_to_cst e2 constructors)
  | Let_rec (bindings, e) -> Let_rec (List.map (fun (x, e') -> x, expr_to_cst e' constructors) bindings, expr_to_cst e constructors)
  | Match (x, branchs) -> Match (expr_to_cst x constructors, (List.map (fun (x, e') -> pattern_to_cst x, expr_to_cst e' constructors)) branchs)
;;
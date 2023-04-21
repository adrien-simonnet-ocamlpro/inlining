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
  | Prim of prim * expr list
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

let print_expr e = pp_expr Format.std_formatter e

let sprintf e =
  Format.asprintf "%a" pp_expr e

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

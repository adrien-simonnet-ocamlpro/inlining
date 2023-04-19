type var = string
type prim = Add | Const of int | Print

type expr =
  | Var of var
  | Let of var * expr * expr
  | Fun of var * expr
  | App of expr * expr
  | Prim of prim * expr list
  | If of expr * expr * expr

let rec sprintf_prim (prim : prim) args =
  match (prim, args) with
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
      Printf.sprintf "(if %s then %s else %s)" (sprintf cond) (sprintf t)
        (sprintf f)
  | App (e1, e2) -> Printf.sprintf "(%s %s)" (sprintf e1) (sprintf e2)

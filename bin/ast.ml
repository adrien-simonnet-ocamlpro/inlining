type var = string

type prim =
  | Add
  | Const of int
  | Print

type match_pattern =
| Int of int
| Deconstructor of string * string list
| Joker of string

type expr =
  | Type of var * (var * var) list * expr
  | Var of var
  | Constructor of string * (expr list)
  | Let of var * expr * expr
  | Let_rec of (var * expr) list * expr
  | Fun of var * expr
  | App of expr * expr
  | Prim of Cps.prim * expr list
  | If of expr * expr * expr
  | Match of expr * (match_pattern * expr) list
  | Match_pattern of expr * (match_pattern * expr) list

let rec pp_expr fmt expr =
  match expr with
  | Fun (x, e) -> Format.fprintf fmt "(fun %s -> %a)" x pp_expr e
  | Var x -> Format.fprintf fmt "%s" x
  | Prim (Const x, _) -> Format.fprintf fmt "%d" x
  | Prim (Add, x1 :: x2 :: _) -> Format.fprintf fmt "(%a + %a)" pp_expr x1 pp_expr x2
  | Prim (Add, _) -> assert false
  | Prim (Sub, x1 :: x2 :: _) -> Format.fprintf fmt "(%a - %a)" pp_expr x1 pp_expr x2
  | Prim (Sub, _) -> assert false
  | Prim (Print, x1 :: _) -> Format.fprintf fmt "(print %a)" pp_expr x1
  | Prim (Print, _) -> assert false
  | Let (var, e1, e2) -> Format.fprintf fmt "(let %s = %a in\n%a)" var pp_expr e1 pp_expr e2
  | Let_rec (_bindings, expr) -> Format.fprintf fmt "(let rec in\n%a)" pp_expr expr
  | If (cond, t, f) ->
    Format.fprintf fmt "(if %a = 0 then %a else %a)" pp_expr cond pp_expr t pp_expr f
  | App (e1, e2) -> Format.fprintf fmt "(%a %a)" pp_expr e1 pp_expr e2
  | Type (name, _, expr) -> Format.fprintf fmt "type %s = %s %a" name "constructors" pp_expr expr
  | Constructor (_, _) -> Format.fprintf fmt "constructor"
  | Match (_, _) -> Format.fprintf fmt "match"
  | Match_pattern (_, _) -> Format.fprintf fmt "constructor"
;;

let print_expr e = pp_expr Format.std_formatter e

let sprintf e = Format.asprintf "%a" pp_expr e

module Constructors = Map.Make (String)

let rec pattern_to_cst pattern: Cst.match_pattern =
  match pattern with
  | Int x -> Int x
  | Deconstructor _ -> Joker
  | Joker _ -> Joker

and expr_to_cst (expr: expr) (constructors: int Constructors.t): Cst.expr =
  match expr with
  | Fun (x, e) -> Fun (x, expr_to_cst e constructors)
  | Var x -> Var x
  | Constructor (str, exprs) -> let index = Constructors.find str constructors in Constructor (index, List.map (fun expr -> expr_to_cst expr constructors) exprs)
  | Prim (prim, args) -> Prim (prim, List.map (fun e' -> expr_to_cst e' constructors) args)
  | Let (var, e1, e2) -> Let (var, expr_to_cst e1 constructors, expr_to_cst e2 constructors)
  | If (cond, t, f) -> If (expr_to_cst cond constructors, expr_to_cst t constructors, expr_to_cst f constructors)
  | App (e1, e2) -> App (expr_to_cst e1 constructors, expr_to_cst e2 constructors)
  | Let_rec (bindings, e) -> Let_rec (List.map (fun (x, e') -> x, expr_to_cst e' constructors) bindings, expr_to_cst e constructors)
  | Match (x, branchs) -> Match (expr_to_cst x constructors, (List.map (fun (x, e') -> pattern_to_cst x, expr_to_cst e' constructors)) branchs)
  | Match_pattern (x, branchs) -> 
    let default_expr = if List.exists (fun (t, _) -> match t with | Joker _ -> true | _ -> false) branchs then 

      let _, default_expr = List.find (fun (t, _) -> match t with
        | Joker _ -> true | _ -> false) branchs in default_expr

      else Prim (Print, [Prim (Const 123456789, [])]) in
    
    let branchs = List.map (fun (pattern, e') -> begin match pattern with
    | Deconstructor (constructor_name, payload_values) -> let pattern_index = Constructors.find constructor_name constructors in pattern_index, payload_values, expr_to_cst e' constructors
    | _ -> assert false
    end) (List.filter (fun (pattern, _) -> match pattern with | Deconstructor _ -> true | _ -> false) branchs) in
    

    Match_pattern (expr_to_cst x constructors, branchs, expr_to_cst default_expr constructors)


    | Type (_, constructors', expr) -> expr_to_cst expr (List.fold_left (fun constructors'' ((constructor_name, _), index) -> Constructors.add constructor_name index constructors'') constructors (List.mapi (fun i v -> v, i) constructors'))
;;
type kvar = K of int

type named =
  | Prim of Ast.prim * Ast.var list
  | Fun of Ast.var * expr * kvar
  | Var of Ast.var

and expr =
  | Let of Ast.var * named * expr
  | Let_cont of kvar * Ast.var list * expr * expr
  | Apply_cont of kvar * Ast.var list
  | If of Ast.var * (kvar * Ast.var list) * (kvar * Ast.var list)
  | Apply of Ast.var * Ast.var * kvar
  | Return of string

let rec replace_var_named var new_var (ast : named) : named =
  match ast with
  | Prim (prim, args) ->
    Prim (prim, List.map (fun arg -> if arg = var then new_var else arg) args)
  | Fun (x, e, k) when x = var -> Fun (x, e, k)
  | Fun (x, e, k) -> Fun (x, replace_var var new_var e, k)
  | Var x when x = var -> Var new_var
  | Var x -> Var x

and replace_var var new_var (ast : expr) : expr =
  match ast with
  | Let (var', e1, e2) when var' = var -> Let (var', replace_var_named var new_var e1, e2)
  | Let (var', e1, e2) ->
    Let (var', replace_var_named var new_var e1, replace_var var new_var e2)
  | If (cond, t, f) when cond = var -> If (new_var, t, f)
  | If (cond, t, f) -> If (cond, t, f)
  | Apply (v1, v2, k) when v1 = var && v2 = var -> Apply (new_var, new_var, k)
  | Apply (v1, v2, k) when v1 = var -> Apply (new_var, v2, k)
  | Apply (v1, v2, k) when v2 = var -> Apply (v1, new_var, k)
  | Apply (v1, v2, k) -> Apply (v1, v2, k)
  | Let_cont (k, args, e1, e2) ->
    Let_cont
      ( k
      , List.map (fun arg -> if arg = var then new_var else arg) args
      , (if List.exists (fun arg -> arg = var) args
         then e1
         else replace_var var new_var e1)
      , replace_var var new_var e2 )
  | Apply_cont (k, args) ->
    Apply_cont (k, List.map (fun arg -> if arg = var then new_var else arg) args)
  | Return x when x = var -> Return new_var
  | Return x -> Return x
;;

let rec replace_cont_named var new_var (ast : named) : named =
  match ast with
  | Prim (prim, args) -> Prim (prim, args)
  | Fun (x, e, K k) when k = var -> Fun (x, replace_cont var new_var e, K new_var)
  | Fun (x, e, K k) -> Fun (x, replace_cont var new_var e, K k)
  | Var x -> Var x

and replace_cont var new_var (ast : expr) : expr =
  match ast with
  | Let (var', e1, e2) ->
    Let (var', replace_cont_named var new_var e1, replace_cont var new_var e2)
  | If (cond, (K kt, argst), (K kf, argsf)) when kt = var && kf = var ->
    If (cond, (K new_var, argst), (K new_var, argsf))
  | If (cond, (K kt, argst), (K kf, argsf)) when kt = var ->
    If (cond, (K new_var, argst), (K kf, argsf))
  | If (cond, (K kt, argst), (K kf, argsf)) when kf = var ->
    If (cond, (K kt, argst), (K new_var, argsf))
  | If (cond, (K kt, argst), (K kf, argsf)) -> If (cond, (K kt, argst), (K kf, argsf))
  | Apply (v1, v2, K k) when k = var -> Apply (v1, v2, K new_var)
  | Apply (v1, v2, k) -> Apply (v1, v2, k)
  | Let_cont (K k, args, e1, e2) when k = var ->
    Let_cont (K new_var, args, replace_cont var new_var e1, replace_cont var new_var e2)
  | Let_cont (k, args, e1, e2) ->
    Let_cont (k, args, replace_cont var new_var e1, replace_cont var new_var e2)
  | Apply_cont (K k, args) when k = var -> Apply_cont (K new_var, args)
  | Apply_cont (k, args) -> Apply_cont (k, args)
  | Return x -> Return x
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

let rec from_ast (ast : Ast.expr) var (expr : expr) : expr =
  match ast with
  (* | Fun (x, e) ->
    let k1 = inc_conts () in
    let v1 = inc vars in
    Let_cont (K k1, [v1], Ast.replace_var x v1 e, expr) *)
  | Fun (x, e) ->
    let k1 = inc_conts () in
    let v1 = inc vars in
    let v2 = inc vars in
    Let
      ( var
      , Fun (v1, from_ast (Ast.replace_var x v1 e) v2 (Apply_cont (K k1, [ v2 ])), K k1)
      , expr )
  (* todo: Ast.replace_var var new_var cps *)
  | Var x -> Let (var, Var x, expr)
  | Prim (prim, args) ->
    let vars = List.map (fun arg -> inc vars, arg) args in
    List.fold_left
      (fun expr (var, e) -> from_ast e var expr)
      (Let (var, Prim (prim, List.map (fun (var, _) -> var) vars), expr))
      vars
  | Let (x1, Let (x2, e2, e2'), e1') ->
    from_ast (Ast.Let (x2, e2, Ast.Let (x1, e2', e1'))) var expr
  | Let (x, Var x', e) -> from_ast (Ast.replace_var x x' e) var expr
  | Let (x, App (e1, e2), suite) ->
    let v = inc vars in
    let v1 = inc vars in
    let v2 = inc vars in
    let k1 = inc_conts () in
    Let_cont
      ( K k1
      , [ v ]
      , from_ast (Ast.replace_var x v suite) var expr
      , from_ast e1 v1 (from_ast e2 v2 (Apply (v1, v2, K k1))) )
  | Let (var', If (cond, t, f), e) ->
    let v1 = inc vars in
    from_ast (If (cond, t, f)) v1 (from_ast (Ast.replace_var var' v1 e) var expr)
  | Let (var', e1, e2) ->
    let v1 = inc vars in
    from_ast e1 v1 (from_ast (Ast.replace_var var' v1 e2) var expr)
  | If (cond, t, f) ->
    let v1 = inc vars in
    let k1 = inc_conts () in
    let k2 = inc_conts () in
    from_ast
      cond
      v1
      (Let_cont
         ( K k1
         , []
         , from_ast t var expr
         , Let_cont (K k2, [], from_ast f var expr, If (v1, (K k1, []), (K k2, []))) ))
  | App (e1, e2) ->
    let k = inc_conts () in
    let v1 = inc vars in
    let v2 = inc vars in
    Let_cont (K k, [ var ], expr, from_ast e1 v1 (from_ast e2 v2 (Apply (v1, v2, K k))))
;;

let rec sprintf_named named =
  match named with
  | Prim (prim, args) -> sprintf_prim prim args
  | Fun (arg, expr, K k) -> Printf.sprintf "(fun k%d x%s -> %s)" k arg (sprintf expr)
  | Var x -> "x" ^ x

and sprintf_prim (prim : Ast.prim) args =
  match prim, args with
  | Const x, _ -> string_of_int x
  | Add, x1 :: x2 :: _ -> Printf.sprintf "(x%s + x%s)" x1 x2
  | Print, x1 :: _ -> Printf.sprintf "(print x%s)" x1
  | _ -> failwith "invalid args"

and sprintf (cps : expr) : string =
  match cps with
  | Let (var, named, expr) ->
    Printf.sprintf "let x%s = %s in\n%s" var (sprintf_named named) (sprintf expr)
  | Let_cont (K k, args, e1, e2) ->
    Printf.sprintf
      "let k%d%s = %s in\n%s"
      k
      (if List.length args > 0
       then List.fold_left (fun acc s -> acc ^ " x" ^ s) "" args
       else " ()")
      (sprintf e1)
      (sprintf e2)
  | Apply_cont (K k, args) ->
    Printf.sprintf
      "(k%d%s)"
      k
      (if List.length args > 0
       then List.fold_left (fun acc s -> acc ^ " x" ^ s) "" args
       else " ()")
  | If (var, (K kt, argst), (K kf, argsf)) ->
    Printf.sprintf
      "(if x%s = 0 then (k%d%s) else (k%d%s))"
      var
      kt
      (if List.length argst > 0
       then List.fold_left (fun acc s -> acc ^ " " ^ s) "" argst
       else " ()")
      kf
      (if List.length argsf > 0
       then List.fold_left (fun acc s -> acc ^ " " ^ s) "" argsf
       else " ()")
  | Apply (x, arg, K k) -> Printf.sprintf "((x%s k%d) x%s)" x k arg
  | Return x -> "x" ^ x
;;

let rec to_ast_named named : Ast.expr =
  match named with
  | Prim (prim, args) -> Ast.Prim (prim, List.map (fun arg -> Ast.Var ("x" ^ arg)) args)
  | Fun (arg, expr, K _) -> Ast.Fun ("x" ^ arg, to_ast expr)
  | Var x -> Ast.Var ("x" ^ x)

and to_ast (cps : expr) : Ast.expr =
  match cps with
  | Let (var, named, expr) -> Ast.Let ("x" ^ var, to_ast_named named, to_ast expr)
  | Let_cont (K k, [ arg ], e1, e2) ->
    Ast.Let ("k" ^ string_of_int k, Ast.Fun ("x" ^ arg, to_ast e1), to_ast e2)
  | Let_cont (K k, _, e1, e2) ->
    Ast.Let ("k" ^ string_of_int k, Ast.Fun ("x", to_ast e1), to_ast e2)
  | Apply_cont (K _, [ arg ]) -> Var ("x" ^ arg)
  | Apply_cont (K k, _) -> App (Ast.Var ("k" ^ string_of_int k), Var "x0")
  | If (var, (K kt, _), (K kf, _)) ->
    Ast.If
      ( Ast.Var var
      , App (Ast.Var ("k" ^ string_of_int kt), Prim (Const 0, []))
      , App (Ast.Var ("k" ^ string_of_int kf), Prim (Const 0, [])) )
  | Apply (x, arg, K k) ->
    App (Ast.Var ("k" ^ string_of_int k), App (Ast.Var ("x" ^ x), Ast.Var ("x" ^ arg)))
  | Return x -> Var x
;;

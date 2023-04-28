let has = Env.has
let has_cont = Env.has_cont
let get = Env.get
let get_cont = Env.get_cont

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

type 'a map = (string * 'a) list

type value =
  | Int of int
  | Fun of Ast.var * expr * kvar * value map

type env = value map

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

let rec propagation_prim (prim : Ast.prim) args (env : (Ast.var * value) list) : named =
  match prim, args with
  | Const x, args' -> Prim (Const x, args')
  | Add, x1 :: x2 :: _ ->
    if has env x1
    then (
      match (get env x1 : value) with
      | Int n1 ->
        if has env x2
        then (
          match get env x2 with
          | Int n2 -> Prim (Const (n1 + n2), [])
          | _ -> failwith "invalid type")
        else if n1 = 0
        then Var x2
        else Prim (Add, args)
      | _ -> failwith "invalid type")
    else Prim (Add, args)
  | Print, _ :: _ -> Prim (Print, args)
  | _ -> failwith "invalid args"

and propagation (cps : expr) env (conts : (int * Ast.var list * expr * env) list) : expr =
  match cps with
  | Let (var, Var var', expr) -> propagation (replace_var var var' expr) env conts
  | Let (var, Prim (prim, args), expr) ->
    (match propagation_prim prim args env with
     | Var var' -> propagation (replace_var var var' expr) env conts
     | Fun (arg, expr, k) ->
       Let
         ( var
         , Fun (arg, expr, k)
         , propagation expr ((var, Fun (arg, expr, k, env)) :: env) conts )
     | Prim (Const x, []) ->
       Let (var, Prim (Const x, []), propagation expr ((var, Int x) :: env) conts)
     | Prim (prim, args) -> Let (var, Prim (prim, args), propagation expr env conts))
  | Let (var, Fun (arg, e, k), expr) ->
    let e' = propagation e env conts in
    Let
      (var, Fun (arg, e', k), propagation expr ((var, Fun (arg, e', k, env)) :: env) conts)
  | Let_cont (K k', args', e1, e2) ->
    let e1' = propagation e1 env conts in
    let e2' = propagation e2 env ((k', args', e1', env) :: conts) in
    (match e1' with
     | Apply_cont (K k, [ arg ]) when [ arg ] = args' ->
       propagation (replace_cont k' k e2') env conts
     | _ -> Let_cont (K k', args', e1', e2'))
  | Apply_cont (K k, args) ->
    if has_cont conts k
    then
      if not (List.exists (fun arg -> not (has env arg)) args)
      then (
        let args', cont, env' = get_cont conts k in
        propagation
          (List.fold_left
             (fun cont (arg', arg) -> replace_var arg' arg cont)
             cont
             (List.map2 (fun arg' arg -> arg', arg) args' args))
          (List.map (fun arg -> arg, get env arg) args @ env')
          conts)
      else Apply_cont (K k, args)
    else Apply_cont (K k, args)
  | If (var, (K kt, argst), (K kf, argsf)) ->
    if has env var
    then (
      match get env var with
      | Int n -> if n = 0 then propagation (Apply_cont (K kt, argst)) env conts else propagation (Apply_cont (K kf, argsf)) env conts
      | _ -> failwith "invalid type")
    else If (var, (K kt, argst), (K kf, argsf))
  | Apply (x, arg, K k) ->
    if has env x
    then (
      match get env x with
      | Fun (arg', expr, K k', env') ->
        let args, cont, env'' = get_cont conts k in
        if has env arg
        then (
          let value = get env arg in
          propagation
            (replace_cont k' k (replace_var arg' arg expr))
            ((arg, value) :: env')
            ((k, args, cont, env'') :: conts))
        else Apply (x, arg, K k)
      | _ -> failwith "invalid type")
    else Apply (x, arg, K k)
  | Return x -> Return x
;;

let rec elim_unused_vars_named (vars : int array) (conts : int array) (named : named)
  : named
  =
  match named with
  | Prim (prim, args) ->
    List.iter
      (fun arg ->
        Array.set vars (int_of_string arg) (Array.get vars (int_of_string arg) + 1))
      args;
    Prim (prim, args)
  | Var x ->
    Array.set vars (int_of_string x) (Array.get vars (int_of_string x) + 1);
    Var x
  | Fun (v1, e, k) -> Fun (v1, elim_unused_vars vars conts e, k)

and elim_unused_vars (vars : int array) (conts : int array) (cps : expr) : expr =
  match cps with
  | Let (var, e1, e2) ->
    let e2' = elim_unused_vars vars conts e2 in
    if Array.get vars (int_of_string var) > 0
    then (
      let e1' = elim_unused_vars_named vars conts e1 in
      Let (var, e1', e2'))
    else e2'
  | Let_cont (K k', args', e1, e2) ->
    let e2' = elim_unused_vars vars conts e2 in
    if Array.get conts k' > 0
    then (
      let e1' = elim_unused_vars vars conts e1 in
      Let_cont (K k', args', e1', e2'))
    else e2'
  | Apply_cont (K k, args) ->
    Array.set conts k (Array.get vars k + 1);
    List.iter
      (fun arg ->
        Array.set vars (int_of_string arg) (Array.get vars (int_of_string arg) + 1))
      args;
    Apply_cont (K k, args)
  | If (var, (K kt, argst), (K kf, argsf)) ->
    Array.set vars (int_of_string var) (Array.get vars (int_of_string var) + 1);
    Array.set conts kt (Array.get vars kt + 1);
    Array.set conts kf (Array.get vars kf + 1);
    If (var, (K kt, argst), (K kf, argsf))
  | Apply (x, arg, K k) ->
    Array.set conts k (Array.get vars k + 1);
    Array.set vars (int_of_string x) (Array.get vars (int_of_string x) + 1);
    Array.set vars (int_of_string arg) (Array.get vars (int_of_string arg) + 1);
    Apply (x, arg, K k)
  | Return x ->
    Array.set vars (int_of_string x) (Array.get vars (int_of_string x) + 1);
    Return x
;;

let rec interp_prim var (prim : Ast.prim) args (env : (Ast.var * value) list) =
  match prim, args with
  | Const x, _ -> [ var, Int x ]
  | Add, x1 :: x2 :: _ ->
    (match (get env x1 : value) with
     | Int n1 ->
       (match get env x2 with
        | Int n2 -> [ var, Int (n1 + n2) ]
        | _ -> failwith "invalid type")
     | _ -> failwith "invalid type")
  | Print, x1 :: _ ->
    (match (get env x1 : value) with
     | Int n ->
       Printf.printf "%d\n" n;
       []
     | _ -> failwith "invalid type")
  | _ -> failwith "invalid args"

and interp_named var (named : named) (env : (Ast.var * value) list) =
  match named with
  | Prim (prim, args) -> interp_prim var prim args env
  | Fun (arg, expr, k) -> [ var, Fun (arg, expr, k, env) ]
  | Var x -> [ var, get env x ]

and interp (cps : expr) (env : env) (conts : (int * Ast.var list * expr * env) list) =
  try
    match cps with
    | Let (var, named, expr) -> interp expr (interp_named var named env @ env) conts
    | Let_cont (K k', args', e1, e2) -> interp e2 env ((k', args', e1, env) :: conts)
    | Apply_cont (K k, _) when k = 0 -> env
    | Apply_cont (K k', args) ->
      let args', cont, env' = get_cont conts k' in
      interp cont (List.map2 (fun arg' arg -> arg', get env arg) args' args @ env') conts
    | If (var, (K kt, argst), (K kf, argsf)) ->
      (match get env var with
       | Int n ->
         if n = 0
         then interp (Apply_cont (K kt, argst)) env conts
         else interp (Apply_cont (K kf, argsf)) env conts
       | _ -> failwith "invalid type")
    | Apply (x, arg, K k) ->
      (match get env x with
       | Fun (arg', expr, K k', env') ->
         let value = get env arg in
         let args, cont, env'' = get_cont conts k in
         interp expr ((arg', value) :: env') ((k', args, cont, env'') :: conts)
       | _ -> failwith "invalid type")
    | Return _ -> env
  with
  | Failure str -> failwith (Printf.sprintf "%s\n%s" str (sprintf cps))
;;

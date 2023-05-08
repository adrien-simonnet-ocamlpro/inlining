let has = Env.has

let get = Env.get2

type var = int
type kvar = K of int

type prim =
  | Add
  | Const of int
  | Print

type named =
  | Prim of prim * var list
  | Fun of var * expr * kvar
  | Var of var

and expr =
  | Let of var * named * expr
  | Apply_cont of kvar * var list
  | If of var * (kvar * var list) * (kvar * var list)
  | Apply of var * var * (kvar * var list)
  | Return of var

type cont =
| Let_cont of kvar * var list * expr * cont
| End

let rec has_cont cont k =
  match cont with
  | Let_cont (K k', _, _, _) when k = k' -> true
  | Let_cont (_, _, _, e2) -> has_cont e2 k
  | End -> false

let rec get_cont cont k =
match cont with
| Let_cont (K k', args, e1, _) when k = k' -> (args, e1, [])
| Let_cont (_, _, _, e2) -> get_cont e2 k
| End -> failwith "cont not found"

type 'a map = (var * 'a) list

type value =
  | Int of int
  | Fun of var * expr * kvar * value map

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
  | Apply_cont (k, args) ->
    Apply_cont (k, List.map (fun arg -> if arg = var then new_var else arg) args)
  | Return x when x = var -> Return new_var
  | Return x -> Return x

  and replace_var_cont var new_var (ast : cont) : cont =
  match ast with
  | Let_cont (k, args, e1, e2) ->
    Let_cont
      ( k
      , List.map (fun arg -> if arg = var then new_var else arg) args
      , (if List.exists (fun arg -> arg = var) args
         then e1
         else replace_var var new_var e1)
      , replace_var_cont var new_var e2 )
  | End -> End
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
  | Apply (v1, v2, (K k, args)) when k = var -> Apply (v1, v2, (K new_var, args))
  | Apply (v1, v2, k) -> Apply (v1, v2, k)
  | Apply_cont (K k, args) when k = var -> Apply_cont (K new_var, args)
  | Apply_cont (k, args) -> Apply_cont (k, args)
  | Return x -> Return x

  and replace_cont_cont var new_var (ast : cont) : cont =
  match ast with
  | Let_cont (K k, args, e1, e2) when k = var ->
    Let_cont (K new_var, args, replace_cont var new_var e1, replace_cont_cont var new_var e2)
  | Let_cont (k, args, e1, e2) ->
    Let_cont (k, args, replace_cont var new_var e1, replace_cont_cont var new_var e2)
  | End -> End
;;

let gen_name id env =
  match Env.get_name id env with
  | Some (v, _) -> v ^ "_" ^ (string_of_int id)
  | None -> "_" ^ (string_of_int id)

let rec sprintf_named2 named subs =
  match named with
  | Prim (prim, args) -> sprintf_prim2 prim args subs
  | Fun (arg, expr, _) -> Printf.sprintf "(fun %s ->\n%s)" (gen_name arg subs) (sprintf expr subs)
  | Var x -> gen_name x subs

and sprintf_prim2 (prim : prim) args subs =
  match prim, args with
  | Const x, _ -> string_of_int x
  | Add, x1 :: x2 :: _ -> Printf.sprintf "%s + %s" (gen_name x1 subs) (gen_name x2 subs)
  | Print, x1 :: _ -> Printf.sprintf "print %s" (gen_name x1 subs)
  | _ -> failwith "invalid args"

and sprintf (cps : expr) subs : string =
  match cps with
  | Let (var, named, expr) ->
    Printf.sprintf "\tlet %s = %s in\n%s" (gen_name var subs) (sprintf_named2 named subs) (sprintf expr subs)
  | Apply_cont (K k, args) ->
    Printf.sprintf
      "\tk%d%s"
      k
      (if List.length args > 0
       then List.fold_left (fun acc s -> acc ^ " " ^ (gen_name s subs)) "" args
       else " ()")
  | If (var, (K kt, argst), (K kf, argsf)) ->
    Printf.sprintf
      "\tif %s = 0 then k%d%s else k%d%s"
      (gen_name var subs)
      kt
      (if List.length argst > 0
       then List.fold_left (fun acc s -> acc ^ " " ^ (gen_name s subs)) "" argst
       else " ()")
      kf
      (if List.length argsf > 0
       then List.fold_left (fun acc s -> acc ^ " " ^ (gen_name s subs)) "" argsf
       else " ()")
  | Apply (x, arg, (K k, args)) -> Printf.sprintf "\tk%d (%s %s)%s" k (gen_name x subs) (gen_name arg subs) (List.fold_left (fun acc s -> acc ^ " " ^ (gen_name s subs)) "" args)
  | Return x -> "\t" ^ (gen_name x subs)

  and sprintf_cont (cps : cont) subs : string =
  match cps with
  | Let_cont (K k, args, e1, Let_cont (K k', args', e1', e2')) ->
    Printf.sprintf
      "k%d%s =\n%s\nand %s"
      k
      (if List.length args > 0
       then List.fold_left (fun acc s -> acc ^ " " ^ (gen_name s subs)) "" args
       else " ()")
      (sprintf e1 subs)
      (sprintf_cont (Let_cont (K k', args', e1', e2')) subs)
  | Let_cont (K k, args, e1, End) ->
    Printf.sprintf
      "k%d%s =\n%s\n"
      k
      (if List.length args > 0
        then List.fold_left (fun acc s -> acc ^ " " ^ (gen_name s subs)) "" args
        else " ()")
      (sprintf e1 subs)
  | End -> "()"

  and sprintf_prog (cps : cont) subs : string =
  match cps with
  | Let_cont (K k, args, e1, Let_cont (K k', args', e1', e2')) ->
    Printf.sprintf
      "let rec k%d%s =\n%s\nand %s"
      k
      (if List.length args > 0
       then List.fold_left (fun acc s -> acc ^ " " ^ (gen_name s subs)) "" args
       else " ()")
      (sprintf e1 subs)
      (sprintf_cont (Let_cont (K k', args', e1', e2')) subs)
  | Let_cont (K k, args, e1, End) ->
    Printf.sprintf
      "\nlet k%d%s =\n%s\n;;"
      k
      (if List.length args > 0
        then List.fold_left (fun acc s -> acc ^ " " ^ (gen_name s subs)) "" args
        else " ()")
      (sprintf e1 subs)
  | End -> "()"
;;

let vis var cont visites = (var, cont)::visites
let a_visite var cont visites = List.exists (fun (var', cont') -> var = var' && cont = cont') visites

let rec propagation_prim (prim : prim) args (env : (var * value) list) : named =
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
        else Prim (Add, args)
      | _ -> failwith "invalid type")
    else Prim (Add, args)
  | Print, _ :: _ -> Prim (Print, args)
  | _ -> failwith "invalid args"

and propagation (cps : expr) (env: (var * value) list) (conts : cont) visites : expr =
  match cps with
  | Let (var, Var var', expr) when has env var' -> Let (var, Var var', propagation expr ((var, get env var')::env) conts visites)
  | Let (var, Var var', expr) -> Let (var, Var var', propagation expr env conts visites)
  | Let (var, Prim (prim, args), expr) ->
    (match propagation_prim prim args env with
     | Var var' -> propagation (replace_var var var' expr) env conts visites
     | Fun (arg, expr, k) ->
       Let
         ( var
         , Fun (arg, expr, k)
         , propagation expr ((var, Fun (arg, expr, k, env)) :: env) conts visites)
     | Prim (Const x, []) ->
       Let (var, Prim (Const x, []), propagation expr ((var, Int x) :: env) conts visites)
     | Prim (prim, args) -> Let (var, Prim (prim, args), propagation expr env conts visites))
  | Let (var, Fun (arg, e, k), expr) ->
    let e' = propagation e (List.filter (fun (var, _) -> not (var = arg) )env) conts visites in
    Let
      (var, Fun (arg, e', k), propagation expr ((var, Fun (arg, e', k, env)) :: env) conts visites)
  | Apply_cont (K k, args) ->
    if has_cont conts k
    then
      if List.for_all (fun arg -> has env arg) args
      then (
        let args', cont, _ = get_cont conts k in
        propagation
        cont
          (List.map2 (fun arg' arg -> arg', get env arg) args' args)
          conts visites)
      else Apply_cont (K k, args)
    else Apply_cont (K k, args)
  | If (var, (K kt, argst), (K kf, argsf)) ->
    if has env var
    then (
      match get env var with
      | Int 0 ->
        if List.for_all (fun arg -> has env arg) argst
        then let _, cont, _ = get_cont conts kt in propagation cont env conts visites
        else Apply_cont (K kt, argst)
      | Int _ ->
        if List.for_all (fun arg -> has env arg) argsf
          then let _, cont, _ = get_cont conts kf in propagation cont env conts visites
          else Apply_cont (K kt, argsf)
      | _ -> failwith "invalid type")
    else If (var, (K kt, argst), (K kf, argsf))
  | Apply (x, arg, (K k, args)) ->
    if false && has env x && not (a_visite x k visites)
    then (
      match get env x with
      | Fun (arg', expr, K k', env') ->


        if has env arg
        then (
          let value = get env arg in
          propagation
            (replace_cont k' k (replace_var arg' arg expr))
            ((arg, value) :: env')
            (conts) (vis x k visites))
        else Apply (x, arg, (K k, args))
      | _ -> failwith "invalid type")
    else Apply (x, arg, (K k, args))
  | Return x -> Return x
  and propagation_cont (cps : cont) (env: (var * value) list) (conts : cont) visites : cont =
  match cps with
  | Let_cont (K k', args', e1, e2) ->
    let e1' = propagation e1 env conts visites in
    let e2' = propagation_cont e2 env conts visites in
    (*(match e1' with
     | Apply_cont (K k, [ arg ]) when [ arg ] = args' ->
       propagation_cont (replace_cont_cont k' k e2') env conts visites
     | _ -> *)Let_cont (K k', args', e1', e2')
  | End -> End
;;

let rec elim_unused_vars_named (vars : int array) (conts : int array) (named : named)
  : named
  =
  match named with
  | Prim (prim, args) ->
    List.iter
      (fun arg ->
        Array.set vars arg (Array.get vars arg + 1))
      args;
    Prim (prim, args)
    | Var x ->
      Array.set vars x (Array.get vars x + 1);
      Var x
  | Fun (v1, e, k) -> Fun (v1, elim_unused_vars vars conts e, k)

and elim_unused_vars (vars : int array) (conts : int array) (cps : expr) : expr =
  match cps with
  | Let (var, e1, e2) ->
    let e2' = elim_unused_vars vars conts e2 in
    if Array.get vars var > 0
    then (
      let e1' = elim_unused_vars_named vars conts e1 in
      Let (var, e1', e2'))
    else e2'
  | Apply_cont (K k, args) ->
    Array.set conts k (Array.get vars k + 1);
    List.iter
      (fun arg ->
        Array.set vars arg (Array.get vars arg + 1))
      args;
    Apply_cont (K k, args)
  | If (var, (K kt, argst), (K kf, argsf)) ->
    Array.set vars var (Array.get vars var + 1);
    Array.set conts kt (Array.get vars kt + 1);
    Array.set conts kf (Array.get vars kf + 1);
    If (var, (K kt, argst), (K kf, argsf))
  | Apply (x, arg, (K k, args)) ->
    Array.set conts k (Array.get vars k + 1);
    Array.set vars x (Array.get vars x + 1);
    Array.set vars arg (Array.get vars arg + 1);
    Apply (x, arg, (K k, args))
  | Return x ->
    Array.set vars x (Array.get vars x + 1);
    Return x
and elim_unused_vars_cont (vars : int array) (conts : int array) (cps : cont) : cont =
    match cps with
    | Let_cont (K k', args', e1, e2) ->
      let e2' = elim_unused_vars_cont vars conts e2 in
      if Array.get conts k' > 0
      then (
        let e1' = elim_unused_vars vars conts e1 in
        Let_cont (K k', args', e1', e2'))
      else e2'
    | End -> End
;;

let rec interp_prim var (prim : prim) args (env : (var * value) list) =
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

and interp_named var (named : named) (env : (var * value) list) =
  match named with
  | Prim (prim, args) -> interp_prim var prim args env
  | Fun (arg, expr, k) -> [ var, Fun (arg, expr, k, env) ]
  | Var x -> [ var, get env x ]

and interp (cps : expr) (env : env) (conts : (int * var list * expr * env) list): value =
  try
    match cps with
    | Let (var, named, expr) -> interp expr (interp_named var named env @ env) conts
    | Apply_cont (K k', args) ->
      let args', cont, _ = Env.get_cont conts k' in
      interp cont (List.map2 (fun arg' arg -> arg', get env arg) args' args ) conts
    | If (var, (K kt, argst), (K kf, argsf)) ->
      (match get env var with
       | Int n ->
         if n = 0
         then interp (Apply_cont (K kt, argst)) env conts
         else interp (Apply_cont (K kf, argsf)) env conts
       | _ -> failwith "invalid type")
    | Apply (x, arg, (K k, _)) ->
      (match get env x with
       | Fun (arg', expr, K k', env') ->
         let value = get env arg in
         let args, cont, _ = Env.get_cont conts k in
         let v = interp expr ((arg', value) :: (x, Fun (arg', expr, K k', env')) :: env') conts in
         interp cont ((List.nth args 0, v)::env) conts
       | _ -> failwith ("invalid type x" ^ (string_of_int x)))
    | Return v -> get env v
  with
  | Failure str -> failwith (Printf.sprintf "%s\n%s" str (sprintf cps []))

and interp_cont k (cps : cont) (conts : (int * var list * expr * env) list) env: value =
match cps with
    | Let_cont (K k', args', e1, e2) -> interp_cont k e2 ((k', args', e1, []) :: conts) env
    | End -> let _, cont, _ = Env.get_cont conts k in interp cont env conts
;;

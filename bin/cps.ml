let has = Env.has

let get = Env.get2

type var = int

type pointer = int

type prim =
  | Add
  | Const of int
  | Print

type named =
  | Prim of prim * var list
  | Var of var
  | Tuple of var list
  | Get of var * int
  | Closure of pointer * var list

and expr =
  | Let of var * named * expr
  | Apply_cont of pointer * var list * (pointer * var list) list
  | Call of var * var list * (pointer * var list) list
  | If of var * (pointer * var list) * (pointer * var list) * (pointer * var list) list
  | Return of var

type cont =
| Let_cont of pointer * var list * expr * cont
| End

let rec has_cont cont k =
  match cont with
  | Let_cont (k', _, _, _) when k = k' -> true
  | Let_cont (_, _, _, e2) -> has_cont e2 k
  | End -> false

let rec get_cont cont k =
match cont with
| Let_cont (k', args, e1, _) when k = k' -> args, e1
| Let_cont (_, _, _, e2) -> get_cont e2 k
| End -> failwith "cont not found"

type 'a map = (var * 'a) list

type value =
  | Int of int
  | Tuple of value list
  | Closure of pointer * value list

type env = value map

let gen_name id env =
  match Env.get_name id env with
  | Some (v, _) -> v ^ "_" ^ (string_of_int id)
  | None -> "_" ^ (string_of_int id)

let rec print_args args subs =
  match args with
  | [] -> "()"
  | [arg] -> gen_name arg subs
  | arg::args' -> (gen_name arg subs) ^ " " ^ print_args args' subs

let rec sprintf_named2 named subs =
  match named with
  | Prim (prim, args) -> sprintf_prim2 prim args subs
  | Var x -> gen_name x subs
  | Tuple (args) -> Printf.sprintf "[%s]" (List.fold_left (fun acc s -> acc ^ " " ^ (gen_name s subs) ^ ";") "" args)
  | Get (record, pos) -> Printf.sprintf "(List.nth %s %d)" (gen_name record subs) pos
  | Closure (k, args) -> Printf.sprintf "(k%d, [%s])" k (List.fold_left (fun acc s -> acc ^ " " ^ (gen_name s subs) ^ ";") "" args)

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
  | Apply_cont (k, args, stack) ->
    List.fold_left (fun string (k', args') -> Printf.sprintf "k%d (%s) %s" k' string (print_args args' subs)) (Printf.sprintf "k%d%s" k (print_args args subs)) stack
  | If (var, (kt, argst), (kf, argsf), stack) ->
    List.fold_left (fun string (k', args') -> Printf.sprintf "k%d (%s) %s" k' string (print_args args' subs)) (Printf.sprintf
      "if %s = 0 then k%d%s else k%d%s"
      (gen_name var subs)
      kt
      (print_args argst subs)
      kf
      (print_args argsf subs)) stack
  | Return x -> "\t" ^ (gen_name x subs)
  | Call (x, args, stack) -> List.fold_left (fun string (k', args') -> Printf.sprintf "k%d (%s) %s" k' string (print_args args' subs)) (Printf.sprintf "(%s %s)" (gen_name x subs) (print_args args subs)) stack

  and sprintf_cont (cps : cont) subs : string =
  match cps with
  | Let_cont (k, args, e1, Let_cont (k', args', e1', e2')) ->
    Printf.sprintf
      "k%d%s =\n%s\nand %s"
      k
      (if List.length args > 0
       then List.fold_left (fun acc s -> acc ^ " " ^ (gen_name s subs)) "" args
       else " ()")
      (sprintf e1 subs)
      (sprintf_cont (Let_cont (k', args', e1', e2')) subs)
  | Let_cont (k, args, e1, End) ->
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
  | Let_cont (k, args, e1, Let_cont (k', args', e1', e2')) ->
    Printf.sprintf
      "let rec k%d%s =\n%s\nand %s"
      k
      (if List.length args > 0
       then List.fold_left (fun acc s -> acc ^ " " ^ (gen_name s subs)) "" args
       else " ()")
      (sprintf e1 subs)
      (sprintf_cont (Let_cont (k', args', e1', e2')) subs)
  | Let_cont (k, args, e1, End) ->
    Printf.sprintf
      "\nlet k%d%s =\n%s\n;;"
      k
      (if List.length args > 0
        then List.fold_left (fun acc s -> acc ^ " " ^ (gen_name s subs)) "" args
        else " ()")
      (sprintf e1 subs)
  | End -> "()"
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
  | Var x -> [ var, get env x ]
  | Tuple (args) -> [var, Tuple (List.map (fun arg -> get env arg) args)]
  | Get (record, pos) -> begin
    match get env record with
    | Tuple (values) -> [var, List.nth values pos]
    | _ -> failwith "invalid type"
    end
  | Closure (k, args) -> [var, Closure (k, List.map (fun arg -> get env arg) args)]

and interp (stack: (pointer * value list) list) (cps : expr) (env : env) (conts : (int * var list * expr * env) list): value =
  try
    match cps with
    | Let (var, named, expr) -> interp stack expr (interp_named var named env @ env) conts
    | Apply_cont (k, args, stack') -> let args', cont, _ = Env.get_cont conts k in
      interp ((List.map (fun (k, env') -> (k, (List.map (fun arg -> get env arg) env'))) stack')@stack) cont (List.map2 (fun arg' arg -> arg', get env arg) args' args ) conts
    | If (var, (kt, argst), (kf, argsf), stack') ->
      (match get env var with
       | Int n ->
         if n = 0
         then interp stack (Apply_cont (kt, argst, stack')) env conts
         else interp stack (Apply_cont (kf, argsf, stack')) env conts
       | _ -> failwith "invalid type")
    | Return v -> begin
      match stack with
      | [] -> get env v
      | (k, env')::stack' -> let args2', cont'', _ = Env.get_cont conts k in
      interp stack' cont'' ((List.hd args2', get env v)::(List.map2 (fun arg' arg -> arg', arg) (List.tl args2') env') ) conts
    end
    | Call (x, args, stack') -> begin
      match get env x with
      | Closure (k', env') -> let args', cont, _ = Env.get_cont conts k' in
        interp ((List.map (fun (k, env') -> (k, (List.map (fun arg -> get env arg) env'))) stack')@stack) cont ((List.hd args', (Tuple env'))::(List.map2 (fun arg' arg -> arg', get env arg) (List.tl args') args)) conts
      | _ -> failwith ("invalid type")
       end
  with
  | Failure str -> failwith (Printf.sprintf "%s\n%s" str (sprintf cps []))

and interp_cont k (cps : cont) (conts : (int * var list * expr * env) list) env: value =
match cps with
    | Let_cont (k', args', e1, e2) -> interp_cont k e2 ((k', args', e1, []) :: conts) env
    | End -> let _, cont, _ = Env.get_cont conts k in interp [] cont env conts
;;


let _ = function
| Int i -> Prim (Const i, [])
| _ -> failwith "not implemented"

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
  | Let (var, Var var', expr) -> Let (var, Var var', propagation expr (if has env var' then (var, get env var')::env else env) conts visites)
  | Let (var, Prim (prim, args), expr) ->
    (match propagation_prim prim args env with
     | Var _ -> propagation (*replace_var var var' expr*) expr env conts visites
     | Prim (Const x, []) ->
       Let (var, Prim (Const x, []), propagation expr ((var, Int x) :: env) conts visites)
     | Prim (prim, args) -> Let (var, Prim (prim, args), propagation expr env conts visites)
     (*TODO*)
     | _ -> cps)
  | Let (var, Tuple vars, expr) -> if List.for_all (fun arg -> has env arg) vars then
      Let (var, Tuple vars, propagation expr ((var, Tuple (List.map (fun var' -> get env var') vars))::env) conts visites)
    else Let (var, Tuple vars, propagation expr env conts visites)
  | Let (var, Get (var', pos), expr) -> if has env var' then begin
    match get env var' with
    | Tuple values -> Let (var, Get (var', pos), propagation expr ((var, List.nth values pos)::env) conts visites)
    | _ -> failwith "invalid type"
  end else Let (var, Get (var', pos), propagation expr env conts visites)
  | Apply_cont (k', args, stack) -> if List.for_all (fun arg -> has env arg) args then
        let args', cont = get_cont conts k' in
        propagation cont (List.map2 (fun arg' arg -> arg', get env arg) args' args) conts visites
      else Apply_cont (k', args, stack)
  | If (var, (kt, argst), (kf, argsf), stack) ->
    if has env var then begin
      match get env var with
      | Int 0 -> if List.for_all (fun arg -> has env arg) argst then
          let args, cont = get_cont conts kt in propagation cont (List.map2 (fun arg' arg -> arg', get env arg) argst args) conts visites
        else Apply_cont (kt, argst, stack)
      | Int _ -> if List.for_all (fun arg -> has env arg) argsf then
          let args, cont = get_cont conts kf in propagation cont (List.map2 (fun arg' arg -> arg', get env arg) argsf args) conts visites
        else Apply_cont (kf, argsf, stack)
      | _ -> failwith "invalid type"
    end else If (var, (kt, argst), (kf, argsf), stack)
  | Return x -> Return x
  | Call (x, args, stack) -> Call (x, args, stack)
  | _ -> cps
  (*TODO*)
  and propagation_cont (cps : cont) (env: (var * value) list) (conts : cont) visites : cont =
  match cps with
  | Let_cont (k', args', e1, e2) ->
    let e1' = propagation e1 env conts visites in
    let e2' = propagation_cont e2 env conts visites in
    (*(match e1' with
     | Apply_cont (k, [ arg ]) when [ arg ] = args' ->
       propagation_cont (replace_cont_cont k' k e2') env conts visites
     | _ -> *)Let_cont (k', args', e1', e2')
  | End -> End
;;

let rec elim_unused_vars_named (vars : int array) (named : named)
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
  (*TODO*)
  | _ -> named

and elim_unused_vars (vars : int array) (conts : int array) (cps : expr) : expr =
  match cps with
  | Let (var, e1, e2) ->
    let e2' = elim_unused_vars vars conts e2 in
    if Array.get vars var > 0
    then (
      let e1' = elim_unused_vars_named vars e1 in
      Let (var, e1', e2'))
    else e2'
  | Apply_cont (k, args, stack) ->
    Array.set conts k (Array.get vars k + 1);
    List.iter
      (fun arg ->
        Array.set vars arg (Array.get vars arg + 1))
      args;
    Apply_cont (k, args, stack)
  | If (var, (kt, argst), (kf, argsf), stack) ->
    Array.set vars var (Array.get vars var + 1);
    Array.set conts kt (Array.get vars kt + 1);
    Array.set conts kf (Array.get vars kf + 1);
    If (var, (kt, argst), (kf, argsf), stack)
  | Return x ->
    Array.set vars x (Array.get vars x + 1);
    Return x
  | Call (x, args, stack) ->
    List.iter (fun (_, args2) ->
    List.iter
    (fun arg ->
      Array.set vars arg (Array.get vars arg + 1)) args;
      List.iter
    (fun arg ->
      Array.set vars arg (Array.get vars arg + 1)) args2) stack;
      Call (x, args, stack)
and elim_unused_vars_cont (vars : int array) (conts : int array) (cps : cont) : cont =
    match cps with
    | Let_cont (k', args', e1, e2) ->
      let e2' = elim_unused_vars_cont vars conts e2 in
      if Array.get conts k' > 0
      then (
        let e1' = elim_unused_vars vars conts e1 in
        Let_cont (k', args', e1', e2'))
      else e2'
    | End -> End
;;

let get env arg = if has env arg then get env arg else arg

let rec inline_named (named : named) (env : (var * var) list) =
  match named with
  | Prim (prim, args) -> Prim (prim, List.map (fun arg -> get env arg) args)
  | Var x -> Var (get env x)
  | Tuple (args) -> Tuple (List.map (fun arg -> get env arg) args)
  | Get (record, pos) -> Get (get env record, pos)
  | Closure (k, args) -> Closure (k, List.map (fun arg -> get env arg) args)

and inline (stack: (pointer * var list) list) (cps : expr) (env : (var * var) list) (conts : cont): expr =
  try
    match cps with
    | Let (var, named, expr) -> Let (var, inline_named named env, inline stack expr env conts)
    | Apply_cont (k, args, stack') -> Apply_cont (k, args, stack' @ stack)
    | If (var, (kt, argst), (kf, argsf), stack') -> If (var, (kt, argst), (kf, argsf), stack' @ stack)
    | Return v -> begin
      match stack with
      | [] -> Return v (* ?? *)
      | (k, env')::stack' -> Apply_cont (k, env', stack')
    end
    | Call (x, args, stack') -> Call (x, args, stack' @ stack)
  with
  | Failure str -> failwith (Printf.sprintf "%s\n%s" str (sprintf cps []))

let rec inline_parent (cps : expr) (conts: cont): expr =
  try
    match cps with
    | Let (var, named, expr) -> Let (var, named, inline_parent expr conts)
    | Apply_cont (k, args, stack') -> let args', cont = get_cont conts k in
        inline stack' cont (List.map2 (fun arg' arg -> arg', arg) args' args) conts
    | If (var, (kt, argst), (kf, argsf), stack') -> If (var, (kt, argst), (kf, argsf), stack')
    | Return v -> Return v (* ?? *)
    | Call (x, args, stack') -> Call (x, args, stack')
  with
  | Failure str -> failwith (Printf.sprintf "%s\n%s" str (sprintf cps []))

let rec inline_cont ks (cps : cont) (conts : cont): cont =
  match cps with
      | Let_cont (k', args', e1, e2) when List.mem k' ks -> Let_cont (k', args', inline_parent e1 conts, inline_cont ks e2 conts)
      | Let_cont (k', args', e1, e2) -> Let_cont (k', args', e1, inline_cont ks e2 conts)
      | End -> End
  ;;
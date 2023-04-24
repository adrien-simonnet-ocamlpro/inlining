type 'a map = (string * 'a) list

type value =
  | Int of int
  | Fun of Ast.var * Cps.expr * Cps.kvar * value map 

type env = value map

let print_env env =
  Printf.printf "%s ]\n%!" (List.fold_left (fun str (x, _) -> str ^ " x" ^ x) "[" env)
;;

let has env var = List.exists (fun (var', _) -> var = var') env
let has_cont conts var = List.exists (fun (var', _, _, _) -> var = var') conts

let get = Env.get

let get_cont (env : (int * string list * Cps.expr * env) list) var =
  match List.find_opt (fun (var', _, _, _) -> var = var') env with
  | Some (_, args, v, env') -> args, v, env'
  | None ->
    failwith
      ("k"
       ^ string_of_int var
       ^ " not found in "
       ^ List.fold_left (fun str (x, _, _, _) -> str ^ " k" ^ string_of_int x) "[" env
       ^ " ].")
;;

let rec propagation_prim (prim : Ast.prim) args (env : (Ast.var * value) list) : Cps.named
  =
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

and propagation (cps : Cps.expr) env (conts : (int * Ast.var list * Cps.expr * env) list)
  : Cps.expr
  =
  match cps with
  | Let (var, Var var', expr) -> propagation (Cps.replace_var var var' expr) env conts
  | Let (var, Prim (prim, args), expr) ->
    (match propagation_prim prim args env with
     | Var var' -> propagation (Cps.replace_var var var' expr) env conts
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
       propagation (Cps.replace_cont k' k e2') env conts
     | _ -> Let_cont (K k', args', e1', e2'))
  | Apply_cont (K k, []) ->
    let _, cont, env' = get_cont conts k in
    propagation cont env' conts
  | Apply_cont (K k, args) ->
    if has_cont conts k
    then
      if not (List.exists (fun arg -> not (has env arg)) args)
      then (
        let args', cont, env' = get_cont conts k in
        propagation
          cont
          (List.map2 (fun arg' arg -> arg', get env arg) args' args @ env')
          conts)
      else Apply_cont (K k, args)
    else Apply_cont (K k, args)
  | If (var, (K kt, argst), (K kf, argsf)) ->
    if has env var
    then (
      match get env var with
      | Int n -> if n != 0 then Apply_cont (K kt, argst) else Apply_cont (K kf, argsf)
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
            (Cps.replace_cont k' k (Cps.replace_var arg' arg expr))
            ((arg', value) :: env')
            ((k', args, cont, env'') :: conts))
        else Apply (x, arg, K k)
      | _ -> failwith "invalid type")
    else Apply (x, arg, K k)
  | Return x -> Return x
;;

let rec elim_unused_vars_named (vars : int array) (conts : int array) (named : Cps.named)
  : Cps.named
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

and elim_unused_vars (vars : int array) (conts : int array) (cps : Cps.expr) : Cps.expr =
  match cps with
  | Let (var, e1, e2) ->
    let e2' = elim_unused_vars vars conts e2 in
    if Array.get vars (int_of_string var) > 0
    then (
      let e1' = elim_unused_vars_named vars conts e1 in
      Let (var, e1', e2'))
    else e2'
  | Let_cont (K k', args', e1, e2) ->
    let e1' = elim_unused_vars vars conts e1 in
    let e2' = elim_unused_vars vars conts e2 in
    if Array.get conts k' > 0 then Let_cont (K k', args', e1', e2') else e2'
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

and interp_named var (named : Cps.named) (env : (Ast.var * value) list) =
  match named with
  | Prim (prim, args) -> interp_prim var prim args env
  | Fun (arg, expr, k) ->
    print_env env;
    [ var, Fun (arg, expr, k, env) ]
  | Var x -> [ var, get env x ]

and interp
  (cps : Cps.expr)
  (env : env)
  (conts : (int * Ast.var list * Cps.expr * env) list)
  =
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
  | Failure str -> failwith (Printf.sprintf "%s\n%s" str (Cps.sprintf cps))
;;

let usage_msg = "cps [-prop] [-clean] [-eval] [-ast] <file1> [<file2>] ... -o <output>"
let verbose = ref false
let input_files = ref []
let output_file = ref ""
let prop = ref false
let eval = ref false
let show_ast = ref false
let unused_vars = ref false
let anon_fun filename = input_files := filename :: !input_files

let speclist =
  [ "-verbose", Arg.Set verbose, "Output debug information"
  ; "-o", Arg.Set_string output_file, "Set output file name"
  ; "-prop", Arg.Set prop, "Const propagation"
  ; "-clean", Arg.Set unused_vars, "Clean unused vars"
  ; "-eval", Arg.Set eval, "Eval CPS"
  ; "-ast", Arg.Set show_ast, "Show AST"
  ]
;;

let _ =
  Arg.parse speclist anon_fun usage_msg;
  for i = 0 to List.length !input_files - 1 do
    let _ = Array.length Sys.argv = 3 in
    let entree = open_in (List.nth !input_files i) in
    let source = Lexing.from_channel entree in
    try
      let ast = Parser.programme Lexer.jetons source in
      if !show_ast
      then Ast.print_expr ast
      else (
        let cps = Cps.from_ast ast "0" (Return "0") in
        let cps2 = if !prop then propagation cps [] [] else cps in
        let cps3 =
          if !unused_vars
          then elim_unused_vars (Array.make 1000 0) (Array.make 1000 0) cps2
          else cps2
        in
        if !eval
        then (
          let env = interp cps3 [] [] in
          Printf.printf
            "\n----- ENV -----\n%s"
            (List.fold_left
               (fun str (var, value) ->
                 match value with
                 | Int i -> str ^ "x" ^ var ^ " = " ^ string_of_int i ^ "\n"
                 | _ -> str ^ "x" ^ var ^ " = fun\n")
               ""
               env))
        else Printf.printf "%s;;\n%!" (Cps.sprintf cps3))
    with
    | Parsing.Parse_error ->
      Printf.printf "Erreur de parsing au caractère %d.\n" source.Lexing.lex_curr_pos
    | Failure s -> Printf.printf "Failure: %s.\n" s
  done
;;

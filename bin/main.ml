type 'a map = (string * 'a) list

type value =
  | Int of int
  | Fun of Ast.var * Cps.expr * Cps.kvar * value map 

type env = value map

(* let print_env env =
  Printf.printf "%s ]\n%!" (List.fold_left (fun str (x, _) -> str ^ " x" ^ x) "[" env)
;; *)


let get = Env.get
let get_cont = Env.get_cont


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
  let outchan = if !output_file = "" then stdout else open_out !output_file in
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
        let cps2 = if !prop then Cps.propagation cps [] [] else cps in
        let cps3 =
          if !unused_vars
          then Cps.elim_unused_vars (Array.make 1000 0) (Array.make 1000 0) cps2
          else cps2
        in
        if !eval
        then (
          let env = interp cps3 [] [] in
          Printf.fprintf outchan
            "----- ENV -----\n%s"
            (List.fold_left
               (fun str (var, value) ->
                 match value with
                 | Int i -> str ^ "x" ^ var ^ " = " ^ string_of_int i ^ "\n"
                 | _ -> str ^ "x" ^ var ^ " = fun\n")
               ""
               env))
        else Printf.fprintf outchan "%s;;\n%!" (Cps.sprintf cps3))
    with
    | Parsing.Parse_error ->
      Printf.fprintf stderr "Erreur de parsing au caractère %d.\n" source.Lexing.lex_curr_pos
    | Failure s -> Printf.fprintf stderr "Failure: %s.\n" s
  done
;;

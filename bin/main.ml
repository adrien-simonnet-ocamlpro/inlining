type value = Int of int | Fun of Ast.var * Cps.expr * Cps.kvar

let get env var =
  match List.find_opt (fun (var', _) -> var = var') env with
  | Some (_, v) -> v
  | None -> failwith ("x" ^ var ^ " not found.")

let get_cont env var =
  match List.find_opt (fun (var', _, _) -> var = var') env with
  | Some (_, args, v) -> (args, v)
  | None -> failwith ("k" ^ string_of_int var ^ " not found.")

let rec interp_prim var (prim : Ast.prim) args (env : (Ast.var * value) list) =
  match (prim, args) with
  | Const x, _ -> (var, Int x) :: env
  | Add, x1 :: x2 :: _ -> (
      match (get env x1 : value) with
      | Int n1 -> (
          match get env x2 with
          | Int n2 -> (var, Int (n1 + n2)) :: env
          | _ -> failwith "invalid type")
      | _ -> failwith "invalid type")
  | Print, x1 :: _ -> (
      match (get env x1 : value) with
      | Int n ->
          Printf.printf "%d\n" n;
          env
      | _ -> failwith "invalid type")
  | _ -> failwith "invalid args"

and interp_named var (named : Cps.named) (env : (Ast.var * value) list) =
  match named with
  | Prim (prim, args) -> interp_prim var prim args env
  | Fun (arg, expr, k) -> (var, Fun (arg, expr, k)) :: env
  | Var x -> (var, get env x) :: env

and interp (cps : Cps.expr) env (conts : (int * Ast.var list * Cps.expr) list) =
  match cps with
  | Let (var, named, expr) -> interp expr (interp_named var named env) conts
  | Let_cont (K k', args', e1, e2) -> interp e2 env ((k', args', e1) :: conts)
  | Apply_cont (K k, _) when k = 0 -> env
  | Apply_cont (K k', args) ->
      let args', cont = get_cont conts k' in
      interp cont
        (List.map2 (fun arg' arg -> (arg', get env arg)) args' args @ env)
        conts
  | If (var, (K kt, argst), (K kf, argsf)) -> (
      match get env var with
      | Int n ->
          if n = 0 then interp (Apply_cont (K kt, argst)) env conts
          else interp (Apply_cont (K kf, argsf)) env conts
      | _ -> failwith "invalid type")
  | Apply (x, arg, K k) -> (
      match get env x with
      | Fun (arg', expr, K k') ->
          let value = get env arg in
          let args, cont = get_cont conts k in
          interp expr ((arg', value) :: env) ((k', args, cont) :: conts)
      | _ -> failwith "invalid type")

let _ =
  let entree = open_in Sys.argv.(1) in
  let source = Lexing.from_channel entree in
  try
    let ast = Parser.programme Lexer.jetons source in
    Printf.printf "----- AST -----\n%s\n" (Ast.sprintf ast);
    let cps = Cps.from_ast ast "0" (Apply_cont (Cps.K 0, [])) in
    Printf.printf "\n----- CPS -----\n%s\n" (Cps.sprintf cps);
    Printf.printf "\n---- EVAL -----\n";
    let env = interp cps [] [] in
    Printf.printf "\n----- ENV -----\n%s"
      (List.fold_left
         (fun str (var, value) ->
           match value with
           | Int i -> str ^ "x" ^ var ^ " = " ^ string_of_int i ^ "\n"
           | _ -> str ^ "x" ^ var ^ " = fun\n")
         "" env)
  with
  | Parsing.Parse_error ->
      Printf.printf "Erreur de parsing au caractère %d.\n"
        source.Lexing.lex_curr_pos
  | Failure s -> Printf.printf "Failure: %s.\n" s

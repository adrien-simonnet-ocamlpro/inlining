

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
        let cps, subs, fv, cont = Ast.to_cps (Cps.End) [] ast 0 (Return 0) [] in
        let cont' = Cps.Let_cont (Cps.K 0, fv, cps, cont) in
        Env.print_subs subs;
        Env.print_fv fv;
        let cps2 = if !prop then Cps.propagation cps [] [] [] else cps in
        let _ =
          if !unused_vars
          then Cps.elim_unused_vars (Array.make 1000 0) (Array.make 1000 0) cps2
          else cps2
        in
        if !eval
        then (
          let init = List.map (fun fv -> let i = Printf.printf "%s = " (Env.get_var subs fv) ; int_of_string (read_line ()) in (fv, Cps.Int i)) fv in
          let r = Cps.interp_cont 0 cont' [] init in
          (
            match r with
            | Int i -> Printf.printf "%d\n" i
            | _ -> Printf.printf "fun\n"
          )
        )
        else (* Printf.fprintf outchan "%s;;\n%!" (Cps.sprintf cps3 subs)) *)
        Printf.fprintf outchan "%s;;\n%!" (Cps.sprintf_prog cont' subs))
    with
    | Parsing.Parse_error ->
      Printf.fprintf
        stderr
        "Erreur de parsing au caractère %d.\n"
        source.Lexing.lex_curr_pos
    | Failure s -> Printf.fprintf stderr "Failure: %s.\n" s
  done
;;

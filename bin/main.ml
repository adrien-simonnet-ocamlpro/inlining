

let usage_msg = "cps [-prop] [-clean] [-eval] [-ast] <file1> [<file2>] ... -o <output>"
let verbose = ref false
let input_files = ref []
let output_file = ref ""
let analysis = ref false
let prop = ref false
let eval = ref false
let show_ast = ref false
let unused_vars = ref false
let anon_fun filename = input_files := filename :: !input_files

let inline_conts = ref ([] : int list)

let inline k = inline_conts := k :: !inline_conts

let speclist =
  [ "-verbose", Arg.Set verbose, "Output debug information"
  ; "-o", Arg.Set_string output_file, "Set output file name"
  ; "-analysis", Arg.Set analysis, "Analysis"
  ; "-prop", Arg.Set prop, "Propagation"
  ; "-clean", Arg.Set unused_vars, "Clean unused vars"
  ; "-eval", Arg.Set eval, "Eval CPS"
  ; "-ast", Arg.Set show_ast, "Show AST"
  ; "-inline", Arg.Int inline, "Inline specified cont"
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
      let cst = Ast.expr_to_cst ast (Ast.Constructors.empty) in
      if !show_ast     
      then Ast.print_expr ast
      else (
        let cps, subs, fv, cont = Cst.to_cps (Cps.End) [] cst 0 (Return 0) [] in
        let cont' = Cps.Let_cont (0, fv, cps, cont) in
        let cps2 = if !prop then let analysis = Analysis.start_analysis cont' [] in
        Analysis.pp_analysis (Format.std_formatter) analysis; cont' (*Propagation.propagation_cont cont' cont' analysis*) else cont' in
        let cps2 = Cps.cont_to_asm cps2 in
        let cps3 =
          if !unused_vars
          then let cps, _ = Cleaner.elim_unused_vars_cont (Array.make 1000 0) cps2 in cps
          else cps2
        in
        let cps3 = if List.length !inline_conts > 0 then Inliner.inline_cont !inline_conts cps3 cps3 else cps3 in
        let cps3 =
          if !unused_vars
          then let conts = (Array.make 1000 0) in Array.set conts 0 1; let cps, conts = Cleaner.elim_unused_vars_cont (conts) cps3 in Cleaner.elim_unused_conts conts cps
          else cps3
        in
        if !eval
        then (
          let init = List.map (fun fv -> let i = Printf.fprintf outchan "%s = " (Env.get_var subs fv) ; int_of_string (read_line ()) in (fv, Interpreter.Int i)) fv in
          let r = Interpreter.interp_cont 0 cps3 [] init in
          (
            match r with
            | Int i -> Printf.fprintf outchan "%d\n" i
            | _ -> Printf.fprintf outchan "fun\n"
          )
        )
        else  (    (* Printf.fprintf outchan "%s;;\n%!" (Cps.sprintf cps3 subs)) *)
        Printf.fprintf outchan "type value =\n| Int of int\n| Tuple of value list\n| Function of (value -> value -> value)\n| Environment of value list\n| Closure of value * value\n| Constructor of int * value\n\nlet print (Int i) = Printf.printf \"%%d\" i\n\nlet add (Int a) (Int b) = Int (a + b)\n\nlet get value pos =\nmatch value with\n| Tuple vs -> List.nth vs pos\n| Environment vs -> List.nth vs pos\n| Closure (f, _) when pos = 0 -> f\n| Closure (_, env) when pos = 1 -> env\n| Constructor (tag, _) when pos = 0 -> Int tag\n| Constructor (_, env) when pos = 1 -> env\n| _ -> assert false\n\nlet call (Function k) = k\n\nlet rec ";
        Asm.pp_cont subs (Format.formatter_of_out_channel outchan) cps3;
        Printf.fprintf outchan ";;\nk0 ()"))
    with
    | Parsing.Parse_error ->
      Printf.fprintf
        stderr
        "Erreur de parsing au caractère %d.\n"
        source.Lexing.lex_curr_pos
    | Failure s -> Printf.fprintf stderr "Failure: %s.\n" s
  done
;;

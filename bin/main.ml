

let usage_msg = "cps [-prop] [-clean] [-eval] [-ast] <file1> [<file2>] ... -o <output>"
let verbose = ref false
let input_files = ref []
let output_file = ref ""
let analysis = ref false
let prop = ref false
let eval = ref false
let show_ast = ref false
let show_cst = ref false
let show_cps = ref false
let show_aps = ref false
let show_asm = ref false

let unused_vars = ref false
let anon_fun filename = input_files := filename :: !input_files

let copy_conts = ref ([] : int list)

let copy k = copy_conts := k :: !copy_conts

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
  ; "-cst", Arg.Set show_cst, "Show CST"
  ; "-cps", Arg.Set show_cps, "Show CPS"
  ; "-aps", Arg.Set show_aps, "Show APS"
  ; "-asm", Arg.Set show_asm, "Show ASM"
  ; "-copy", Arg.Int copy, "Copy specified block"
  ; "-inline", Arg.Int inline, "Inline specified block"
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
      if !show_ast then Ast.pp_expr (Format.formatter_of_out_channel outchan) ast
      else begin
        let cst, _vars, _subs, _fvs = Ast.expr_to_cst ast (Seq.ints 0) [] (Ast.Constructors.empty) in
        Env.print_subs _subs;
        Env.print_subs _fvs;
        if !show_cst then Cst.pp_expr (List.fold_left (fun map (s, v) -> Cps.VarMap.add v s map) (Cps.VarMap.empty) (_subs @ _fvs)) (Format.formatter_of_out_channel outchan) cst
        else begin
          let var0, _vars = match Seq.uncons _vars with
          | Some (var0, _vars) -> var0, _vars
          | None -> assert false in
          let expr, _vars, fv, conts = Cst.to_cps _vars [] cst var0 (Return var0) in
          Env.print_fv fv;
          let cps = Cst.add_block 0 (Cps.Cont (fv, expr)) conts in
          let cps = if !unused_vars then Cps.clean_blocks cps else cps in
          let _cps_analysis = if !analysis then let a = Analysis.start_analysis cps in Analysis.pp_analysis (Format.std_formatter) a; a else Analysis.Analysis.empty in
          let cps = if !prop then Analysis.propagation_blocks cps _cps_analysis else cps in
          let cps, _vars = if List.length !copy_conts > 0 then Cps.copy_blocks cps (Cps.BlockSet.of_list !copy_conts) _vars else cps, _vars in
          if !show_cps then Cps.pp_blocks (List.fold_left (fun map (s, v) -> Cps.VarMap.add v s map) (Cps.VarMap.empty) _subs) (Format.formatter_of_out_channel outchan) cps
          else begin
            let asm, _vars = Cps.blocks_to_asm cps (Seq.ints 1000) in
            let asm = if !unused_vars then let cps, _ = Asm.elim_unused_vars_blocks asm in cps else asm in
            let asm = if List.length !inline_conts > 0 then Asm.inline_blocks asm (Asm.BlockSet.of_list !inline_conts) else asm in
            let asm = if !unused_vars then let cps, conts = Asm.elim_unused_vars_blocks asm in Array.set conts 0 1; Asm.elim_unused_blocks conts cps else asm in
            if !show_asm then begin
              Printf.fprintf outchan "type value =\n| Int of int\n| Tuple of value list\n| Function of (value -> value -> value)\n| Environment of value list\n| Closure of value * value\n| Constructor of int * value\n\nlet print (Int i) = Printf.printf \"%%d\" i\n\nlet add (Int a) (Int b) = Int (a + b)\n\nlet get value pos =\nmatch value with\n| Tuple vs -> List.nth vs pos\n| Environment vs -> List.nth vs pos\n| Closure (f, _) when pos = 0 -> f\n| Closure (_, env) when pos = 1 -> env\n| Constructor (tag, _) when pos = 0 -> Int tag\n| Constructor (_, env) when pos = 1 -> env\n| _ -> assert false\n\nlet call (Function k) = k\n\nlet rec ";
              Asm.pp_blocks (List.fold_left (fun map (s, v) -> Cps.VarMap.add v s map) (Cps.VarMap.empty) _subs) (Format.formatter_of_out_channel outchan) asm;
              Printf.fprintf outchan ";;\nk0 ()"
            end else begin
              let init = List.map (fun fv -> let i = Printf.fprintf outchan "%s = " (Env.get_var _fvs fv) ; int_of_string (read_line ()) in (fv, Asm.Int i)) fv in
              let r = Asm.interp_blocks asm 0 init in
                match r with
                | Int i -> Printf.fprintf outchan "%d\n" i
                | _ -> Printf.fprintf outchan "fun\n"
            end
          end
        end
      end
    with
    | Parsing.Parse_error -> Printf.fprintf stderr "Erreur de parsing au caractère %d.\n" source.Lexing.lex_curr_pos
    | Failure s -> Printf.fprintf stderr "Failure: %s.\n" s
  done
;;

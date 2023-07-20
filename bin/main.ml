

let usage_msg = "cps <file1> [<file2>] ... -o <output>"
let verbose = ref false
let input_files = ref []
let log_file = ref ""
let output_file = ref ""
let rounds = ref 1

let unused_vars = ref true
let anon_fun filename = input_files := filename :: !input_files

let copy_conts = ref ([] : int list)

let copy k = copy_conts := k :: !copy_conts

let inline_conts = ref ([] : int list)

let inline k = inline_conts := k :: !inline_conts

let threshold = ref 5

let speclist =
  [ "-verbose", Arg.Set verbose, "Output debug information"
  ; "-o", Arg.Set_string output_file, "Set output file name"
  ; "-log", Arg.Set_string log_file, "Set log file name"
  ; "-copy", Arg.Int copy, "Copy specified block"
  ; "-inline", Arg.Int inline, "Inline specified block"
  ; "-max", Arg.Set_int threshold, "Copy blocks smaller than specified threshold"
  ; "-rounds", Arg.Set_int rounds, "Repeat tree optimization and inlining phases this many times (default 1). Rounds are numbered starting from zero."
  ]
;;

let rec step_analysis cps _vars _pointers count logchan =
  if count > 0 then begin
    Logger.start "Cleaning\n";
    let cps = Cps.clean_blocks cps in
    Logger.stop ();
    Logger.start "Analysing\n";
    let _cps_analysis = Analysis.start_analysis cps in
    let cps = Cps.BlockMap.filter (fun k _ -> if Analysis.Closures.mem k _cps_analysis then true else (Logger.log "Filtred k%d\n" k; false)) cps in
    Logger.stop ();
    Logger.start "Propagating\n";
    let cps = Analysis.propagation_blocks cps _cps_analysis in
    Logger.stop ();
    let to_copy = Cps.BlockMap.fold (fun k (block, expr) to_copy -> if Cps.size_block block + Cps.size_expr expr < !threshold then Cps.BlockSet.add k to_copy else to_copy) cps Cps.BlockSet.empty in
    Logger.start "Copying%s\n" (Cps.BlockSet.fold (fun k s -> s ^ " k" ^ (string_of_int k)) to_copy "");
    let cps, _vars, _pointers = Cps.copy_blocks cps (Cps.BlockSet.union (Cps.BlockSet.of_list !copy_conts) to_copy) _vars _pointers in
    Logger.stop ();
    Logger.start "Cleaning\n";
    let cps = Cps.clean_blocks cps in
    Logger.stop ();
    step_analysis cps _vars _pointers (count-1) logchan
  end else cps, _vars, _pointers

let _ =
  Arg.parse speclist anon_fun usage_msg;
  let logchan = if !log_file = "" then stdout else open_out !log_file in
  for i = 0 to List.length !input_files - 1 do
    let input_file = (List.nth !input_files i) in
    let _ = Array.length Sys.argv = 3 in
    let entree = open_in input_file in
    let source = Lexing.from_channel entree in
    try
      Logger.start "AST -> %s\n" (input_file ^ ".ast");
      let ast = Parser.programme Lexer.jetons source in
      Ast.pp_expr (Format.formatter_of_out_channel (open_out (input_file ^ ".ast"))) ast;
      Logger.stop ();
      Logger.start "CST -> %s\n" (input_file ^ ".cst");
      let cst, _vars, _subs, _fvs = Ast.expr_to_cst ast (Seq.ints 0) Ast.VarMap.empty Ast.TagMap.empty in
      Cst.pp_expr (List.fold_left (fun map (s, v) -> Cps.VarMap.add v s map) _subs (Ast.VarMap.fold (fun i v subs -> (i, v)::subs) _fvs [])) (Format.formatter_of_out_channel (open_out (input_file ^ ".cst"))) cst;
      Logger.stop ();
      Logger.start "CPS -> %s\n" (input_file ^ ".cps");
      let var0, _vars = match Seq.uncons _vars with
      | Some (var0, _vars) -> var0, _vars
      | None -> assert false in
      let _pointer0, _pointers = match Seq.uncons (Seq.ints 0) with
      | Some (pointer0, _pointers) -> pointer0, _pointers
      | None -> assert false in
      let expr, _vars, _pointers, fv, conts = Cst.to_cps _vars _pointers [] cst var0 (Return var0) in
      let cps = Cst.add_block _pointer0 (Cps.Cont fv, expr) conts in
      let cps, _vars, _pointers = step_analysis cps _vars _pointers !rounds logchan in
      Cps.pp_blocks _subs (Format.formatter_of_out_channel (open_out (input_file ^ ".cps"))) cps;
      Logger.stop ();
      Logger.start "ASM -> %s\n" (input_file ^ ".asm");
      let asm, _vars, _pointers = Cps.blocks_to_asm cps _vars _pointers in
      Logger.start " Cleaning...\n";
      let asm, conts' = Asm.elim_unused_vars_blocks asm in
      Logger.stop ();
      Logger.start " Inlining...\n";
      let asm = Asm.inline_blocks asm (Asm.BlockSet.union (Asm.BlockSet.of_list !inline_conts) (Asm.BlockSet.of_list (List.map (fun (b, _) -> b) (List.filter (fun (_, count) -> count = 1) (Array.to_list (Array.mapi (fun i count -> (i, count)) conts')))))) in
      Logger.stop ();
      Logger.start " Cleaning...\n";
      let asm = if !unused_vars then let cps, conts = Asm.elim_unused_vars_blocks asm in Array.set conts 0 1; Asm.elim_unused_blocks conts cps else asm in
      Logger.start " Cleaning...\n";
      let asm = if !unused_vars then let cps, conts = Asm.elim_unused_vars_blocks asm in Array.set conts 0 1; Asm.elim_unused_blocks conts cps else asm in
      Logger.stop ();
      Asm.pp_blocks _subs (Format.formatter_of_out_channel (open_out (input_file ^ ".asm"))) asm;
      Printf.printf " Size = %d.\n %s\n" (Asm.size_blocks asm) (input_file ^ ".asm");
      Logger.stop ();
      let init = List.map (fun fv -> let i = Printf.printf "%s = " (Env.get_var (Ast.VarMap.bindings _fvs) fv) ; int_of_string (read_line ()) in (fv, Asm.Int i)) fv in
      Logger.log "eval\n";
      let r, _benchmark = Asm.interp_blocks asm 0 init in
        Asm.pp_benchmark _benchmark Format.std_formatter;
        match r with
        | Int i -> Printf.printf "%d\n" i
        | _ -> Printf.printf "fun\n"
    with
    | Parsing.Parse_error -> Printf.fprintf stderr "Erreur de parsing au caractère %d.\n" source.Lexing.lex_curr_pos
    | Failure s -> Printf.fprintf stderr "Failure: %s.\n" s
  done
;;

let rec cherche_motif motif liste =
  match motif, liste with
  | [], _ -> true
  | m'::motif', l'::liste' when m' = l' -> cherche_motif motif' liste'
  | _, _ -> false

let rec cherche_periode periode liste =
  match liste with
  | [] -> 0
  | l::liste' -> if cherche_motif (periode@[l]) liste' then List.length (periode@[l]) else cherche_periode (periode@[l]) liste'

let rec remove_n_list n list =
  if n = 0 then list else match list with
  | _::l' -> remove_n_list (n-1) l'
  | _ -> assert false

let rec periodic_stack_heuristic stack = let n = cherche_periode [] stack in if n > 0 then periodic_stack_heuristic (remove_n_list n stack) else stack
let cfa n stack = if n < 0 then stack else []


let usage_msg = "cps <file1> [<file2>] ... -o <output>"
let verbose = ref false
let input_files = ref []
let log_file = ref ""
let output_file = ref ""
let rounds = ref 1

let stack_analysis = ref periodic_stack_heuristic
let set_cfa n = stack_analysis := cfa n

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
  ; "-cfa", Arg.Int set_cfa, "Set CFA deep."
  ]
;;

let rec step_analysis cps _vars _pointers count logchan =
  if count > 0 then begin
    Logger.start "Copying@.";
    let to_copy = Cps.PointerMap.fold (fun k (block, instr) to_copy -> if Cps.size_block block + Cps.size_instr instr < !threshold then Cps.PointerSet.add k to_copy else to_copy) cps Cps.PointerSet.empty in
    let cps, _vars, _pointers = Cps.copy_blocks cps _vars _pointers (fun p -> begin
      if Asm.PointerSet.mem p to_copy then begin
      Logger.log "Copied %d@." p; true end else false
    end) in
    Logger.stop ();
    Logger.start "Cleaning\n%!";
    let cps = Cps.clean_blocks cps in
    Logger.stop ();
    Logger.start "Analysing\n%!";
    let _cps_analysis = Analysis.join_stacks (Analysis.start_analysis !stack_analysis cps) in
    let cps = Cps.PointerMap.filter (fun k _ -> if Cps.PointerMap.mem k _cps_analysis then true else (Logger.log "Filtred k%d\n" k; false)) cps in
    (*Analysis.pp_analysis Format.std_formatter _cps_analysis;*)
    Logger.stop ();
    Logger.start "Propagating\n%!";
    let cps = Analysis.propagation_blocks cps _cps_analysis in
    Logger.stop ();
    Logger.start "Eliminating unused variables\n%!";
    (*let cps, __vars, __pointers = Cps.count_vars_instr_blocks cps in
    Array.set __pointers 0 1;
    let cps = Cps.elim_unused_blocks cps __pointers in*)
    Logger.stop ();
    Logger.start "Cleaning\n%!";
    let cps = Cps.clean_blocks cps in
    Logger.stop ();
    step_analysis cps _vars _pointers (count-1) logchan
  end else cps, _vars, _pointers

let _ =
  Arg.parse speclist anon_fun usage_msg;
  let logchan = if !log_file = "" then stdout else open_out !log_file in
  for i = 0 to List.length !input_files - 1 do
    let input_file = (List.nth !input_files i) in
    let ic = open_in input_file in
    let source = Lexing.from_channel ic in
    try
      Logger.start "AST@.";
      let ast = Parser.file Lexer.tokens source in
      Ast.pp_expr (Format.formatter_of_out_channel (open_out (input_file ^ ".ast"))) ast;
      Logger.log "%s.ast@." input_file;
      Logger.stop ();
      Logger.start "CST@.";
      let cst, _vars, _subs, _fvs = Ast.expr_to_cst ast (Seq.ints 0) Ast.VarMap.empty Ast.TagMap.empty Ast.VarMap.empty in
      Cst.pp_expr (List.fold_left (fun map (s, v) -> Cps.VarMap.add v s map) _subs (Ast.VarMap.fold (fun i v subs -> (i, v)::subs) _fvs [])) (Format.formatter_of_out_channel (open_out (input_file ^ ".cst"))) cst;
      Logger.log "%s.cst@." input_file;
      Logger.stop ();
      Logger.start "CPS@.";
      let var0, _vars = match Seq.uncons _vars with
      | Some (var0, _vars) -> var0, _vars
      | None -> assert false in
      let _pointer0, _pointers = match Seq.uncons (Seq.ints 0) with
      | Some (pointer0, _pointers) -> pointer0, _pointers
      | None -> assert false in
      let instr, _vars, _pointers, fvs, conts = Cst.expr_to_cps _vars _pointers Cps.VarSet.empty cst var0 (Return var0) in
      let cps = Cst.Blocks.add _pointer0 (Cps.Clos (fvs, []), instr) conts in
      let cps, _vars, _pointers = step_analysis cps _vars _pointers !rounds logchan in
      Cps.pp_blocks _subs (Format.formatter_of_out_channel (open_out (input_file ^ ".cps"))) cps;
      Logger.log "%s.cps@." input_file;
      Logger.stop ();
      Logger.start "ASM@.";
      let asm, _vars, _pointers = Cps.blocks_to_asm cps _vars _pointers in
      Logger.start "Cleaning@.";
      let asm, conts' = Asm.elim_unused_vars_blocks asm in
      Logger.stop ();
      Logger.start "Inlining@.";
      let inline_set = (Asm.PointerSet.of_list (List.map (fun (b, _) -> b) (List.filter (fun (_, count) -> count = 1) (Array.to_list (Array.mapi (fun i count -> (i, count)) conts'))))) in
      let asm = Asm.inline_blocks asm (fun p -> begin
        if Asm.PointerSet.mem p inline_set then begin
        Logger.log "Inlined %d@." p; true end else false
      end) in
      Logger.stop ();
      Logger.start "Cleaning@.";
      let asm = if !unused_vars then let cps, conts = Asm.elim_unused_vars_blocks asm in Array.set conts 0 1; Asm.elim_unused_blocks conts cps else asm in
      Logger.stop ();
      
      Logger.log "Program size: %d@." (Asm.size_blocks asm);
      Asm.pp_blocks _subs (Format.formatter_of_out_channel (open_out (input_file ^ ".asm"))) asm;
      Logger.log "%s.asm@." input_file;
      Logger.stop ();

      (* Initial environment including free variables. *)
      let env = Asm.Tuple (List.map (fun fv -> begin
        Printf.printf "%s = " (Env.get_var (Ast.VarMap.bindings _fvs) fv);
        let i = int_of_string (read_line ()) in
        Asm.Int i
      end) (Cps.fvs_to_list fvs)) in

      let benchmark = {
        Asm.const =  0;
        write = 0;
        read = 0;
        add =  0;
        sub =  0;
        push =  0;
        pop =  0;
        jmp =  0
      } in

      let b, e = Cps.PointerMap.find 0 asm in
      let x = List.hd b in
      let r = Asm.interp [] e (Cps.VarMap.singleton x env) asm benchmark in
    
      Asm.pp_benchmark benchmark Format.std_formatter;
      Asm.pp_value Format.std_formatter r;
      Format.fprintf Format.std_formatter "\n%!"
    with
    | Parsing.Parse_error -> Printf.fprintf stderr "Erreur de parsing au caractère %d.\n" source.Lexing.lex_curr_pos
    | Failure s -> Printf.fprintf stderr "Failure: %s.\n" s
  done
;;

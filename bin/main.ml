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
let rounds = ref 1

let stack_analysis = ref periodic_stack_heuristic
let set_cfa n = stack_analysis := cfa n

let anon_fun filename = input_files := filename :: !input_files

let copy_conts = ref ([] : int list)

let copy k = copy_conts := k :: !copy_conts

let inline_conts = ref ([] : int list)

let inline k = inline_conts := k :: !inline_conts

let threshold = ref 5

let speclist = [
  "-verbose", Arg.Set verbose, "Output debug information.";
  "-max", Arg.Set_int threshold, "Copy blocks smaller than specified threshold.";
  "-copy", Arg.Int copy, "Copy specified block.";
  "-rounds", Arg.Set_int rounds, "Repeat analysis this many times (default 1).";
  "-cfa", Arg.Int set_cfa, "Set CFA deep.";
  "-inline", Arg.Int inline, "Inline specified block."
]

let rec step_analysis (blocks: Cps.blocks) (vars: Cps.var Seq.t) (pointers: Cps.pointer Seq.t) (rounds: int) =
  if rounds > 0 then begin
    (* Copy blocks *)
    Logger.start "Copying@.";
    let to_copy = Cps.PointerSet.union (Cps.PointerSet.of_list !copy_conts) (Cps.PointerMap.fold (fun k (block, instr) to_copy -> if Cps.size_block block + Cps.size_instr instr < !threshold then Cps.PointerSet.add k to_copy else to_copy) blocks Cps.PointerSet.empty) in
    let blocks, vars, pointers = Cps.copy_blocks blocks vars pointers (fun p -> begin
      if Asm.PointerSet.mem p to_copy then begin
      Logger.log "Copied %d@." p; true end else false
    end) in
    Logger.stop ();

    (* Remove aliases before analysis *)
    Logger.start "Removing aliases@.";
    let blocks = Cps.clean_blocks blocks in
    Logger.stop ();

    (* Analysis *)
    Logger.start "Analysing@.";
    let analysis = Analysis.join_stacks (Analysis.start_analysis !stack_analysis blocks) in
    let blocks = Cps.PointerMap.filter (fun k _ -> if Cps.PointerMap.mem k analysis then true else (Logger.log "Filtred k%d@." k; false)) blocks in
    if !verbose then Analysis.pp_analysis Format.std_formatter analysis;
    Logger.stop ();

    (* Propagating analysis results into blocks *)
    Logger.start "Propagating@.";
    let blocks = Analysis.propagation_blocks blocks analysis in
    Logger.stop ();

    (* Eliminating unused variables, not working #9e15aa9ed5d2e74f20fafac363e7a7674fc60495 *)
    (*
    Logger.start "Eliminating unused variables@.";
    let blocks, __vars, __pointers = Cps.count_vars_instr_blocks blocks in
    Array.set __pointers 0 1;
    let blocks = Cps.elim_unused_blocks blocks __pointers in
    Logger.stop ();
    *)

    step_analysis blocks vars pointers (rounds-1)
  end else blocks, vars, pointers

let _ =
  Arg.parse speclist anon_fun usage_msg;

  for i = 0 to List.length !input_files - 1 do
    let input_file = (List.nth !input_files i) in
    let ic = open_in input_file in
    let lexbuf = Lexing.from_channel ic in

    try
      (* Parsing AST *)
      Logger.start "AST@.";
      let expr = Parser.file Lexer.tokens lexbuf in
      Ast.pp_expr (Format.formatter_of_out_channel (open_out (input_file ^ ".ast"))) expr;
      Logger.log "%s.ast@." input_file;
      Logger.stop ();

      (* AST to CST *)
      Logger.start "CST@.";
      let cst, vars, subs, fvs = Ast.expr_to_cst expr (Seq.ints 0) Ast.VarMap.empty Ast.TagMap.empty Ast.VarMap.empty in
      Cst.pp_expr (Ast.VarMap.fold (fun i v subs' -> Cps.VarMap.add v i subs') fvs subs) (Format.formatter_of_out_channel (open_out (input_file ^ ".cst"))) cst;
      Logger.log "%s.cst@." input_file;
      Logger.stop ();

      (* CST to CPS *)
      Logger.start "CPS@.";
      let var0, vars =
        match Seq.uncons vars with
        | Some (var0, vars) -> var0, vars
        | None -> assert false in
      let pointer0, pointers =
        match Seq.uncons (Seq.ints 0) with
        | Some (pointer0, pointers) -> pointer0, pointers
        | None -> assert false in
      let instr, vars, pointers, fvs', blocks = Cst.expr_to_cps vars pointers Cps.VarSet.empty cst var0 (Return var0) in
      let cps = Cst.Blocks.add pointer0 (Cps.Clos (fvs', []), instr) blocks in
      let cps, vars, pointers = step_analysis cps vars pointers !rounds in
      Cps.pp_blocks subs (Format.formatter_of_out_channel (open_out (input_file ^ ".cps"))) cps;
      Logger.log "%s.cps@." input_file;
      Logger.stop ();

      (* CPS to ASM *)
      Logger.start "ASM@.";
      let asm, _, _ = Cps.blocks_to_asm cps vars pointers in
      Logger.start "Cleaning@.";
      let asm, blocks' = Asm.elim_unused_vars_blocks asm in
      Logger.stop ();
      Logger.start "Inlining@.";
      let inline_set = Asm.PointerSet.of_list (List.map (fun (b, _) -> b) (List.filter (fun (_, count) -> count = 1) (Array.to_list (Array.mapi (fun i count -> (i, count)) blocks')))) in
      let asm = Asm.inline_blocks asm (fun p -> begin
        if Asm.PointerSet.mem p inline_set then begin
        Logger.log "Inlined %d@." p; true end else false
      end) in
      Logger.stop ();
      Logger.start "Cleaning@.";
      let asm = let cps, blocks = Asm.elim_unused_vars_blocks asm in Array.set blocks 0 1; Asm.elim_unused_blocks blocks cps in
      Logger.stop ();
      Logger.log "Program size: %d@." (Asm.size_blocks asm);
      Asm.pp_blocks subs (Format.formatter_of_out_channel (open_out (input_file ^ ".asm"))) asm;
      Logger.log "%s.asm@." input_file;
      Logger.stop ();

      (* Initial environment including free variables. *)
      let env = Asm.Tuple (List.map (fun fv -> begin
        Printf.printf "%s = " (Env.get_var (Ast.VarMap.bindings fvs) fv);
        let i = int_of_string (read_line ()) in
        Asm.Int i
      end) (Cps.fvs_to_list fvs')) in

      let benchmark: Asm.benchmark = {
        const =  0;
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
      Format.fprintf Format.std_formatter "@."
    with
    | Parsing.Parse_error -> Printf.fprintf stderr "Erreur de parsing au caractère %d.\n" lexbuf.Lexing.lex_curr_pos
    | Failure s -> Printf.fprintf stderr "Failure: %s.\n" s
  done

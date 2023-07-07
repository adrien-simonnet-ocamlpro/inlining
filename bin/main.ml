

let usage_msg = "cps [-prop] [-clean] [-eval] [-ast] <file1> [<file2>] ... -o <output>"
let verbose = ref false
let input_files = ref []
let log_file = ref ""
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

let threshold = ref 0

let speclist =
  [ "-verbose", Arg.Set verbose, "Output debug information"
  ; "-o", Arg.Set_string output_file, "Set output file name"
  ; "-log", Arg.Set_string log_file, "Set log file name"
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
  ; "-max", Arg.Set_int threshold, "Copy blocks smaller than specified threshold"
  ]
;;

let _ =
  Arg.parse speclist anon_fun usage_msg;
  let logchan = if !log_file = "" then stdout else open_out !log_file in
  let outchan = if !output_file = "" then stdout else open_out !output_file in
  for i = 0 to List.length !input_files - 1 do
    let _ = Array.length Sys.argv = 3 in
    let entree = open_in (List.nth !input_files i) in
    let source = Lexing.from_channel entree in
    try
      Printf.fprintf logchan "----- AST -----\n\n";
      let ast = Parser.programme Lexer.jetons source in
      Ast.pp_expr (Format.formatter_of_out_channel logchan) ast;
      if !show_ast then Ast.pp_expr (Format.formatter_of_out_channel outchan) ast
      else begin
        Printf.fprintf logchan "\n\n----- CST -----\n\n";
        let cst, _vars, _subs, _fvs = Ast.expr_to_cst ast (Seq.ints 0) Ast.VarMap.empty Ast.TagMap.empty in
        Cst.pp_expr (List.fold_left (fun map (s, v) -> Cps.VarMap.add v s map) _subs (Ast.VarMap.fold (fun i v subs -> (i, v)::subs) _fvs [])) (Format.formatter_of_out_channel logchan) cst;
        if !show_cst then Cst.pp_expr (List.fold_left (fun map (s, v) -> Cps.VarMap.add v s map) _subs (Ast.VarMap.fold (fun i v subs -> (i, v)::subs) _fvs [])) (Format.formatter_of_out_channel outchan) cst
        else begin
          Printf.fprintf logchan "\n----- CPS -----\n";
          let var0, _vars = match Seq.uncons _vars with
          | Some (var0, _vars) -> var0, _vars
          | None -> assert false in
          let _pointer0, _pointers = match Seq.uncons (Seq.ints 0) with
          | Some (pointer0, _pointers) -> pointer0, _pointers
          | None -> assert false in
          let expr, _vars, _pointers, fv, conts = Cst.to_cps _vars _pointers [] cst var0 (Return var0) in
          let cps = Cst.add_block _pointer0 (Cps.Cont fv, expr) conts in
          let cps = if !unused_vars then Cps.clean_blocks cps else cps in
          let _cps_analysis = if !analysis then let a = Analysis.start_analysis cps in Analysis.pp_analysis (Format.formatter_of_out_channel logchan) a; a else Analysis.Analysis.empty in
          let cps = if !prop then Analysis.propagation_blocks cps _cps_analysis else cps in
          let to_copy = Cps.BlockMap.fold (fun k (block, expr) to_copy -> if Cps.size_block block + Cps.size_expr expr < !threshold then Cps.BlockSet.add k to_copy else to_copy) cps Cps.BlockSet.empty in
          Cps.BlockSet.iter (fun k -> Printf.fprintf logchan "copy %d\n%!" k) to_copy;
          let cps, _vars, _pointers = Cps.copy_blocks cps (Cps.BlockSet.union (Cps.BlockSet.of_list !copy_conts) to_copy) _vars _pointers in
          Cps.pp_blocks _subs (Format.formatter_of_out_channel logchan) cps;
          if !show_cps then Cps.pp_blocks _subs (Format.formatter_of_out_channel outchan) cps
          else begin
            Printf.fprintf logchan "\n----- ASM -----\n";
            let asm, _vars, _pointers = Cps.blocks_to_asm cps _vars _pointers in
            let asm, conts' = if !unused_vars then Asm.elim_unused_vars_blocks asm else asm, Array.make 1000 10000 in
            let asm = Asm.inline_blocks asm (Asm.BlockSet.union (Asm.BlockSet.of_list !inline_conts) (Asm.BlockSet.of_list (List.map (fun (b, _) -> b) (List.filter (fun (_, count) -> count = 1) (Array.to_list (Array.mapi (fun i count -> (i, count)) conts')))))) in
            let asm = if !unused_vars then let cps, conts = Asm.elim_unused_vars_blocks asm in Array.set conts 0 1; Asm.elim_unused_blocks conts cps else asm in
            Printf.fprintf logchan "type value =\n| Int of int\n| Tuple of value list\n| Function of (value -> value -> value)\n| Environment of value list\n| Closure of value * value\n| Constructor of int * value\n\nlet print (Int i) = Printf.printf \"%%d\" i\n\nlet add (Int a) (Int b) = Int (a + b)\n\nlet get value pos =\nmatch value with\n| Tuple vs -> List.nth vs pos\n| Environment vs -> List.nth vs pos\n| Closure (f, _) when pos = 0 -> f\n| Closure (_, env) when pos = 1 -> env\n| Constructor (tag, _) when pos = 0 -> Int tag\n| Constructor (_, env) when pos = 1 -> env\n| _ -> assert false\n\nlet call (Function k) = k\n\nlet rec ";
            Asm.pp_blocks _subs (Format.formatter_of_out_channel logchan) asm;
            Printf.fprintf logchan ";;\nk0 ()";
            if !show_asm then begin
              Printf.fprintf outchan "type value =\n| Int of int\n| Tuple of value list\n| Function of (value -> value -> value)\n| Environment of value list\n| Closure of value * value\n| Constructor of int * value\n\nlet print (Int i) = Printf.printf \"%%d\" i\n\nlet add (Int a) (Int b) = Int (a + b)\n\nlet get value pos =\nmatch value with\n| Tuple vs -> List.nth vs pos\n| Environment vs -> List.nth vs pos\n| Closure (f, _) when pos = 0 -> f\n| Closure (_, env) when pos = 1 -> env\n| Constructor (tag, _) when pos = 0 -> Int tag\n| Constructor (_, env) when pos = 1 -> env\n| _ -> assert false\n\nlet call (Function k) = k\n\nlet rec ";
              Asm.pp_blocks _subs (Format.formatter_of_out_channel outchan) asm;
              Printf.fprintf outchan ";;\nk0 ()"
            end else begin
              let init = List.map (fun fv -> let i = Printf.fprintf outchan "%s = " (Env.get_var (Ast.VarMap.bindings _fvs) fv) ; int_of_string (read_line ()) in (fv, Asm.Int i)) fv in
              let r, _benchmark = Asm.interp_blocks asm 0 init in
                Asm.pp_benchmark _benchmark (Format.formatter_of_out_channel outchan);
                Printf.fprintf outchan "Program size = %d\n" (Asm.size_blocks asm);
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

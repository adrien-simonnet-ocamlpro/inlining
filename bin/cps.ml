type var = int
type pointer = int
type tag = int
type frame = pointer * var list

module VarMap = Map.Make (Int)

type prim = Asm.prim

type named =
| Prim of prim * var list
| Var of var
| Tuple of var list
| Get of var * int
| Closure of pointer * var list
| Constructor of tag * var list

and expr =
| Let of var * named * expr
| Apply_cont of pointer * var list
| Call of var * var list * frame
| If of var * (int * pointer * var list) list * (pointer * var list)
| Match_pattern of var * (tag * pointer * var list) list * (pointer * var list)
| Return of var

type cont =
| Cont of pointer * var list * expr
| Clos of pointer * var list * var list * expr
| Return of pointer * var * var list * expr

type conts = cont list

let gen_name (var: var) (subs: string VarMap.t): string =
  match VarMap.find_opt var subs with
  | Some str -> str ^ "_" ^ (string_of_int var)
  | None -> "_" ^ (string_of_int var)

let rec pp_args ?(subs = (VarMap.empty: string VarMap.t)) ?(empty=(" ": string)) ?(split=(" ": string)) (fmt: Format.formatter) (args: var list): unit =
  match args with
  | [] -> Format.fprintf fmt "%s" empty
  | [ arg ] -> Format.fprintf fmt "%s" (gen_name arg subs)
  | arg :: args' -> Format.fprintf fmt "%s%s%a" (gen_name arg subs) split (pp_args ~split ~empty ~subs) args'

let pp_prim (subs: string VarMap.t) (fmt: Format.formatter) (prim : prim) (args: var list): unit =
  match prim, args with
  | Const x, _ -> Format.fprintf fmt "Int %d" x
  | Add, x1 :: x2 :: _ -> Format.fprintf fmt "add %s %s" (gen_name x1 subs) (gen_name x2 subs)
  | Sub, x1 :: x2 :: _ -> Format.fprintf fmt "sub %s %s" (gen_name x1 subs) (gen_name x2 subs)
  | Print, x1 :: _ -> Format.fprintf fmt "print %s" (gen_name x1 subs)
  | _ -> failwith "invalid args"  

let pp_named (subs: string VarMap.t) (fmt: Format.formatter) named =
match named with
| Prim (prim, args) -> pp_prim subs fmt prim args
| Var x -> Format.fprintf fmt "%s" (gen_name x subs)
| Tuple (args) -> Format.fprintf fmt "Tuple [%a]" (pp_args ~split: "; " ~subs ~empty:"") args
| Get (record, pos) -> Format.fprintf fmt "Get (%s, %d)" (gen_name record subs) pos
| Closure (k, env) -> Format.fprintf fmt "Closure (f%d, %a)" k (pp_args ~split:" " ~subs ~empty: "()") env
| Constructor (tag, env) -> Format.fprintf fmt "Constructor (%d, [%a])" tag (pp_args ~split: "; " ~subs ~empty: "") env

let rec pp_expr (subs: string VarMap.t) (fmt: Format.formatter) (cps : expr) : unit =
  match cps with
  | Let (var, named, expr) -> Format.fprintf fmt "\tlet %s = %a in\n%a" (gen_name var subs) (pp_named subs) named (pp_expr subs) expr
  | Apply_cont (k, args) -> Format.fprintf fmt "\tk%d %a" k (pp_args ~subs ~split: " " ~empty: "") args
  | If (var, matchs, (kf, argsf)) -> Format.fprintf fmt "\tmatch %s with%s | _ -> k%d %a" (gen_name var subs) (List.fold_left (fun acc (n, kt, argst) -> acc ^ (Format.asprintf "| Int %d -> k%d %a " n kt (pp_args ~subs ~empty: "()" ~split: " ") argst)) " " matchs) kf (pp_args ~subs ~empty: "()" ~split: " ") argsf
  | Match_pattern (var, matchs, (kf, argsf)) -> Format.fprintf fmt "\tmatch %s with%s | _ -> k%d %a" (gen_name var subs) (List.fold_left (fun acc (n, kt, argst) -> acc ^ (Format.asprintf "| Int %d -> f%d %a " n kt (pp_args ~subs ~empty: "()" ~split: " ") argst)) " " matchs) kf (pp_args ~subs ~empty: "()" ~split: " ") argsf
  | Return x -> Format.fprintf fmt "\t%s" (gen_name x subs)
  | Call (x, args, (k, kargs)) -> Format.fprintf fmt "\tk%d (%s %a) %a" k (gen_name x subs) (pp_args ~split:" " ~subs ~empty: "()") args (pp_args ~split:" " ~subs ~empty: "()") kargs

let rec pp_cont ?(join = "let rec") (subs: string VarMap.t) (fmt: Format.formatter) (cps : conts) : unit =
  match cps with
  | Cont (k, args, e1) :: conts -> Format.fprintf fmt "%s k%d %a =\n%a\n%a" join k (pp_args ~subs ~empty: "()" ~split: " ") args (pp_expr subs) e1 (pp_cont ~join: "and" subs) conts
  | Clos (k, env, args, e1) :: conts -> Format.fprintf fmt "%s f%d %a %a =\n%a\n%a" join k (pp_args ~subs ~empty: "()" ~split: " ") env (pp_args ~subs ~empty: "()" ~split: " ") args (pp_expr subs) e1 (pp_cont ~join: "and" subs) conts
  | Return (k, arg, args, e1) :: conts -> Format.fprintf fmt "%s r%d %s %a =\n%a\n%a" join k (gen_name arg subs) (pp_args ~subs ~empty: "()" ~split: " ") args (pp_expr subs) e1 (pp_cont ~join: "and" subs) conts
  | [] -> Format.fprintf fmt "%!"

let update_var (var: var) (alias: var VarMap.t): var = if VarMap.mem var alias then VarMap.find var alias else var
let update_vars (vars: var list) (alias: var VarMap.t): var list = List.map (fun var -> update_var var alias) vars

let clean_named (named: named) (alias: var VarMap.t): named =
  match named with
  | Prim (prim, vars) -> Prim (prim, update_vars vars alias)
  | Var var -> Var (update_var var alias)
  | Tuple vars -> Tuple (update_vars vars alias)
  | Get (record, pos) -> Get (update_var record alias, pos)
  | Closure (k, vars) -> Closure (k, update_vars vars alias)
  | Constructor (tag, vars) -> Constructor (tag, update_vars vars alias)

let rec clean_expr (expr: expr) (alias: var VarMap.t): expr =
  match expr with
  | Let (var, Var var', expr') -> clean_expr expr' (VarMap.add var var' alias)
  | Let (var, named, expr) -> Let (var, clean_named named alias, clean_expr expr alias)
  | Apply_cont (k, args) -> Apply_cont (k, update_vars args alias)
  | If (var, matchs, (kf, argsf)) -> If (update_var var alias, List.map (fun (n, k, args) -> n, k, update_vars args alias) matchs, (kf, update_vars argsf alias))
  | Match_pattern (pattern_id, matchs, (kf, argsf)) -> Match_pattern (update_var pattern_id alias, List.map (fun (n, k, args) -> n, k, update_vars args alias) matchs, (kf, update_vars argsf alias))
  | Return var -> Return (update_var var alias)
  | Call (x, args, (k, kargs)) -> Call (update_var x alias, update_vars args alias, (k, update_vars kargs alias))

let rec clean_cont (cps: conts): conts =
  match cps with
  | Cont (k', args', e1) :: e2 -> Cont (k', args', clean_expr e1 (VarMap.empty)) :: clean_cont e2
  | Clos (k', body_free_variables, args', e1) :: e2 -> Clos (k', body_free_variables, args', clean_expr e1 (VarMap.empty)) :: clean_cont e2
  | Return (k', result, args', e1) :: e2 -> Return (k', result, args', clean_expr e1 (VarMap.empty)) :: clean_cont e2
  | [] -> []

let inc (vars: var Seq.t): var * var Seq.t =
  match Seq.uncons vars with
  | None -> assert false
  | Some (i, vars') -> i, vars'

let rec named_to_asm (var: var) (named: named) (expr: expr) (vars: var Seq.t): Asm.expr * var Seq.t =
  match named with
  | Prim (prim, args) -> begin
      let asm, vars = expr_to_asm expr vars in
      Let (var, Prim (prim, args), asm), vars
    end
  | Var x -> begin
      let asm, vars = expr_to_asm expr vars in
      Let (var, Var x, asm), vars
    end
  | Tuple args -> begin
      let asm, vars = expr_to_asm expr vars in
      Let (var, Tuple args, asm), vars
    end
  | Get (record, pos) -> begin
      let asm, vars = expr_to_asm expr vars in
      Let (var, Get (record, pos), asm), vars
    end
  | Closure (k, env) -> begin
      let k_id, vars = inc vars in
      let env_id, vars = inc vars in
      let asm, vars = expr_to_asm expr vars in
      Let (k_id, Pointer k, Let (env_id, Tuple env, Let (var, Tuple [k_id; env_id], asm))), vars
    end
  | Constructor (tag, env) -> begin
      let tag_id, vars = inc vars in
      let env_id, vars = inc vars in
      let asm, vars = expr_to_asm expr vars in
      Let (tag_id, Prim (Const tag, []), Let (env_id, Tuple env, Let (var, Tuple [tag_id; env_id], asm))), vars
    end

and expr_to_asm (cps: expr) (vars: var Seq.t): Asm.expr * int Seq.t =
  match cps with
  | Let (var, named, expr) -> named_to_asm var named expr vars
  | Apply_cont (k, args) -> Apply_direct (k, args, []), vars
  | If (var, matchs, (kf, argsf)) -> If (var, matchs, (kf, argsf), []), vars
  | Match_pattern (cons, matchs, (kf, argsf)) -> begin
      let tag_id, vars = inc vars in
      let payload_id, vars = inc vars in
      Asm.Let (tag_id, Get (cons, 0), (Asm.Let (payload_id, Get (cons, 1), If (tag_id, List.map (fun (n, k, args) -> (n, k, payload_id :: args)) matchs, (kf, argsf), [])))), vars
    end
  | Return var -> Return var, vars
  | Call (clos, args, frame) -> begin
      let k_id, vars = inc vars in
      let env_id, vars = inc vars in
      Let (k_id, Get (clos, 0), Let (env_id, Get (clos, 1), Apply_indirect (k_id, env_id :: args, [frame]))), vars
    end

let rec cont_to_asm (cps: conts) (vars: var Seq.t): Asm.cont * var Seq.t =
  match cps with
  | Cont (k', args', e1) :: e2 -> begin
      let asm1, vars = expr_to_asm e1 vars in
      let asm2, vars = cont_to_asm e2 vars in
      Let_cont (k', args', asm1, asm2), vars
    end
  | Return (k', arg, args', e1) :: e2 -> begin
      let asm1, vars = expr_to_asm e1 vars in
      let asm2, vars = cont_to_asm e2 vars in
      Let_cont (k', arg :: args', asm1, asm2), vars
    end
  | Clos (k', body_free_variables, args', e1) :: e2 -> begin
      let environment_id, vars = inc vars in
      let asm1, vars = expr_to_asm e1 vars in
      let asm2, vars = cont_to_asm e2 vars in
      let body = List.fold_left (fun cps' (pos, body_free_variable) -> Asm.Let (body_free_variable, Asm.Get (environment_id, pos), cps')) asm1 (List.mapi (fun i fv -> i, fv) body_free_variables) in
      Let_cont (k', environment_id :: args', body, asm2), vars
    end
  | [] -> End, vars

(* Identifiers *)

type var = string
type tag = string

(* Modules *)

(* Map for variables. *)
module VarMap = Map.Make (String)

(* Map for tags. *)
module TagMap = Map.Make (String)

(* Map for abstractions (var -> Cst.var). *)
module Abs = struct
  type t = Cst.var VarMap.t
  let empty = VarMap.empty
  let singleton (var: var) = VarMap.singleton var
  let add (var: var) (var': Cst.var) (abs: t): t = VarMap.add var var' abs
  let mem (var: var) (abs: t): bool = VarMap.mem var abs
  let find (var: var) (abs: t): Cst.var = VarMap.find var abs
  let union (abs1: t) (abs2: t) = VarMap.union (fun v i1 i2 -> failwith (Printf.sprintf "Ast.Abs.union: %d and %d are both substitions for %s." i1 i2 v)) abs1 abs2
end

(* Map for substitutions (Cst.var -> var). *)
module Subs = struct
  type t = var Cst.VarMap.t
  let empty = Cst.VarMap.empty
  let add (v1: Cst.var) (v2: var) (subs: t): t = Cst.VarMap.add v1 v2 subs
  let union (subs1: t) (subs2: t) = Cst.VarMap.union (fun i v1 v2 -> failwith (Printf.sprintf "Ast.Subs.union: %s and %s are both substitued by %d." v1 v2 i)) subs1 subs2
end

(* Map for free variables (var -> Cst.var). *)
module Fvs = Abs

(* AST *)

type binary_operator =
| Add
| Sub

type match_pattern =
| Deconstructor of tag * var list
| Joker of var

type expr =
| Unit
| Var of var
| Fun of var list * expr
| App of expr * expr
| Tuple of expr list
| Let of var * expr * expr
| Let_tuple of var list * expr * expr
| Let_rec of (var * expr) list * expr
| Int of int
| Binary of binary_operator * expr * expr
| If of expr * expr * expr
| Type of var * (var * var list) list * expr
| Constructor of tag * (expr list)
| Match of expr * (match_pattern * expr) list

(* Pretty printers *)

let pp_binary_operator (fmt: Format.formatter) (operator: binary_operator): unit =
  match operator with
  | Add -> Format.fprintf fmt "+"
  | Sub -> Format.fprintf fmt "-"

let pp_match_pattern (fmt: Format.formatter) (pattern: match_pattern): unit =
  match pattern with
  | Deconstructor (name, []) -> Format.fprintf fmt "%s" name
  | Deconstructor (name, [ var ]) -> Format.fprintf fmt "%s %s" name var
  | Deconstructor (name, var :: vars) -> Format.fprintf fmt "%s (%s" name var; List.iter (fun var' -> Format.fprintf fmt ", %s" var') vars; Format.fprintf fmt ")"
  | Joker s -> Format.fprintf fmt "%s" s

let pp_args (fmt: Format.formatter) (args: var list): unit =
  match args with
  | [] -> Format.fprintf fmt "()"
  | [ arg ] -> Format.fprintf fmt "%s" arg
  | arg :: args' -> Format.fprintf fmt "%s" arg; List.iter (fun arg' -> Format.fprintf fmt " %s" arg') args'

let rec pp_expr fmt expr =
  match expr with
  | Unit -> Format.fprintf fmt "()"
  | Int i -> Format.fprintf fmt "%d%!" i
  | Binary (op, a, b) -> Format.fprintf fmt "(%a %a %a)%!" pp_expr a pp_binary_operator op pp_expr b
  | Fun (args, e) -> Format.fprintf fmt "(fun %a -> %a)%!" pp_args args pp_expr e
  | Tuple [] -> Format.fprintf fmt "()"
  | Tuple [expr] -> Format.fprintf fmt "%a" pp_expr expr
  | Tuple (expr :: exprs) -> Format.fprintf fmt "(%a" pp_expr expr; List.iter (fun e' -> Format.fprintf fmt ", %a" pp_expr e') exprs; Format.fprintf fmt ")"
  | Var x -> Format.fprintf fmt "%s%!" x
  | Let (var, e1, e2) -> Format.fprintf fmt "let %s = %a in\n\n%a%!" var pp_expr e1 pp_expr e2
  | Let_tuple ([], e1, e2) -> Format.fprintf fmt "let () = %a in\n\n%a%!" pp_expr e1 pp_expr e2
  | Let_tuple ([var], e1, e2) -> Format.fprintf fmt "let %s = %a in\n\n%a%!" var pp_expr e1 pp_expr e2
  | Let_tuple (var :: vars, e1, e2) -> Format.fprintf fmt "let %s" var; List.iter (fun var -> Format.fprintf fmt ", %s" var) vars; Format.fprintf fmt " = %a in\n\n%a)%!"  pp_expr e1 pp_expr e2
  | Let_rec (bindings, expr0) -> begin
      Format.fprintf fmt "let rec"; begin
        match bindings with
        | [] -> Format.fprintf fmt "\n"
        | [var, e] -> Format.fprintf fmt " %s = %a" var pp_expr e
        | (var, e) :: bindings' -> begin
            Format.fprintf fmt " %s = %a" var pp_expr e;
            List.iter (fun (var, expr) -> Format.fprintf fmt "\nand %s = %a" var pp_expr expr) bindings'
          end;
      end;
      Format.fprintf fmt " in\n\n%a%!" pp_expr expr0
    end
  | If (cond, t, f) -> Format.fprintf fmt "(if %a = 0 then %a else %a)%!" pp_expr cond pp_expr t pp_expr f
  | App (e1, e2) -> Format.fprintf fmt "(%a %a)%!" pp_expr e1 pp_expr e2
  | Type (name, constructors, expr) -> begin
      Format.fprintf fmt "type %s =" name; List.iter (fun (cname, ctype) -> begin
        match ctype with
        | [] -> Format.fprintf fmt "\n| %s" cname
        | [t] -> Format.fprintf fmt "\n| %s of %s" cname t
        | t :: ts -> Format.fprintf fmt "\n| %s of %s" cname t; List.iter (fun t -> Format.fprintf fmt " * %s" t) ts
      end) constructors;
      Format.fprintf fmt "\n\n%a%!" pp_expr expr
    end
  | Constructor (name, []) -> Format.fprintf fmt "%s" name
  | Constructor (name, [ e ]) -> Format.fprintf fmt "%s %a" name pp_expr e
  | Constructor (name, e :: exprs') -> Format.fprintf fmt "%s (%a" name pp_expr e; List.iter (fun e' -> Format.fprintf fmt ", %a" pp_expr e') exprs'; Format.fprintf fmt ")"
  | Match (e, matchs) -> begin
      Format.fprintf fmt "(match %a with" pp_expr e;
      List.iter (fun (pattern, e) -> Format.fprintf fmt "\n| %a -> %a" pp_match_pattern pattern pp_expr e) matchs;
      Format.fprintf fmt ")%!"
    end
  
let inc (vars: Cst.var Seq.t): Cst.var * Cst.var Seq.t =
  match Seq.uncons vars with
  | None -> assert false
  | Some (i, vars') -> i, vars'

(* AST conversion to CST *)

let binary_to_cst (binary: binary_operator): Cst.binary_operator =
  match binary with
  | Add -> Add
  | Sub -> Sub

let rec expr_to_cst (expr: expr) (vars: Cst.var Seq.t) (substitutions: Abs.t) (constructors: Cst.var TagMap.t): Cst.expr * Cst.var Seq.t * var Cst.VarMap.t * Cst.var VarMap.t =
  match expr with
  | Unit -> Unit, vars, Subs.empty, Abs.empty
  | Int i -> Int i, vars, Subs.empty, Abs.empty
  | Binary (op, e1, e2) -> begin
      let e1', vars, subs1, fvs1 = expr_to_cst e1 vars substitutions constructors in
      let e2', vars, subs2, fvs2 = expr_to_cst e2 vars (Fvs.union fvs1 substitutions) constructors in
      Binary (binary_to_cst op, e1', e2'), vars, Subs.union subs1 subs2, Fvs.union fvs1 fvs2
    end
  | Fun (args, e) -> begin
      let vars, args_ids = List.fold_left_map (fun vars _ -> begin
        let arg_id, vars = inc vars in
        vars, arg_id
      end) vars args in
      let e', vars, subs, fvs = expr_to_cst e vars (List.fold_left (fun substitutions' (arg, arg_id) -> Abs.add arg arg_id substitutions') substitutions (List.combine args args_ids)) constructors in
      Fun (args_ids, e'), vars, (List.fold_left (fun subs' (arg, arg_id) -> Subs.add arg_id arg subs') subs (List.combine args args_ids)), fvs
    end
  | Tuple exprs -> begin
      let (vars, subs, fvs), exprs' = List.fold_left_map (fun (vars, subs, fvs) expr -> begin
        let expr', vars, subs', fvs' = expr_to_cst expr vars (Fvs.union fvs substitutions) constructors in
        (vars, Subs.union subs' subs, Fvs.union fvs' fvs), expr'
      end) (vars, Subs.empty, Abs.empty) exprs in
      Tuple exprs', vars, subs, fvs
    end
  | Var x -> if Abs.mem x substitutions then Var (Abs.find x substitutions), vars, Subs.empty, Fvs.empty else begin
      let var_id, vars = inc vars in
      Var (var_id), vars, Subs.empty, Fvs.singleton x var_id
    end
  | Constructor (str, exprs) -> begin
      let index = TagMap.find str constructors in
      let (vars, subs, fvs), exprs' = List.fold_left_map (fun (vars, subs, fvs) expr -> begin
        let expr', vars, subs', fvs' = expr_to_cst expr vars (Fvs.union fvs substitutions) constructors in
        (vars, Subs.union subs' subs, Fvs.union fvs' fvs), expr'
      end) (vars, Subs.empty, Abs.empty) exprs in
      Constructor (index, exprs'), vars, subs, fvs
    end
  | Let (var, e1, e2) -> begin
      let var_id, vars = inc vars in
      let e1', vars, subs1, fvs1 = expr_to_cst e1 vars substitutions constructors in
      let e2', vars, subs2, fvs2 = expr_to_cst e2 vars (Abs.add var var_id (Fvs.union fvs1 substitutions)) constructors in
      Let (var_id, e1', e2'), vars, Subs.add var_id var (Subs.union subs1 subs2), Fvs.union fvs1 fvs2
    end
  | Let_tuple (vars', e1, e2) -> begin
      let vars, vars_ids = List.fold_left_map (fun vars _ -> begin
        let arg_id, vars = inc vars in
        vars, arg_id
      end) vars vars' in
      let e1', vars, subs1, fvs1 = expr_to_cst e1 vars substitutions constructors in
      let e2', vars, subs2, fvs2 = expr_to_cst e2 vars (List.fold_left (fun substitutions (var, var_id) -> Abs.add var var_id substitutions) (Fvs.union fvs1 substitutions) (List.combine vars' vars_ids)) constructors in
      Let_tuple (vars_ids, e1', e2'), vars, (List.fold_left (fun subs' (arg, arg_id) -> Subs.add arg_id arg subs') (Subs.union subs1 subs2) (List.combine vars' vars_ids)), Fvs.union fvs1 fvs2
    end
  | If (e1, e2, e3) -> begin
      let e1', vars, subs1, fvs1 = expr_to_cst e1 vars substitutions constructors in
      let e2', vars, subs2, fvs2 = expr_to_cst e2 vars (Fvs.union fvs1 substitutions) constructors in
      let e3', vars, subs3, fvs3 = expr_to_cst e3 vars (Fvs.union fvs2 (Fvs.union fvs1 substitutions)) constructors in
      If (e1', e2', e3'), vars, Subs.union subs1 (Subs.union subs2 subs3), Fvs.union fvs1 (Fvs.union fvs2 fvs3)
    end
  | App (e1, e2) -> begin
      let e1', vars, subs1, fvs1 = expr_to_cst e1 vars substitutions constructors in
      let e2', vars, subs2, fvs2 = expr_to_cst e2 vars (Fvs.union fvs1 substitutions) constructors in
      App (e1', e2'), vars, Subs.union subs1 subs2, Fvs.union fvs1 fvs2
    end
  | Let_rec (bindings, e) -> begin
      let vars, bindings_ids = List.fold_left_map (fun vars (var, _) -> begin
        let var_id, vars = inc vars in
        vars, (var, var_id)
      end) vars bindings in
      let (vars, subs, fvs), bindings' = List.fold_left_map (fun (vars, subs, fvs) ((_, expr), (_, var_id)) -> begin
        let expr', vars, subs', fvs' = expr_to_cst expr vars (Abs.union (List.fold_left (fun subs'' (b, b_id) -> Abs.add b b_id subs'') Abs.empty bindings_ids) (Fvs.union fvs substitutions)) constructors in
        (vars, Subs.union subs' subs, Fvs.union fvs' fvs), (var_id, expr')
      end) (vars, Subs.empty, Abs.empty) (List.combine bindings bindings_ids) in
      let e', vars, subs', fvs' = expr_to_cst e vars (Abs.union (List.fold_left (fun subs'' (b, b_id) -> Abs.add b b_id subs'') Abs.empty bindings_ids) (Fvs.union fvs substitutions)) constructors in
      Let_rec (bindings', e'), vars, Subs.union (List.fold_left (fun subs'' (b, b_id) -> Subs.add b_id b subs'') Subs.empty bindings_ids) (Subs.union subs subs'), Fvs.union fvs fvs'
    end
  | Match (x, branchs) -> begin
      let default_expr = if List.exists (fun (t, _) -> match t with | Joker _ -> true | _ -> false) branchs then 
        let _, default_expr = List.find (fun (t, _) -> match t with
          | Joker _ -> true | _ -> false) branchs in default_expr
        else Int 123456789 in
      
      let (vars, subs, fvs), branchs' = List.fold_left_map (fun (vars, subs, fvs) (pattern, e) -> begin
        match pattern with
        | Deconstructor (constructor_name, payload_values) -> begin
            let vars, args_ids = List.fold_left_map (fun vars var -> begin
              let var_id, vars = inc vars in
              vars, (var, var_id)
            end) vars payload_values in
            let pattern_index = TagMap.find constructor_name constructors in
            let e', vars, subs', fvs' = expr_to_cst e vars (Abs.union (List.fold_left (fun subs'' (b, b_id) -> Abs.add b b_id subs'') Abs.empty args_ids) (Fvs.union fvs substitutions)) constructors in
            (vars, Subs.union (List.fold_left (fun subs'' (b, b_id) -> Subs.add b_id b subs'') Subs.empty args_ids) (Subs.union subs subs'), Fvs.union fvs fvs'), (pattern_index, List.map (fun (_, arg_id) -> arg_id) args_ids, e')
          end
        | _ -> assert false
      end) (vars, Subs.empty, Abs.empty) (List.filter (fun (pattern, _) -> match pattern with | Deconstructor _ -> true | _ -> false) branchs) in
      let e', vars, subs', fvs' = expr_to_cst x vars (Fvs.union fvs substitutions) constructors in
      let default_expr, vars, subs'', fvs'' = expr_to_cst default_expr vars (Fvs.union fvs' (Fvs.union fvs substitutions)) constructors in
      Match (e', branchs', default_expr), vars, Subs.union subs (Subs.union subs' subs''), Fvs.union fvs (Fvs.union fvs' fvs'')
    end
  | Type (_, constructors', expr) -> expr_to_cst expr vars substitutions (List.fold_left (fun constructors'' ((constructor_name, _), index) -> TagMap.add constructor_name index constructors'') constructors (List.mapi (fun i v -> v, i) constructors'))

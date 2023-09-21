open Utils

(* Identifier for variables *)
type var = string

(* Identifier for constructor tags *)
type tag = string

(* Identifier for type names *)
type type_name = string

(* Identifier for field names *)
type field_name = string

(* Module for variables. *)
module VarMap = Map.Make (String)

(* Module for tags. *)
module TagMap = Map.Make (String)

(* Module for abstractions (var -> Cst.var). *)
module Abstractions = struct
  type t = Cst.var VarMap.t
  let empty = VarMap.empty
  let singleton (var: var) =
    VarMap.singleton var
  let add (var: var) (var': Cst.var) (abs: t): t =
    VarMap.add var var' abs
  let mem (var: var) (abs: t): bool =
    VarMap.mem var abs
  let find (var: var) (abs: t): Cst.var =
    VarMap.find var abs

  exception AbstractionsNotUnique of var * int * int
  let union (abs1: t) (abs2: t) =
    VarMap.union (fun v i1 i2 -> raise (AbstractionsNotUnique (v, i1, i2))) abs1 abs2
end

(* Module for substitutions (Cst.var -> var). *)
module Substitutions = struct
  type t = var Cst.VarMap.t
  let empty = Cst.VarMap.empty
  let add (v1: Cst.var) (v2: var) (subs: t): t =
    Cst.VarMap.add v1 v2 subs

  exception SubstitutionsNotUnique of int * var * var
  let union (subs1: t) (subs2: t) =
    Cst.VarMap.union (fun i v1 v2 -> raise (SubstitutionsNotUnique (i, v1, v2))) subs1 subs2
end

(* Module for free variables (var -> Cst.var). *)
module FreeVariables = Abstractions

(* Binary operators *)
type binary_operator =
| Add (* + *)
| Sub (* - *)

(* Match patterns *)
type match_pattern =
| Deconstructor of tag * var list (* Catch pattern *)
| Joker of var (* Catch all *)

(* Type definitions *)
type type_definition =
| Type_name of type_name (* Alias *)
| Star of type_definition * type_definition (* Tuple *)
| Arrow of type_definition * type_definition (* Fun *)

(* Type declarations *)
type type_declaration =
| Alias of type_definition (* Alias *)
| Data of (var * type_definition list) list (* Data type with constructors *)
| Record of (field_name * type_definition) list (* Record type with fields *)

(* Expressions *)
type expr =
| Var of var (* Variable *)
| Fun of var list * expr (* Abstraction *)
| App of expr * expr (* Application *)
| Tuple of expr list (* Tuple of expressions *)
| Let of var * expr * expr (* Let *)
| Let_tuple of var list * expr * expr (* Let unpack tuple *)
| Let_rec of (var * expr) list * expr (* Let rec of functions *)
| Int of int (* Integer *)
| Binary of binary_operator * expr * expr (* Binary operation *)
| If of expr * expr * expr (* If cond then else *)
| Type of type_name * type_declaration * expr (* Typle declaration *)
| Constructor of tag * expr list (* Constructor application *)
| Match of expr * (match_pattern * expr) list (* Pattern matching *)
| Record_construction of (field_name * expr) list (* Record construction *)
| Record_field of expr * field_name (* Read record field *)

(* Pretty printer for binary operators. *)
let pp_binary_operator (fmt: Format.formatter) (operator: binary_operator): unit =
  match operator with
  | Add -> Format.fprintf fmt "+"
  | Sub -> Format.fprintf fmt "-"

(* Pretty printer for match patterns. *)
let pp_match_pattern (fmt: Format.formatter) (pattern: match_pattern): unit =
  match pattern with
  | Deconstructor (name, []) -> Format.fprintf fmt "%s" name
  | Deconstructor (name, [ var ]) -> Format.fprintf fmt "%s %s" name var
  | Deconstructor (name, var :: vars) -> Format.fprintf fmt "%s (%s" name var; List.iter (fun var' -> Format.fprintf fmt ", %s" var') vars; Format.fprintf fmt ")"
  | Joker s -> Format.fprintf fmt "%s" s

(* Pretty printer for type definitions. *)
let rec pp_type_definition (fmt: Format.formatter) (tdef: type_definition): unit =
  match tdef with
  | Type_name n -> Format.fprintf fmt "%s" n
  | Star (tdef1, tdef2) -> Format.fprintf fmt "(%a * %a)" pp_type_definition tdef1 pp_type_definition tdef2
  | Arrow (tdef1, tdef2) -> Format.fprintf fmt "(%a -> %a)" pp_type_definition tdef1 pp_type_definition tdef2

(* Pretty printer for type declarations. *)
let pp_type_declaration (fmt: Format.formatter) (tdec: type_declaration): unit =
  match tdec with
  | Alias alias -> Format.fprintf fmt "%a" pp_type_definition alias
  | Data constructors -> begin
      List.iter (fun (cname, ctype) -> begin
        match ctype with
        | [] -> Format.fprintf fmt "\n| %s" cname
        | [t] -> Format.fprintf fmt "\n| %s of %a" cname pp_type_definition t
        | t :: ts -> begin
            Format.fprintf fmt "\n| %s of %a" cname pp_type_definition t;
            List.iter (fun t -> Format.fprintf fmt " * %a" pp_type_definition t) ts
          end
      end) constructors
    end
  | Record fields -> begin
      Format.fprintf fmt "{";
      List.iter (fun (cname, ctype) -> Format.fprintf fmt "\n\t%s: %a" cname pp_type_definition ctype) fields;
      Format.fprintf fmt "\n}%!"
    end

(* Pretty printer for expressions. *)
let rec pp_expr fmt expr =
  match expr with
  | Int i -> Format.fprintf fmt "%d%!" i
  | Binary (op, a, b) -> Format.fprintf fmt "(%a %a %a)%!" pp_expr a pp_binary_operator op pp_expr b
  | Fun ([], e) -> Format.fprintf fmt "(fun () -> %a)%!" pp_expr e
  | Fun ([arg], e) -> Format.fprintf fmt "(fun %s -> %a)%!" arg pp_expr e
  | Fun (arg :: args, e) -> begin
      Format.fprintf fmt "(fun %s" arg;
      List.iter (fun arg' -> Format.fprintf fmt " %s" arg') args;
      Format.fprintf fmt "-> %a)%!" pp_expr e
    end
  | Tuple [] -> Format.fprintf fmt "()"
  | Tuple [expr] -> Format.fprintf fmt "%a" pp_expr expr
  | Tuple (expr :: exprs) -> Format.fprintf fmt "(%a" pp_expr expr; List.iter (fun e' -> Format.fprintf fmt ", %a" pp_expr e') exprs; Format.fprintf fmt ")"
  | Var x -> Format.fprintf fmt "%s%!" x
  | Let (var, e1, e2) -> Format.fprintf fmt "let %s = %a in\n\n%a%!" var pp_expr e1 pp_expr e2
  | Let_tuple ([], e1, e2) -> Format.fprintf fmt "let () = %a in\n\n%a%!" pp_expr e1 pp_expr e2
  | Let_tuple ([var], e1, e2) -> Format.fprintf fmt "let %s = %a in\n\n%a%!" var pp_expr e1 pp_expr e2
  | Let_tuple (var :: vars, e1, e2) -> Format.fprintf fmt "let %s" var; List.iter (fun var -> Format.fprintf fmt ", %s" var) vars; Format.fprintf fmt " = %a in\n\n%a)%!"  pp_expr e1 pp_expr e2
  | Let_rec (bindings, expr') -> begin
      Format.fprintf fmt "let rec"; begin
        match bindings with
        | [] -> Format.fprintf fmt "\n"
        | [var, e] -> Format.fprintf fmt " %s = %a" var pp_expr e
        | (var, e) :: bindings' -> begin
            Format.fprintf fmt " %s = %a" var pp_expr e;
            List.iter (fun (var, expr) -> Format.fprintf fmt "\nand %s = %a" var pp_expr expr) bindings'
          end;
      end;
      Format.fprintf fmt " in\n\n%a%!" pp_expr expr'
    end
  | If (cond, t, f) -> Format.fprintf fmt "(if %a = 0 then %a else %a)%!" pp_expr cond pp_expr t pp_expr f
  | App (e1, e2) -> Format.fprintf fmt "(%a %a)%!" pp_expr e1 pp_expr e2
  | Type (name, tdec, expr) -> Format.fprintf fmt "type %s = %a\n\n%a" name pp_type_declaration tdec pp_expr expr
  | Constructor (name, []) -> Format.fprintf fmt "%s" name
  | Constructor (name, [ e ]) -> Format.fprintf fmt "%s %a" name pp_expr e
  | Constructor (name, e :: exprs') -> Format.fprintf fmt "%s (%a" name pp_expr e; List.iter (fun e' -> Format.fprintf fmt ", %a" pp_expr e') exprs'; Format.fprintf fmt ")"
  | Match (e, matchs) -> begin
      Format.fprintf fmt "(match %a with" pp_expr e;
      List.iter (fun (pattern, e) -> Format.fprintf fmt "\n| %a -> %a" pp_match_pattern pattern pp_expr e) matchs;
      Format.fprintf fmt ")%!"
    end
  | Record_construction [] -> Format.fprintf fmt "{ }"
  | Record_construction [fn, e] -> Format.fprintf fmt "{ %s = %a }" fn pp_expr e
  | Record_construction ((fn, e) :: exprs') -> begin
      Format.fprintf fmt "{ %s = %a" fn pp_expr e;
      List.iter (fun (fn, e) -> Format.fprintf fmt "; %s = %a" fn pp_expr e) exprs';
      Format.fprintf fmt " }"
    end
  | Record_field (e, fname) -> Format.fprintf fmt "%a.%s" pp_expr e fname

(* Converts binary operators. *)
let binary_to_cst (binary: binary_operator): Cst.binary_operator =
  match binary with
  | Add -> Add
  | Sub -> Sub

(* Add new constructors and records to type environment. *)
let type_declaration_to_cst (tdec: type_declaration) (constructors: Cst.tag TagMap.t) (records: int VarMap.t): Cst.tag TagMap.t * int VarMap.t =
  match tdec with
  | Alias _ -> constructors, records
  | Data constructors' -> (List.fold_left (fun constructors'' ((constructor_name, _), index) -> TagMap.add constructor_name index constructors'') constructors (List.mapi (fun i v -> v, i) constructors')), records
  | Record fields -> constructors, (List.fold_left (fun records'' ((fname, _), index) -> TagMap.add (fname) index records'') records (List.mapi (fun i v -> v, i) fields))

(* Converts an expression by transforming every string to an unique identifier. *)
let rec expr_to_cst (expr: expr) (vars: Cst.var Seq.t) (substitutions: Abstractions.t) (constructors: Cst.tag TagMap.t) (records: int VarMap.t): Cst.expr * Cst.var Seq.t * Substitutions.t * FreeVariables.t =
  match expr with
  | Int i -> Int i, vars, Substitutions.empty, Abstractions.empty
  | Binary (op, e1, e2) -> begin
      let e1', vars, subs1, fvs1 = expr_to_cst e1 vars substitutions constructors records in
      let e2', vars, subs2, fvs2 = expr_to_cst e2 vars (FreeVariables.union fvs1 substitutions) constructors records in
      Binary (binary_to_cst op, e1', e2'), vars, Substitutions.union subs1 subs2, FreeVariables.union fvs1 fvs2
    end
  | Fun (args, e) -> begin
      let vars, args_ids = List.fold_left_map (fun vars _ -> begin
        let arg_id, vars = inc vars in
        vars, arg_id
      end) vars args in
      let e', vars, subs, fvs = expr_to_cst e vars (List.fold_left (fun substitutions' (arg, arg_id) -> Abstractions.add arg arg_id substitutions') substitutions (List.combine args args_ids)) constructors records in
      Fun (args_ids, e'), vars, (List.fold_left (fun subs' (arg, arg_id) -> Substitutions.add arg_id arg subs') subs (List.combine args args_ids)), fvs
    end
  | Tuple exprs -> begin
      let (vars, subs, fvs), exprs' = List.fold_left_map (fun (vars, subs, fvs) expr -> begin
        let expr', vars, subs', fvs' = expr_to_cst expr vars (FreeVariables.union fvs substitutions) constructors records in
        (vars, Substitutions.union subs' subs, FreeVariables.union fvs' fvs), expr'
      end) (vars, Substitutions.empty, Abstractions.empty) exprs in
      Tuple exprs', vars, subs, fvs
    end
  | Var x -> if Abstractions.mem x substitutions then Var (Abstractions.find x substitutions), vars, Substitutions.empty, FreeVariables.empty else begin
      let var_id, vars = inc vars in
      Var (var_id), vars, Substitutions.empty, FreeVariables.singleton x var_id
    end
  | Constructor (str, exprs) -> begin
      let index = TagMap.find str constructors in
      let (vars, subs, fvs), exprs' = List.fold_left_map (fun (vars, subs, fvs) expr -> begin
        let expr', vars, subs', fvs' = expr_to_cst expr vars (FreeVariables.union fvs substitutions) constructors records in
        (vars, Substitutions.union subs' subs, FreeVariables.union fvs' fvs), expr'
      end) (vars, Substitutions.empty, Abstractions.empty) exprs in
      Constructor (index, exprs'), vars, subs, fvs
    end
  | Let (var, e1, e2) -> begin
      let var_id, vars = inc vars in
      let e1', vars, subs1, fvs1 = expr_to_cst e1 vars substitutions constructors records in
      let e2', vars, subs2, fvs2 = expr_to_cst e2 vars (Abstractions.add var var_id (FreeVariables.union fvs1 substitutions)) constructors records in
      Let (var_id, e1', e2'), vars, Substitutions.add var_id var (Substitutions.union subs1 subs2), FreeVariables.union fvs1 fvs2
    end
  | Let_tuple (vars', e1, e2) -> begin
      let vars, vars_ids = List.fold_left_map (fun vars _ -> begin
        let arg_id, vars = inc vars in
        vars, arg_id
      end) vars vars' in
      let e1', vars, subs1, fvs1 = expr_to_cst e1 vars substitutions constructors records in
      let e2', vars, subs2, fvs2 = expr_to_cst e2 vars (List.fold_left (fun substitutions (var, var_id) -> Abstractions.add var var_id substitutions) (FreeVariables.union fvs1 substitutions) (List.combine vars' vars_ids)) constructors records in
      Let_tuple (vars_ids, e1', e2'), vars, (List.fold_left (fun subs' (arg, arg_id) -> Substitutions.add arg_id arg subs') (Substitutions.union subs1 subs2) (List.combine vars' vars_ids)), FreeVariables.union fvs1 fvs2
    end
  | If (e1, e2, e3) -> begin
      let e1', vars, subs1, fvs1 = expr_to_cst e1 vars substitutions constructors records in
      let e2', vars, subs2, fvs2 = expr_to_cst e2 vars (FreeVariables.union fvs1 substitutions) constructors records in
      let e3', vars, subs3, fvs3 = expr_to_cst e3 vars (FreeVariables.union fvs2 (FreeVariables.union fvs1 substitutions)) constructors records in
      If (e1', e2', e3'), vars, Substitutions.union subs1 (Substitutions.union subs2 subs3), FreeVariables.union fvs1 (FreeVariables.union fvs2 fvs3)
    end
  | App (e1, e2) -> begin
      let e1', vars, subs1, fvs1 = expr_to_cst e1 vars substitutions constructors records in
      let e2', vars, subs2, fvs2 = expr_to_cst e2 vars (FreeVariables.union fvs1 substitutions) constructors records in
      App (e1', e2'), vars, Substitutions.union subs1 subs2, FreeVariables.union fvs1 fvs2
    end
  | Let_rec (bindings, e) -> begin
      let vars, bindings_ids = List.fold_left_map (fun vars (var, _) -> begin
        let var_id, vars = inc vars in
        vars, (var, var_id)
      end) vars bindings in
      let (vars, subs, fvs), bindings' = List.fold_left_map (fun (vars, subs, fvs) ((_, expr), (_, var_id)) -> begin
        let expr', vars, subs', fvs' = expr_to_cst expr vars (Abstractions.union (List.fold_left (fun subs'' (b, b_id) -> Abstractions.add b b_id subs'') Abstractions.empty bindings_ids) (FreeVariables.union fvs substitutions)) constructors records in
        (vars, Substitutions.union subs' subs, FreeVariables.union fvs' fvs), (var_id, expr')
      end) (vars, Substitutions.empty, Abstractions.empty) (List.combine bindings bindings_ids) in
      let e', vars, subs', fvs' = expr_to_cst e vars (Abstractions.union (List.fold_left (fun subs'' (b, b_id) -> Abstractions.add b b_id subs'') Abstractions.empty bindings_ids) (FreeVariables.union fvs substitutions)) constructors records in
      Let_rec (bindings', e'), vars, Substitutions.union (List.fold_left (fun subs'' (b, b_id) -> Substitutions.add b_id b subs'') Substitutions.empty bindings_ids) (Substitutions.union subs subs'), FreeVariables.union fvs fvs'
    end
  | Match (x, branchs) -> begin
      let is_joker (t, _) =
        match t with
        | Joker _ -> true
        | _ -> false in
      
      let default_expr =
        match List.find_opt is_joker branchs with
        | Some (_, e) -> e
        | None -> Int 123456789 in
      
      let (vars, subs, fvs), branchs' = List.fold_left_map (fun (vars, subs, fvs) (pattern, e) -> begin
        match pattern with
        | Deconstructor (constructor_name, payload_values) -> begin
            let vars, args_ids = List.fold_left_map (fun vars var -> begin
              let var_id, vars = inc vars in
              vars, (var, var_id)
            end) vars payload_values in
            let pattern_index = TagMap.find constructor_name constructors in
            let e', vars, subs', fvs' = expr_to_cst e vars (Abstractions.union (List.fold_left (fun subs'' (b, b_id) -> Abstractions.add b b_id subs'') Abstractions.empty args_ids) (FreeVariables.union fvs substitutions)) constructors records in
            (vars, Substitutions.union (List.fold_left (fun subs'' (b, b_id) -> Substitutions.add b_id b subs'') Substitutions.empty args_ids) (Substitutions.union subs subs'), FreeVariables.union fvs fvs'), (pattern_index, List.map (fun (_, arg_id) -> arg_id) args_ids, e')
          end
        | _ -> assert false
      end) (vars, Substitutions.empty, Abstractions.empty) (List.filter (fun (pattern, _) -> match pattern with | Deconstructor _ -> true | _ -> false) branchs) in
      let e', vars, subs', fvs' = expr_to_cst x vars (FreeVariables.union fvs substitutions) constructors records in
      let default_expr, vars, subs'', fvs'' = expr_to_cst default_expr vars (FreeVariables.union fvs' (FreeVariables.union fvs substitutions)) constructors records in
      Match (e', branchs', default_expr), vars, Substitutions.union subs (Substitutions.union subs' subs''), FreeVariables.union fvs (FreeVariables.union fvs' fvs'')
    end
  | Type (_tname, tdec, e) -> begin
      let constructors, records = type_declaration_to_cst tdec constructors records in
      expr_to_cst e vars substitutions constructors records
    end
  | Record_construction fields -> begin
      let (vars, subs, fvs), exprs' = List.fold_left_map (fun (vars, subs, fvs) expr -> begin
        let expr', vars, subs', fvs' = expr_to_cst expr vars (FreeVariables.union fvs substitutions) constructors records in
        (vars, Substitutions.union subs' subs, FreeVariables.union fvs' fvs), expr'
      end) (vars, Substitutions.empty, Abstractions.empty) (List.map (fun (_, e) -> e) (List.sort (fun (fn1, _) (fn2, _) -> TagMap.find fn1 records - TagMap.find fn2 records) fields)) in
      Tuple exprs', vars, subs, fvs
    end
  | Record_field (e, fname) -> begin
      let index = TagMap.find fname records in
      let e', vars, subs, fvs = expr_to_cst e vars substitutions constructors records in
      Get (e', index), vars, subs, fvs
    end

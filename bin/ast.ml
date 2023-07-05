type var = string
type tag = string

module TagMap = Map.Make (String)
module VarMap = Map.Make (String)

type match_pattern =
| Deconstructor of tag * var list
| Joker of var

type binary_operator =
| Add
| Sub

type expr =
| Var of var
| Fun of var * expr
| App of expr * expr
| Let of var * expr * expr
| Let_rec of (var * expr) list * expr
| Int of int
| Binary of binary_operator * expr * expr
| If of expr * expr * expr
| Type of var * (var * var) list * expr
| Constructor of tag * (expr list)
| Match of expr * (match_pattern * expr) list

let pp_binary fmt operator =
  match operator with
  | Add -> Format.fprintf fmt "+"
  | Sub -> Format.fprintf fmt "-"

let rec pp_match_pattern fmt (pattern: match_pattern) =
  match pattern with
  | Deconstructor (name, []) -> Format.fprintf fmt "%s" name
  | Deconstructor (name, [var]) -> Format.fprintf fmt "%s %s" name var
  | Deconstructor (name, var::vars) -> Format.fprintf fmt "%s (%s)" name (List.fold_left (fun str var' -> str ^ ", " ^ var') var vars)
  | Joker _ -> Format.fprintf fmt "_"

and pp_constructor fmt (name, exprs: tag * expr list) =
  match exprs with
  | [] -> Format.fprintf fmt "%s" name
  | [e] -> Format.fprintf fmt "%s %a" name pp_expr e
  | e :: exprs' -> Format.fprintf fmt "%s (%a)" name (pp_exprs ~empty: "" ~split: ", ") (e :: exprs')

and pp_exprs ?(empty=(" ": string)) ?(split=(" ": string)) (fmt: Format.formatter) (exprs: expr list): unit =
  match exprs with
  | [] -> Format.fprintf fmt "%s" empty
  | [ e ] -> Format.fprintf fmt "%a" pp_expr e
  | e :: exprs' -> Format.fprintf fmt "%a%s%a" pp_expr e split (pp_exprs ~split ~empty) exprs'

and pp_expr fmt expr =
  match expr with
  | Int i -> Format.fprintf fmt "%d%!" i
  | Binary (op, a, b) -> Format.fprintf fmt "(%a %a %a)%!" pp_expr a pp_binary op pp_expr b
  | Fun (x, e) -> Format.fprintf fmt "(fun %s -> %a)%!" x pp_expr e
  | Var x -> Format.fprintf fmt "%s%!" x
  | Let (var, e1, e2) -> Format.fprintf fmt "(let %s = %a in\n%a)%!" var pp_expr e1 pp_expr e2
  | Let_rec (_bindings, expr) -> Format.fprintf fmt "(let rec in\n%a)%!" pp_expr expr
  | If (cond, t, f) -> Format.fprintf fmt "(if %a = 0 then %a else %a)%!" pp_expr cond pp_expr t pp_expr f
  | App (e1, e2) -> Format.fprintf fmt "(%a %a)%!" pp_expr e1 pp_expr e2
  | Type (name, _, expr) -> Format.fprintf fmt "type %s = %s \n\n%a%!" name "constructors" pp_expr expr
  | Constructor (name, exprs) -> Format.fprintf fmt "%a%!" pp_constructor (name, exprs)
  | Match (e, matchs) -> Format.fprintf fmt "(match %a with %!" pp_expr e; (List.iter (fun (pattern, e) -> Format.fprintf fmt "%a -> %a" pp_match_pattern pattern pp_expr e) matchs); Format.fprintf fmt ")"
  
let inc (vars: Cst.var Seq.t): Cst.var * Cst.var Seq.t =
  match Seq.uncons vars with
  | None -> assert false
  | Some (i, vars') -> i, vars'

let has_var_name = Env.has_var

let get_var_id = Env.get_value

let binary_to_cst (binary: binary_operator): Cst.binary_operator =
  match binary with
  | Add -> Add
  | Sub -> Sub

let empty_subs = Cst.VarMap.empty
let add_subs = Cst.VarMap.add
let union_subs = Cst.VarMap.union (fun i v1 v2 -> failwith (Printf.sprintf "Ast.subs_union: %s and %s are both substitued with %d." v1 v2 i))

let rec expr_to_cst (expr: expr) (vars: Cst.var Seq.t) (substitutions: (var * Cst.var) list) (constructors: Cst.var TagMap.t): Cst.expr * Cst.var Seq.t * var Cst.VarMap.t * (var * Cst.var) list =
  match expr with
  | Int i -> Int i, vars, empty_subs, []
  | Binary (op, e1, e2) -> begin
      let e1', vars, subs1, fvs1 = expr_to_cst e1 vars  substitutions constructors in
      let e2', vars, subs2, fvs2 = expr_to_cst e2 vars (fvs1 @ substitutions) constructors in
      Binary (binary_to_cst op, e1', e2'), vars, union_subs subs1 subs2, fvs1 @ fvs2
    end
  | Fun (x, e) -> begin
      let arg_id, vars = inc vars in
      let e', vars, subs, fvs = expr_to_cst e vars ((x, arg_id) :: substitutions) constructors in
      Fun (arg_id, e'), vars, add_subs arg_id x subs, fvs
    end
  | Var x -> if has_var_name substitutions x then Var (get_var_id substitutions x), vars, empty_subs, [] else begin
      let var_id, vars = inc vars in
      Var (var_id), vars, empty_subs, [x, var_id]
    end
  | Constructor (str, exprs) -> begin
      let index = TagMap.find str constructors in
      let (vars, subs, fvs), exprs' = List.fold_left_map (fun (vars, subs, fvs) expr -> begin
        let expr', vars, subs', fvs' = expr_to_cst expr vars (fvs @ substitutions) constructors in
        (vars, union_subs subs' subs, fvs' @ fvs), expr'
      end) (vars, empty_subs, []) exprs in
      Constructor (index, exprs'), vars, subs, fvs
    end
  | Let (var, e1, e2) -> begin
      let var_id, vars = inc vars in
      let e1', vars, subs1, fvs1 = expr_to_cst e1 vars substitutions constructors in
      let e2', vars, subs2, fvs2 = expr_to_cst e2 vars ((var, var_id) :: (fvs1 @ substitutions)) constructors in
      Let (var_id, e1', e2'), vars, add_subs var_id var (union_subs subs1 subs2), fvs1 @ fvs2
    end
  | If (e1, e2, e3) -> begin
      let e1', vars, subs1, fvs1 = expr_to_cst e1 vars substitutions constructors in
      let e2', vars, subs2, fvs2 = expr_to_cst e2 vars (fvs1 @ substitutions) constructors in
      let e3', vars, subs3, fvs3 = expr_to_cst e3 vars (fvs2 @ fvs1 @ substitutions) constructors in
      If (e1', e2', e3'), vars, union_subs subs1 (union_subs subs2 subs3), fvs1 @ fvs2 @ fvs3
    end
  | App (e1, e2) -> begin
      let e1', vars, subs1, fvs1 = expr_to_cst e1 vars substitutions constructors in
      let e2', vars, subs2, fvs2 = expr_to_cst e2 vars (fvs1 @ substitutions) constructors in
      App (e1', e2'), vars, union_subs subs1 subs2, fvs1 @ fvs2
    end
  | Let_rec (bindings, e) -> begin
      let vars, bindings_ids = List.fold_left_map (fun vars (var, _) -> begin
        let var_id, vars = inc vars in
        vars, (var, var_id)
      end) vars bindings in
      let (vars, subs, fvs), bindings' = List.fold_left_map (fun (vars, subs, fvs) ((_, expr), (_, var_id)) -> begin
        let expr', vars, subs', fvs' = expr_to_cst expr vars (bindings_ids @ fvs @ substitutions) constructors in
        (vars, union_subs subs' subs, fvs' @ fvs), (var_id, expr')
      end) (vars, empty_subs, []) (List.combine bindings bindings_ids) in
      let e', vars, subs', fvs' = expr_to_cst e vars (bindings_ids @ fvs @ substitutions) constructors in
      Let_rec (bindings', e'), vars, union_subs (List.fold_left (fun subs'' (b, b_id) -> add_subs b_id b subs'') empty_subs bindings_ids) (union_subs subs subs'), fvs @ fvs'
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
            let e', vars, subs', fvs' = expr_to_cst e vars (args_ids @ fvs @ substitutions) constructors in
            (vars, union_subs (List.fold_left (fun subs'' (b, b_id) -> add_subs b_id b subs'') empty_subs args_ids) (union_subs subs subs'), fvs @ fvs'), (pattern_index, List.map (fun (_, arg_id) -> arg_id) args_ids, e')
          end
        | _ -> assert false
      end) (vars, empty_subs, []) (List.filter (fun (pattern, _) -> match pattern with | Deconstructor _ -> true | _ -> false) branchs) in
      let e', vars, subs', fvs' = expr_to_cst x vars (fvs @ substitutions) constructors in
      let default_expr, vars, subs'', fvs'' = expr_to_cst default_expr vars (fvs' @ fvs @ substitutions) constructors in
      Match (e', branchs', default_expr), vars, union_subs subs (union_subs subs' subs''), fvs @ fvs' @ fvs''
    end
  | Type (_, constructors', expr) -> expr_to_cst expr vars substitutions (List.fold_left (fun constructors'' ((constructor_name, _), index) -> TagMap.add constructor_name index constructors'') constructors (List.mapi (fun i v -> v, i) constructors'))

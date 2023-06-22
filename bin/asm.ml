type var = int
type pointer = int
type tag = int
type frame = pointer * var list
type stack = frame list

module VarMap = Map.Make (Int)

type prim =
| Add
| Sub
| Const of int
| Print

type named =
| Prim of prim * var list
| Var of var
| Tuple of var list
| Get of var * int
| Pointer of int

and expr =
| Let of var * named * expr
| Apply_direct of pointer * var list * stack
| Apply_indirect of var * var list * stack
| If of var * (int * pointer * var list) list * (pointer * var list) * stack
| Return of var

type cont =
| Let_cont of pointer * var list * expr * cont
| End

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

let pp_named (subs: string VarMap.t) (fmt: Format.formatter) (named: named): unit =
  match named with
  | Pointer p -> Format.fprintf fmt "k%d" p
  | Prim (prim, args) -> pp_prim subs fmt prim args
  | Var x -> Format.fprintf fmt "%s" (gen_name x subs)
  | Tuple (args) -> Format.fprintf fmt "Tuple [%a]" (pp_args ~split: "; " ~subs ~empty:"") args
  | Get (record, pos) -> Format.fprintf fmt "Get (%s, %d)" (gen_name record subs) pos

let rec pp_expr (subs: string VarMap.t) (fmt: Format.formatter) (cps : expr): unit =
  match cps with
  | Let (var, named, expr) -> Format.fprintf fmt "\tlet %s = %a in\n%a" (gen_name var subs) (pp_named subs) named (pp_expr subs) expr
  | Apply_direct (k, args, []) -> Format.fprintf fmt "\tk%d %a" k (pp_args ~subs ~split: " " ~empty: "") args
  | If (var, matchs, (kf, argsf), []) -> Format.fprintf fmt "\tmatch %s with%s | _ -> k%d %a" (gen_name var subs) (List.fold_left (fun acc (n, kt, argst) -> acc ^ (Format.asprintf "| Int %d -> k%d %a " n kt (pp_args ~subs ~empty: "()" ~split: " ") argst)) " " matchs) kf (pp_args ~subs ~empty: "()" ~split: " ") argsf
  | Return x -> Format.fprintf fmt "\t%s" (gen_name x subs)
  | Apply_indirect (x, args, [(k, kargs)]) -> Format.fprintf fmt "\tk%d (%s %a) %a" k (gen_name x subs) (pp_args ~split:" " ~subs ~empty: "()") args (pp_args ~split:" " ~subs ~empty: "()") kargs
  | _ -> assert false

let rec pp_cont ?(join = "let rec") (subs: string VarMap.t) (fmt: Format.formatter) (cps: cont): unit =
  match cps with
  | Let_cont (k, args, e1, cont) -> Format.fprintf fmt "%s k%d %a =\n%a\n%a" join k (pp_args ~subs ~empty: "()" ~split: " ") args (pp_expr subs) e1 (pp_cont ~join: "and" subs) cont
  | End -> Format.fprintf fmt "%!"

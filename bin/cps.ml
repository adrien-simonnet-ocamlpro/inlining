let has = Env.has

let get = Env.get2

type var = int

type pointer = int

type address = int

type return_adress =
| Direct of address
| Indirect of var

type frame = pointer * var list

type stack = frame list

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
  | Closure of pointer * var list

and expr =
  | Let of var * named * expr
  | Apply_cont of pointer * var list * stack
  | Call of var * var list * stack
  | If of var * (pointer * var list) * (pointer * var list) * stack
  | Return of var

type cont =
| Let_cont of pointer * var list * expr * cont
| End

let rec has_cont cont k =
  match cont with
  | Let_cont (k', _, _, _) when k = k' -> true
  | Let_cont (_, _, _, e2) -> has_cont e2 k
  | End -> false

let rec get_cont cont k =
match cont with
| Let_cont (k', args, e1, _) when k = k' -> args, e1
| Let_cont (_, _, _, e2) -> get_cont e2 k
| End -> failwith "cont not found"

type 'a map = (var * 'a) list





let gen_name id env =
  match Env.get_name id env with
  | Some (v, _) -> v ^ "_" ^ (string_of_int id)
  | None -> "_" ^ (string_of_int id)

let rec print_args args subs =
  match args with
  | [] -> "()"
  | [arg] -> gen_name arg subs
  | arg::args' -> (gen_name arg subs) ^ " " ^ print_args args' subs

let rec pp_args subs empty fmt args =
  match args with
  | [] -> Format.fprintf fmt empty
  | [arg] -> Format.fprintf fmt "%s" (gen_name arg subs)
  | arg::args' -> Format.fprintf fmt "%s %a" (gen_name arg subs) (pp_args subs empty) args'

let rec pp_prim subs fmt (prim : prim) args =
  match prim, args with
  | Const x, _ -> Format.fprintf fmt "Int %d" x
  | Add, x1 :: x2 :: _ -> Format.fprintf fmt "add %s %s" (gen_name x1 subs) (gen_name x2 subs)
  | Print, x1 :: _ -> Format.fprintf fmt "Printf.printf \"%%s\" %s" (gen_name x1 subs)
  | _ -> failwith "invalid args"  

and pp_named subs fmt named =
match named with
| Prim (prim, args) -> pp_prim subs fmt prim args
| Var x -> Format.fprintf fmt "%s" (gen_name x subs)
| Tuple (args) -> Format.fprintf fmt "[%a]" (pp_args subs "") args
| Get (record, pos) -> Format.fprintf fmt "get %s %d" (gen_name record subs) pos
| Closure (k, args) -> Format.fprintf fmt "Tuple [Function k%d; Tuple [%a]]" k (pp_args subs "") args

and pp_expr subs fmt (cps : expr) : unit =
  match cps with
  | Let (var, named, expr) ->
    Format.fprintf fmt "\tlet %s = %a in\n%a" (gen_name var subs) (pp_named subs) named (pp_expr subs) expr
  | Apply_cont (k, args, stack) -> let s =
    List.fold_left (fun string (k', args') -> Printf.sprintf "k%d (%s) %s" k' string (print_args args' subs)) (Printf.sprintf "k%d %s" k (print_args args subs)) stack
  in Format.fprintf fmt "%s" s
    | If (var, (kt, argst), (kf, argsf), stack) -> let s =
    List.fold_left (fun string (k', args') -> Printf.sprintf "k%d (%s) %s" k' string (print_args args' subs)) (Printf.sprintf
      "if %s = Int 0 then k%d %s else k%d %s"
      (gen_name var subs)
      kt
      (print_args argst subs)
      kf
      (print_args argsf subs)) stack
    in Format.fprintf fmt "%s" s
  | Return x -> Format.fprintf fmt "\t%s" (gen_name x subs)
  | Call (x, args, stack) -> let s = List.fold_left (fun string (k', args') -> Printf.sprintf "k%d (%s) %s" k' string (print_args args' subs)) (Printf.sprintf "(call %s %s)" (gen_name x subs) (print_args args subs)) stack
  in Format.fprintf fmt "%s" s
  
  and pp_cont subs fmt (cps : cont) : unit =
  match cps with
  | Let_cont (k, args, e1, Let_cont (k', args', e1', e2')) ->
    Format.fprintf fmt "k%d %a =\n%a\nand %a%!" k (pp_args subs "()") args (pp_expr subs) e1 (pp_cont subs) (Let_cont (k', args', e1', e2'))
  | Let_cont (k, args, e1, End) ->
    Format.fprintf fmt "k%d %a =\n%a\n%!" k (pp_args subs "()") args (pp_expr subs) e1
  | End -> Format.fprintf fmt "()%!"

let print_prog subs e = pp_cont subs Format.std_formatter e

let map_values args values = List.map2 (fun arg value -> arg, value) args values







let has = Env.has

let get = Env.get2

type var = int

type pointer = int

type address = int

type tag = int

type return_adress =
| Direct of address
| Indirect of var

type frame = pointer * var list

type stack = frame list

type prim = Asm.prim

type named =
  | Prim of prim * var list
  | Var of var
  | Tuple of var list
  | Get of var * int
  | Closure of pointer * var list
  | Constructor of int * var list

and expr =
  | Let of var * named * expr
  | Apply_cont of pointer * var list * stack
  | Call of var * var list * stack
  | If of var * (int * pointer * var list) list * (pointer * var list) * stack
  | Match_pattern of var * (int * pointer * var list) list * (pointer * var list) * stack
  | Return of var

type cont =
| Let_cont of pointer * var list * expr * cont
| Let_clos of pointer * var list * var list * expr * cont
| End

let rec has_cont cont k =
  match cont with
  | Let_cont (k', _, _, _) when k = k' -> true
  | Let_cont (_, _, _, e2) -> has_cont e2 k
  | Let_clos (_, _, _, _, e2) -> has_cont e2 k
  | End -> false

let rec get_cont cont k =
match cont with
| Let_cont (k', args, e1, _) when k = k' -> args, e1
| Let_cont (_, _, _, e2) -> get_cont e2 k
| Let_clos (_, _, _, _, e2) -> get_cont e2 k
| End -> failwith "cont not found"

let rec has_clos cont k =
  match cont with
  | Let_clos (k', _, _, _, _) when k = k' -> true
  | Let_clos (_, _, _, _, e2) -> has_clos e2 k
  | Let_cont (_, _, _, e2) -> has_clos e2 k
  | End -> false

let rec get_clos cont k =
  match cont with
  | Let_clos (k', env, args, e1, _) when k = k' -> env, args, e1
  | Let_clos (_, _, _, _, e2) -> get_clos e2 k
  | Let_cont (_, _, _, e2) -> get_clos e2 k
  | End -> failwith "clos not found"
  
type 'a map = (var * 'a) list





let gen_name id env =
  match Env.get_name id env with
  | Some (v, _) -> v ^ "_" ^ (string_of_int id)
  | None -> "_" ^ (string_of_int id)

let rec print_args ?(empty="()") args subs =
  match args with
  | [] -> empty
  | [arg] -> gen_name arg subs
  | arg::args' -> (gen_name arg subs) ^ " " ^ print_args args' subs

let rec pp_args ?(split=" ") subs empty fmt args =
  match args with
  | [] -> Format.fprintf fmt empty
  | [arg] -> Format.fprintf fmt "%s" (gen_name arg subs)
  | arg::args' -> Format.fprintf fmt "%s%s%a" (gen_name arg subs) split (pp_args ~split subs empty) args'

let rec pp_prim subs fmt (prim : prim) args =
  match prim, args with
  | Const x, _ -> Format.fprintf fmt "Int %d" x
  | Add, x1 :: x2 :: _ -> Format.fprintf fmt "add %s %s" (gen_name x1 subs) (gen_name x2 subs)
  | Sub, x1 :: x2 :: _ -> Format.fprintf fmt "sub %s %s" (gen_name x1 subs) (gen_name x2 subs)
  | Print, x1 :: _ -> Format.fprintf fmt "print %s" (gen_name x1 subs)
  | _ -> failwith "invalid args"  

and pp_named subs fmt named =
match named with
| Prim (prim, args) -> pp_prim subs fmt prim args
| Var x -> Format.fprintf fmt "%s" (gen_name x subs)
| Tuple (args) -> Format.fprintf fmt "Tuple [%a]" (pp_args ~split:"; " subs "") args
| Get (record, pos) -> Format.fprintf fmt "get %s %d" (gen_name record subs) pos
(*TODO*)
| Closure (k, env) -> Format.fprintf fmt "Closure (Function k%d, [%a])" k (pp_args ~split:"; " subs "") env
| Constructor (tag, env) -> Format.fprintf fmt "Constructor (%d, [%a])" tag (pp_args ~split:"; " subs "") env

and pp_expr subs fmt (cps : expr) : unit =
  match cps with
  | Let (var, named, expr) ->
    Format.fprintf fmt "\tlet %s = %a in\n%a" (gen_name var subs) (pp_named subs) named (pp_expr subs) expr
  | Apply_cont (k, args, stack) -> let s =
    List.fold_left (fun string (k', args') -> Printf.sprintf "k%d (%s) %s" k' string (print_args ~empty:"" args' subs)) (Printf.sprintf "k%d %s" k (print_args args subs)) stack
  in Format.fprintf fmt "%s" s
    | If (var, matchs, (kf, argsf), stack) -> let s =
    List.fold_left (fun string (k', args') -> Printf.sprintf "k%d (%s) %s" k' string (print_args ~empty:"" args' subs)) (Printf.sprintf
      "match %s with%s | _ -> k%d %s"
      (gen_name var subs)
      (List.fold_left (fun acc (n, kt, argst) -> acc ^ (Printf.sprintf "| Int %d -> k%d %s " n kt (print_args argst subs))) " " matchs)
      kf
      (print_args argsf subs)) stack
    in Format.fprintf fmt "%s" s
  | Match_pattern (var, matchs, (kf, argsf), stack) -> let s =
    List.fold_left (fun string (k', args') -> Printf.sprintf "k%d (%s) %s" k' string (print_args ~empty:"" args' subs)) (Printf.sprintf
      "match %s with%s | _ -> k%d %s"
      (gen_name var subs)
      (List.fold_left (fun acc (n, kt, argst) -> acc ^ (Printf.sprintf "| Int %d -> f%d %s " n kt (print_args argst subs))) " " matchs)
      kf
      (print_args argsf subs)) stack
    in Format.fprintf fmt "%s" s
  | Return x -> Format.fprintf fmt "\t%s" (gen_name x subs)
  | Call (x, args, stack) -> let s = List.fold_left (fun string (k', args') -> Printf.sprintf "f%d (%s) %s" k' string (print_args ~empty:"" args' subs)) (Printf.sprintf "(call %s %s)" (gen_name x subs) (print_args args subs)) stack
  in Format.fprintf fmt "%s" s
  
  and pp_cont subs fmt (cps : cont) : unit =
  match cps with
  | Let_cont (k, args, e1, End) ->
    Format.fprintf fmt "k%d %a =\n%a\n%!" k (pp_args subs "()") args (pp_expr subs) e1
  | Let_cont (k, args, e1, cont) ->
    Format.fprintf fmt "k%d %a =\n%a\nand %a%!" k (pp_args subs "()") args (pp_expr subs) e1 (pp_cont subs) cont
  | Let_clos (k, env, args, e1, End) ->
    Format.fprintf fmt "f%d [%a] %a =\n%a\n%!" k (pp_args subs " ") env (pp_args subs "()") args (pp_expr subs) e1
  | Let_clos (k, env, args, e1, cont) ->
    Format.fprintf fmt "f%d [%a] %a =\n%a\nand %a%!" k (pp_args subs " ") env (pp_args subs "()") args (pp_expr subs) e1 (pp_cont subs) cont
  | End -> Format.fprintf fmt "()%!"

let print_prog subs e = pp_cont subs Format.std_formatter e

let map_values args values = List.map2 (fun arg value -> arg, value) args values

let vars = ref 10000000

let inc vars =
  vars := !vars + 1;
  !vars
;;


let named_to_asm (named : named) : Asm.named =
  match named with
  | Prim (prim, args) -> Prim (prim, args)
  | Var x -> Var x
  | Tuple args -> Tuple args
  | Get (record, pos) -> Get (record, pos)
  | Closure (_k, _x) -> assert false
  (*TODO*)
  | Constructor (_tag, _environment_id) -> assert false

let rec expr_to_asm (cps : expr) : Asm.expr =
    match cps with
    | Let (var, Closure (k, environment_id), expr) -> let v1 = inc vars in let v2 = inc vars in Let (v1, Pointer k, Let (v2, Tuple environment_id, Let (var, Tuple [v1; v2], expr_to_asm expr)))
    | Let (var, Constructor (tag, args), expr) -> let v1 = inc vars in let v2 = inc vars in Let (v1, Prim (Const tag, []), Let (v2, Tuple args, Let (var, Tuple [v1; v2], expr_to_asm expr)))
    | Let (var, named, expr) -> Let (var, named_to_asm named, expr_to_asm expr)
    | Apply_cont (k, args, stack) -> Apply_cont (k, args, stack)
    | If (var, matchs, (kf, argsf), stack) -> If (var, matchs, (kf, argsf), stack)
    | Match_pattern (pattern_id, matchs, (kf, argsf), stack) -> begin
        let pattern_tag_id = inc vars in
        let pattern_payload_id = inc vars in
        (Asm.Let (pattern_tag_id, Get (pattern_id, 0), (Asm.Let (pattern_payload_id, Get (pattern_id, 1), If (pattern_tag_id, List.map (fun (n, k, args) -> (n, k, pattern_payload_id :: args)) matchs, (kf, argsf), stack)))))
      end
    | Return v -> Return v
    | Call (x, args, stack) -> let v1 = inc vars in let v2 = inc vars in Let (v1, Get (x, 0), Let (v2, Get (x, 1), Call (v1, v2::args, stack)))

let rec cont_to_asm (cps : cont) : Asm.cont =
  match cps with
      | Let_cont (k', args', e1, e2) -> Let_cont (k', args', expr_to_asm e1, cont_to_asm e2)
      | Let_clos (k', body_free_variables, args', e1, e2) -> begin
          let environment_id = inc vars in
          let _, body = List.fold_left (fun (pos, cps') body_free_variable -> pos + 1, Asm.Let (body_free_variable, Asm.Get (environment_id, pos), cps')) (0, (expr_to_asm e1)) body_free_variables in
          Let_cont (k', environment_id::args', body, cont_to_asm e2)
        end
      | End -> End
  ;;
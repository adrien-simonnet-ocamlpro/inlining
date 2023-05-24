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

type value =
  | Int of int
  | Tuple of value list

type env = value map

type value_domain =
  | Int_domain of Int_domain.t
  | Tuple_domain of value_domain list
  | Pointer_domain of Pointer_domain.t

type env_domain = value_domain map

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

let rec interp_prim var (prim : prim) args (env : (var * value) list) =
  match prim, args with
  | Const x, _ -> [ var, Int x ]
  | Add, x1 :: x2 :: _ ->
    (match (get env x1 : value) with
     | Int n1 ->
       (match get env x2 with
        | Int n2 -> [ var, Int (n1 + n2) ]
        | _ -> failwith "invalid type")
     | _ -> failwith "invalid type")
  | Print, x1 :: _ ->
    (match (get env x1 : value) with
     | Int n ->
       Printf.printf "%d\n" n;
       []
     | _ -> failwith "invalid type")
  | _ -> failwith "invalid args"

and interp_named var (named : named) (env : (var * value) list) =
  match named with
  | Prim (prim, args) -> interp_prim var prim args env
  | Var x -> [ var, get env x ]
  | Tuple (args) -> [var, Tuple (List.map (fun arg -> get env arg) args)]
  | Get (record, pos) -> begin
    match get env record with
    | Tuple (values) -> [var, List.nth values pos]
    | _ -> failwith "invalid type"
    end
  | Closure (k, args) -> [var, Tuple [Int k; Tuple (List.map (fun arg -> get env arg) args)]]

and interp (stack: (pointer * value list) list) (cps : expr) (env : env) (conts : (int * var list * expr * env) list): value =

    match cps with
    | Let (var, named, expr) -> interp stack expr (interp_named var named env @ env) conts
    | Apply_cont (k, args, stack') -> let args', cont, _ = Env.get_cont conts k in
      interp ((List.map (fun (k, env') -> (k, (List.map (fun arg -> get env arg) env'))) stack')@stack) cont (List.map2 (fun arg' arg -> arg', get env arg) args' args ) conts
    | If (var, (kt, argst), (kf, argsf), stack') ->
      (match get env var with
       | Int n ->
         if n = 0
         then interp stack (Apply_cont (kt, argst, stack')) env conts
         else interp stack (Apply_cont (kf, argsf, stack')) env conts
       | _ -> failwith "invalid type")
    | Return v -> begin
      match stack with
      | [] -> get env v
      | (k, env')::stack' -> let args2', cont'', _ = Env.get_cont conts k in
      interp stack' cont'' ((List.hd args2', get env v)::(List.map2 (fun arg' arg -> arg', arg) (List.tl args2') env') ) conts
    end
    | Call (x, args, stack') -> begin
      match get env x with
      | Int k' -> let args', cont, _ = Env.get_cont conts k' in
        interp ((List.map (fun (k, env') -> (k, (List.map (fun arg -> get env arg) env'))) stack')@stack) cont ((List.map2 (fun arg' arg -> arg', get env arg) args' args)) conts
      | _ -> failwith ("invalid type")
       end


and interp_cont k (cps : cont) (conts : (int * var list * expr * env) list) env: value =
match cps with
    | Let_cont (k', args', e1, e2) -> interp_cont k e2 ((k', args', e1, []) :: conts) env
    | End -> let _, cont, _ = Env.get_cont conts k in interp [] cont env conts
;;


let _ = function
| Int i -> Prim (Const i, [])
| _ -> failwith "not implemented"

let vis var cont visites = (var, cont)::visites
let a_visite var cont visites = List.exists (fun (var', cont') -> var = var' && cont = cont') visites

module Analysis = Map.Make (Int)

let map_values args values = List.map2 (fun arg value -> arg, value) args values

let rec propagation_prim (prim : prim) args (env : env_domain) : named * value_domain =
  match prim, args with
  | Const x, args' -> Prim (Const x, args'), Int_domain (Int_domain.singleton x)
  | Add, x1 :: x2 :: args' -> begin match get env x1, get env x2 with
    | Int_domain d1, Int_domain d2 when Int_domain.is_singleton d1 && Int_domain.is_singleton d2 -> let x = ((Int_domain.get_singleton d1) + Int_domain.get_singleton d2) in Prim (Const x, []), Int_domain (Int_domain.singleton x)
    | Int_domain _, Int_domain _ -> Prim (Add, x1 :: x2 :: args'), Int_domain (Int_domain.top)
    | _ -> failwith "invalid type"
  end
  | Print, _ :: _ -> Prim (Print, args), Int_domain (Int_domain.top)
  | _ -> failwith "invalid args"

and propagation_named (named : named) (env : env_domain) : named * value_domain =
    match named with
    | Var var' -> Var var', get env var'
    | Prim (prim, args) -> propagation_prim prim args env
    | Tuple vars -> Tuple vars, Tuple_domain (List.map (fun var' -> get env var') vars)
    | Get (var', pos) -> begin match get env var' with
      | Tuple_domain values -> Get (var', pos), List.nth values pos
      | _ -> failwith "invalid type"
      end
    | Closure (k, vars) -> Closure (k, vars), Tuple_domain [Pointer_domain (Pointer_domain.singleton k); Tuple_domain (List.map (fun var' -> get env var') vars)]


and propagation (cps : expr) (env: env_domain) (conts : cont) : expr =
  match cps with
  | Let (var, named, expr) -> let named', value = propagation_named named env in Let (var, named', propagation expr ((var, value)::env) conts)
  | Apply_cont (k', args, stack) -> Apply_cont (k', args, stack)
  | If (var, (kt, argst), (kf, argsf), stack) ->
    if has env var then begin
      match get env var with
      | Int_domain i when Int_domain.is_singleton i && Int_domain.get_singleton i = 0 -> Apply_cont (kt, argst, stack)
      | Int_domain _ -> Apply_cont (kf, argsf, stack)
      | _ -> failwith "invalid type"
    end else If (var, (kt, argst), (kf, argsf), stack)
  | Return x -> Return x
  | Call (x, args, stack) when has env x -> begin
    match get env x with
    | Pointer_domain k -> if Pointer_domain.is_singleton k then Apply_cont (Pointer_domain.get_singleton k, args, stack) else Call (x, args, stack)
    | _ -> failwith "invalid type" end
  | Call (x, args, stack) -> Call (x, args, stack)

and propagation_cont (cps : cont) (conts : cont) map : cont =
  match cps with
  | Let_cont (k', args', e1, e2) ->
    let e1' = propagation e1 (map_values args' (Analysis.find k' map)) conts in
    let e2' = propagation_cont e2 conts map in
    Let_cont (k', args', e1', e2')
  | End -> End
;;


let map_args (args: var list) (env: env_domain) = List.map (fun arg -> get env arg) args

let map_stack (stack: stack) (env: env_domain) = List.map (fun (k'', args') -> k'', map_args args' env) stack

let analysis_prim (prim : prim) args (env : env_domain) : value_domain =
  match prim, args with
  | Const x, _ -> Int_domain (Int_domain.singleton x)
  | Add, x1 :: x2 :: _ -> begin match get env x1, get env x2 with
      | Int_domain d1, Int_domain d2 when Int_domain.is_singleton d1 && Int_domain.is_singleton d2 -> Int_domain (Int_domain.singleton ((Int_domain.get_singleton d1) + (Int_domain.get_singleton d2)))
      | Int_domain _, Int_domain _ -> Int_domain (Int_domain.top)
      | _ -> failwith "invalid type"
    end
  | Print, _ :: _ -> Int_domain (Int_domain.top)
  | _ -> failwith "invalid args"

let analysis_named (named : named) (env : env_domain) : value_domain =
  match named with
  | Var var' -> get env var'
  | Prim (prim, args) -> analysis_prim prim args env
  | Tuple vars -> Tuple_domain (map_args vars env)
  | Get (var', pos) -> begin
      match get env var' with
      | Tuple_domain values -> List.nth values pos
      | _ -> failwith "invalid type"
    end
  | Closure (k, vars) -> Tuple_domain [Pointer_domain (Pointer_domain.singleton k); Tuple_domain (map_args vars env)]

let rec analysis_cont (cps: expr) (stack: ((pointer * value_domain list) list)) (env: (address * value_domain) list) : (pointer * value_domain list * ((pointer * value_domain list) list)) list =
  match cps with
  | Let (var, named, expr) -> let value = analysis_named named env in analysis_cont expr stack ((var, value)::env)
  | Apply_cont (k', args, stack) -> [k', map_args args env, map_stack stack env]
  | If (var, (kt, argst), (kf, argsf), stack) -> 
    if has env var then begin
      match get env var with
      | Int_domain i when Int_domain.is_singleton i && Int_domain.get_singleton i = 0 -> [kt, map_args argst env, map_stack stack env]
      | Int_domain i when Int_domain.is_singleton i && Int_domain.get_singleton i != 0 -> [kf, map_args argsf env, map_stack stack env]
      | Int_domain _ -> [kt, map_args argst env, map_stack stack env; kf, map_args argsf env, map_stack stack env]
      | _ -> failwith "invalid type"
    end else [kt, map_args argst env, map_stack stack env; kf, map_args argsf env, map_stack stack env]
  | Return x -> begin match stack with
      | [] -> []
      | (k, args)::stack' -> [k, (get env x)::args, stack']
    end
  | Call (x, args, stack) -> begin
    match get env x with
    | Pointer_domain d -> List.map (fun k -> k, map_args args env, map_stack stack env) (Pointer_domain.to_list d)
    | _ -> failwith "invalid type" end


let rec join_values v1 v2 = match v1, v2 with
| Pointer_domain p1, Pointer_domain p2 -> Pointer_domain (Pointer_domain.join p1 p2)
| Int_domain d1, Int_domain d2 -> Int_domain (Int_domain.join d1 d2)
| Tuple_domain values1, Tuple_domain values2 -> Tuple_domain (join_env values1 values2)
| _ -> assert false

and join_env (old_env: value_domain list) (new_env: value_domain list): value_domain list = List.map2 join_values old_env new_env







let rec analysis (conts: (pointer * value_domain list * ((pointer * value_domain list) list)) list) (prog: cont) (map: (value_domain list) Analysis.t) =
  match conts with
  | [] -> map
  | (k, env, stack)::conts' ->
    if Analysis.mem k map then 
      let old_env = Analysis.find k map in
      let new_env = join_env old_env env in
      if new_env = old_env
        then analysis conts' prog map
        else let args, cont = get_cont prog k in
          let next_conts = analysis_cont cont stack (map_values args env) in
          analysis (conts'@next_conts) prog (Analysis.add k new_env map)
    else
      let args, cont = get_cont prog k in
      let next_conts = analysis_cont cont stack (map_values args env) in
      analysis (conts'@next_conts) prog (Analysis.add k env map)

let start_analysis prog args = analysis [0, args, []] prog (Analysis.empty)

let rec pp_value_domain fmt = function
| Int_domain d ->  Int_domain.pp fmt d
| Pointer_domain d -> Pointer_domain.pp fmt d
| Tuple_domain values -> Format.fprintf fmt "[%a]" (pp_env "") values

and pp_env empty fmt args =
  match args with
  | [] -> Format.fprintf fmt "%s" empty
  | [arg] -> Format.fprintf fmt "%a" pp_value_domain arg
  | arg::args' -> Format.fprintf fmt "%a %a" pp_value_domain arg (pp_env empty) args'

let pp_analysis fmt (map: (value_domain list) Analysis.t) = Format.fprintf fmt "Analysis:\n"; Analysis.iter (fun k env -> Format.fprintf fmt "%d: %a\n" k (pp_env "") env) map

let rec elim_unused_vars_named (vars : int array) conts (named : named)
  : named
  =
  match named with
  | Prim (prim, args) ->
    List.iter
      (fun arg ->
        Array.set vars arg (Array.get vars arg + 1))
      args;
    Prim (prim, args)
    | Var x ->
      Array.set vars x (Array.get vars x + 1);
      Var x
  | Tuple args -> List.iter
  (fun arg ->
    Array.set vars arg (Array.get vars arg + 1))
  args; Tuple args
  | Closure (k, args) -> List.iter
  (fun arg ->
    Array.set vars arg (Array.get vars arg + 1))
  args; Array.set conts k (Array.get conts k + 1); Closure (k, args)
  | Get (arg, pos) -> Array.set vars arg (Array.get vars arg + 1); Get (arg, pos)


and elim_unused_vars (vars : int array) (conts : int array) (cps : expr) : expr =
  match cps with
  | Let (var, e1, e2) ->
    let e2' = elim_unused_vars vars conts e2 in
    if Array.get vars var > 0
    then (
      let e1' = elim_unused_vars_named vars conts e1 in
      Let (var, e1', e2'))
    else e2'
  | Apply_cont (k, args, stack) ->
    Array.set conts k (Array.get conts k + 1);
    List.iter
      (fun arg ->
        Array.set vars arg (Array.get vars arg + 1))
      args;
    Apply_cont (k, args, stack)
  | If (var, (kt, argst), (kf, argsf), stack) ->
    Array.set vars var (Array.get vars var + 1);
    Array.set conts kt (Array.get conts kt + 1);
    Array.set conts kf (Array.get conts kf + 1);
    If (var, (kt, argst), (kf, argsf), stack)
  | Return x ->
    Array.set vars x (Array.get vars x + 1);
    Return x
  | Call (x, args, stack) ->
    Array.set vars x (Array.get vars x + 1);
    List.iter (fun (k, args2) -> Array.set conts k (Array.get conts k + 1);
    List.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args2) stack;
    List.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args;
      Call (x, args, stack)
and elim_unused_vars_cont (conts : int array) (cps : cont) : cont * int array =
    match cps with
    | Let_cont (k', args', e1, e2) ->
      let e2', _ = elim_unused_vars_cont conts e2 in
      let e1' = elim_unused_vars (Array.make 1000 0) conts e1 in Let_cont (k', args', e1', e2'), conts
    | End -> End, conts
and elim_unused_conts (conts : int array) (cps : cont) : cont =
    match cps with
    | Let_cont (k', args', e1, e2) -> if Array.get conts k' > 0 then Let_cont (k', args', e1, elim_unused_conts conts e2) else elim_unused_conts conts e2
    | End -> End
;;

let get env arg = if has env arg then get env arg else arg

let rec inline_named (named : named) (env : (var * var) list) =
  match named with
  | Prim (prim, args) -> Prim (prim, List.map (fun arg -> get env arg) args)
  | Var x -> Var (get env x)
  | Tuple (args) -> Tuple (List.map (fun arg -> get env arg) args)
  | Get (record, pos) -> Get (get env record, pos)
  | Closure (k, args) -> Closure (k, List.map (fun arg -> get env arg) args)

and inline (stack: (pointer * var list) list) (cps : expr) (env : (var * var) list) (conts : cont): expr =

    match cps with
    | Let (var, named, expr) -> Let (var, inline_named named env, inline stack expr env conts)
    | Apply_cont (k, args, stack') -> Apply_cont (k, args, stack' @ stack)
    | If (var, (kt, argst), (kf, argsf), stack') -> If (var, (kt, argst), (kf, argsf), stack' @ stack)
    | Return v -> begin
      match stack with
      | [] -> assert false (* ?? *)
      | (k, env')::stack' -> Apply_cont (k, v::env', stack')
    end
    | Call (x, args, stack') -> Call (x, args, stack' @ stack)


let rec inline_parent (cps : expr) (conts: cont): expr =

    match cps with
    | Let (var, named, expr) -> Let (var, named, inline_parent expr conts)
    | Apply_cont (k, args, stack') -> let args', cont = get_cont conts k in
        inline stack' cont (List.map2 (fun arg' arg -> arg', arg) args' args) conts
    | If (var, (kt, argst), (kf, argsf), stack') -> If (var, (kt, argst), (kf, argsf), stack')
    | Return _ -> assert false (* ?? *)
    | Call (x, args, stack') -> Call (x, args, stack')


let rec inline_cont ks (cps : cont) (conts : cont): cont =
  match cps with
      | Let_cont (k', args', e1, e2) when List.mem k' ks -> Let_cont (k', args', inline_parent e1 conts, inline_cont ks e2 conts)
      | Let_cont (k', args', e1, e2) -> Let_cont (k', args', e1, inline_cont ks e2 conts)
      | End -> End
  ;;
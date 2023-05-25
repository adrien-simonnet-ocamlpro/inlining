module Analysis = Map.Make (Int)

type value_domain =
  | Int_domain of Int_domain.t
  | Tuple_domain of value_domain list
  | Pointer_domain of Pointer_domain.t

type t = (value_domain list) Analysis.t

type 'a map = 'a Cps.map

type env_domain = value_domain map

type var = Cps.var

type stack = Cps.stack

type prim = Cps.prim
type named = Cps.named
type pointer = Cps.pointer
type expr = Cps.expr
type address = Cps.address
type cont = Cps.cont

let get = Env.get2
let has = Env.has

let get_cont = Cps.get_cont

let map_values = Cps.map_values

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
  | Apply_cont (k', args, stack') -> [k', map_args args env, (map_stack stack' env)@stack]
  | If (var, (kt, argst), (kf, argsf), stack') -> 
    if has env var then begin
      match get env var with
      | Int_domain i when Int_domain.is_singleton i && Int_domain.get_singleton i = 0 -> [kt, map_args argst env, (map_stack stack' env)@stack]
      | Int_domain i when Int_domain.is_singleton i && Int_domain.get_singleton i != 0 -> [kf, map_args argsf env, (map_stack stack' env)@stack]
      | Int_domain _ -> [kt, map_args argst env, (map_stack stack' env)@stack; kf, map_args argsf env, (map_stack stack' env)@stack]
      | _ -> failwith "invalid type"
    end else [kt, map_args argst env, (map_stack stack' env)@stack; kf, map_args argsf env, (map_stack stack' env)@stack]
  | Return x -> begin match stack with
      | [] -> []
      | (k, args)::stack' -> [k, (get env x)::args, stack']
    end
  | Call (x, args, stack') -> begin
    match get env x with
    | Pointer_domain d -> List.map (fun k -> k, map_args args env, (map_stack stack' env)@stack) (Pointer_domain.to_list d)
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

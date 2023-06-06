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

let get2 env var =
  match List.find_opt (fun (var', _) -> var = var') env with
  | Some (_, v) -> v
  | None -> assert false
;;

let get_cont = Cps.get_cont

let map_values = Cps.map_values

let map_args (args: var list) (env: env_domain) = List.map (fun arg -> get env arg) args

let map_stack (stack: stack) (env: env_domain) = List.map (fun (k'', args') -> k'', map_args args' env) stack

let rec cherche_motif motif liste =
  match motif, liste with
  | [], _ -> true
  | m'::motif', l'::liste' when m' = l' -> cherche_motif motif' liste'
  | _, _ -> false

let rec cherche_periode periode liste =
  match liste with
  | [] -> false
  | l::liste' -> cherche_motif (periode@[l]) liste' || cherche_periode (periode@[l]) liste'

let map = List.map (fun (k, _) -> k)

let join_stack old_stack new_stack = if cherche_periode [] (map new_stack) then old_stack else new_stack

let analysis_prim (prim : prim) args (env : env_domain) : value_domain =
  match prim, args with
  | Const x, _ -> Int_domain (Int_domain.singleton x)
  | Add, x1 :: x2 :: _ -> begin match get env x1, get env x2 with
      | Int_domain d1, Int_domain d2 when Int_domain.is_singleton d1 && Int_domain.is_singleton d2 -> Int_domain (Int_domain.singleton ((Int_domain.get_singleton d1) + (Int_domain.get_singleton d2)))
      | Int_domain _, Int_domain _ -> Int_domain (Int_domain.top)
      | _ -> failwith "invalid type"
    end
  | Sub, x1 :: x2 :: _ -> begin match get env x1, get env x2 with
    | Int_domain d1, Int_domain d2 when Int_domain.is_singleton d1 && Int_domain.is_singleton d2 -> Int_domain (Int_domain.singleton ((Int_domain.get_singleton d1) - (Int_domain.get_singleton d2)))
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
      | _ -> assert false
    end
  | Closure (k, var') -> Tuple_domain [Pointer_domain (Pointer_domain.singleton k); get env var']
  (* TODO *)
  | Environment vars -> Tuple_domain (map_args vars env)
  | Tag x -> Int_domain (Int_domain.singleton x)
  | Constructor (tag, environment_id) -> Tuple_domain [Int_domain (Int_domain.singleton tag); get env environment_id]

let rec analysis_cont (cps: expr) (stack: ((pointer * value_domain list) list)) (env: (address * value_domain) list) : (pointer * value_domain list * ((pointer * value_domain list) list)) list =
  match cps with
  | Let (var, named, expr) -> let value = analysis_named named env in analysis_cont expr stack ((var, value)::env)
  | Apply_cont (k', args, stack') -> [k', map_args args env, (join_stack stack ((map_stack stack' env)@stack))]
  | If (var, matchs, (kf, argsf), stack') -> 
    if has env var then begin
      match get env var with
      | Int_domain i when Int_domain.is_singleton i -> begin
        match List.find_opt (fun (n', _, _) -> Int_domain.get_singleton i = n') matchs with
        | Some (_, kt, argst) -> [kt, map_args argst env, (join_stack stack ((map_stack stack' env)@stack))]
        | None -> [kf, map_args argsf env, (join_stack stack ((map_stack stack' env)@stack))]
        end
      | Int_domain _ -> (kf, map_args argsf env, (join_stack stack ((map_stack stack' env)@stack)))::(List.map (fun (_, kt, argst) -> kt, map_args argst env, (join_stack stack ((map_stack stack' env)@stack))) matchs)
      | _ -> failwith "invalid type"
    end else (kf, map_args argsf env, (join_stack stack ((map_stack stack' env)@stack)))::(List.map (fun (_, kt, argst) -> kt, map_args argst env, (join_stack stack ((map_stack stack' env)@stack))) matchs)
  | Return x -> begin match stack with
      | [] -> []
      | (k, args)::stack' -> [k, (get env x)::args, stack']
    end
  | Call (x, args, stack') -> begin
    match get env x with
    | Pointer_domain d -> List.map (fun k -> k, map_args args env, (join_stack stack ((map_stack stack' env)@stack))) (Pointer_domain.to_list d)
    | _ -> failwith "invalid type" end


let rec join_values v1 v2 = match v1, v2 with
| Pointer_domain p1, Pointer_domain p2 -> Pointer_domain (Pointer_domain.join p1 p2)
| Int_domain d1, Int_domain d2 -> Int_domain (Int_domain.join d1 d2)
| Tuple_domain values1, Tuple_domain values2 -> Tuple_domain (join_env values1 values2)
| _ -> assert false

and join_env (old_env: value_domain list) (new_env: value_domain list): value_domain list = List.map2 join_values old_env new_env


let has3 env var = List.exists (fun (var', _) -> map var = map var') env
let get3 env var =
  let map = List.map (fun (k, _) -> k) in
  match List.find_opt (fun (var', _) -> map var = map var') env with
  | Some (_, v) -> v
  | None -> assert false
;;

let rec analysis (conts: (pointer * value_domain list * ((pointer * value_domain list) list)) list) (prog: cont) (map: ((((address * value_domain list) list) * value_domain list) list) Analysis.t) : (value_domain list) Analysis.t =
  match conts with
  | [] -> Analysis.map (fun contexts -> List.fold_left (fun acc (_, new_env) -> if acc = [] then new_env else join_env acc new_env) [] contexts) map
  | (k, env, stack)::conts' ->
    if Analysis.mem k map then 
      let old_context = Analysis.find k map in
      if has3 old_context stack then
        let old_env = get3 old_context stack in
        let new_env = join_env old_env env in
        if new_env = old_env
          then analysis conts' prog map
          else let args, cont = get_cont prog k in
            let next_conts = analysis_cont cont stack (map_values args env) in
            analysis (conts'@next_conts) prog (Analysis.add k ((stack,new_env)::old_context) map)
      else
        let args, cont = get_cont prog k in
        let next_conts = analysis_cont cont stack (map_values args env) in
        analysis (conts'@next_conts) prog (Analysis.add k ((stack,env)::old_context) map)
    else
      let args, cont = get_cont prog k in
      let next_conts = analysis_cont cont stack (map_values args env) in
      analysis (conts'@next_conts) prog (Analysis.add k [stack,env] map)

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

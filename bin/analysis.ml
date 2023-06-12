module Analysis = Map.Make (Int)

module Allocations = Map.Make (Int)

module Closures = Map.Make (Int)

module Values = Set.Make (Int)

type value_domain =
  | Int_domain of Int_domain.t
  | Tuple_domain of Values.t list
  | Pointer_domain of Pointer_domain.t
  | Closure_domain of (Values.t list) Closures.t

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

let join_env (old_env: 'a list) (new_env: 'a list): 'a list = List.map2 Values.union old_env new_env

let join_values v1 v2 = match v1, v2 with
| Pointer_domain p1, Pointer_domain p2 -> Pointer_domain (Pointer_domain.join p1 p2)
| Int_domain d1, Int_domain d2 -> Int_domain (Int_domain.join d1 d2)
| Tuple_domain values1, Tuple_domain values2 -> Tuple_domain (List.map2 Values.union values1 values2)
| Closure_domain clos1, Closure_domain clos2 -> Closure_domain (Closures.union (fun _ env1 env2 -> Some (join_env env1 env2)) clos1 clos2)
| _ -> assert false



let rec join_value_list (values: value_domain list): value_domain =
  match values with
  | [] -> assert false
  | [value] -> value
  | value :: values' -> join_values value (join_value_list values')

let join_stack old_stack new_stack = if cherche_periode [] (map new_stack) then old_stack else new_stack

let join_allocs allocs allocations =
  let values = Values.elements allocs in
  let values_domain = List.map (fun alloc -> Allocations.find alloc allocations) values in
  join_value_list values_domain

let get (env: (address * Values.t) list) value allocations =
  let allocs = get env value in join_allocs allocs allocations

let analysis_prim (prim : prim) args (env: (address * Values.t) list) (allocations: value_domain Allocations.t) : value_domain =
  match prim, args with
  | Const x, _ -> Int_domain (Int_domain.singleton x)
  | Add, x1 :: x2 :: _ -> begin match get env x1 allocations, get env x2 allocations with
      | Int_domain d1, Int_domain d2 when Int_domain.is_singleton d1 && Int_domain.is_singleton d2 -> Int_domain (Int_domain.singleton ((Int_domain.get_singleton d1) + (Int_domain.get_singleton d2)))
      | Int_domain _, Int_domain _ -> Int_domain (Int_domain.top)
      | _ -> assert false
    end
  | Sub, x1 :: x2 :: _ -> begin match get env x1 allocations, get env x2 allocations with
    | Int_domain d1, Int_domain d2 when Int_domain.is_singleton d1 && Int_domain.is_singleton d2 -> Int_domain (Int_domain.singleton ((Int_domain.get_singleton d1) - (Int_domain.get_singleton d2)))
    | Int_domain _, Int_domain _ -> Int_domain (Int_domain.top)
    | _ -> assert false
  end
  | Print, _ :: _ -> Int_domain (Int_domain.top)
  | _ -> failwith "invalid args"

let map_args2 (args: var list) (env: (address * Values.t) list) = List.map (fun arg -> Env.get2 env arg) args

let map_stack2 (stack: stack) (env) = List.map (fun (k'', args') -> k'', map_args2 args' env) stack

let analysis_named (named : named) (env: (address * Values.t) list) (allocations: value_domain Allocations.t) : value_domain =
  match named with
  | Var var' -> get env var' allocations
  | Prim (prim, args) -> analysis_prim prim args env allocations
  | Tuple vars -> Tuple_domain (map_args2 vars env)
  | Get (var', pos) -> begin
      match get env var' allocations with
      | Tuple_domain values -> join_allocs (List.nth values pos) allocations
      | Closure_domain clos when pos = 0 -> Pointer_domain (Pointer_domain.of_list (List.map (fun (k, _) -> k) (Closures.bindings clos)))
      | Closure_domain clos when pos = 1 -> Tuple_domain (Closures.fold (fun _ values value -> List.map2 Values.union values value) clos (snd (Closures.choose clos)))
      | _ -> assert false
    end
  | Closure (_k, _var') -> begin
      match get env _var' allocations with
      | Tuple_domain values -> Closure_domain (Closures.singleton _k values)
      | _ -> assert false
    end  (*Tuple_domain [Pointer_domain (Pointer_domain.singleton k); Env.get2 env var']*)
  (* TODO *)
  | Environment vars -> Tuple_domain (map_args2 vars env)
  | Tag x -> Int_domain (Int_domain.singleton x)
  | Constructor (_tag, _environment_id) -> begin
    match get env _environment_id allocations with
    | Tuple_domain values -> Closure_domain (Closures.singleton _tag values)
    | _ -> assert false
  end(*Tuple_domain [Int_domain (Int_domain.singleton tag); get env environment_id allocations]*)

let add_alloc var new_value map = Allocations.update var (fun value -> begin
    match value with
    | Some old_value -> Some (join_values old_value new_value)
    | None -> Some new_value
  end) map

let rec analysis_cont (cps: expr) (stack: ((pointer * Values.t list) list)) (env: (address * Values.t) list) (allocations: value_domain Allocations.t): (pointer * Values.t list * ((pointer * Values.t list) list) * value_domain Allocations.t) list =
  match cps with
  | Let (var, named, expr) -> begin
      let value = analysis_named named env allocations in
      analysis_cont expr stack ((var, Values.singleton var)::env) (add_alloc var value allocations)
    end
  | Apply_cont (k', args, stack') -> [k', map_args2 args env, (join_stack stack ((map_stack2 stack' env)@stack)), allocations]
  | If (var, matchs, (kf, argsf), stack') -> 
    if has env var then begin
      match get env var allocations with
      | Int_domain i when Int_domain.is_singleton i -> begin
        match List.find_opt (fun (n', _, _) -> Int_domain.get_singleton i = n') matchs with
        | Some (_, kt, argst) -> [kt, map_args2 argst env, (join_stack stack ((map_stack2 stack' env)@stack)), allocations]
        | None -> [kf, map_args2 argsf env, (join_stack stack ((map_stack2 stack' env)@stack)), allocations]
        end
      | Int_domain _ -> (kf, map_args2 argsf env, (join_stack stack ((map_stack2 stack' env)@stack)), allocations)::(List.map (fun (_, kt, argst) -> kt, map_args2 argst env, (join_stack stack ((map_stack2 stack' env)@stack)), allocations) matchs)
      | Pointer_domain i when Pointer_domain.is_singleton i -> begin
        match List.find_opt (fun (n', _, _) -> Pointer_domain.get_singleton i = n') matchs with
        | Some (_, kt, argst) -> [kt, map_args2 argst env, (join_stack stack ((map_stack2 stack' env)@stack)), allocations]
        | None -> [kf, map_args2 argsf env, (join_stack stack ((map_stack2 stack' env)@stack)), allocations]
        end
      | Pointer_domain _ -> (kf, map_args2 argsf env, (join_stack stack ((map_stack2 stack' env)@stack)), allocations)::(List.map (fun (_, kt, argst) -> kt, map_args2 argst env, (join_stack stack ((map_stack2 stack' env)@stack)), allocations) matchs)
      | _ -> assert false
    end else (kf, map_args2 argsf env, (join_stack stack ((map_stack2 stack' env)@stack)), allocations)::(List.map (fun (_, kt, argst) -> kt, map_args2 argst env, (join_stack stack ((map_stack2 stack' env)@stack)), allocations) matchs)
  | Return x -> begin match stack with
      | [] -> []
      | (k, args)::stack' -> [k, (Env.get2 env x)::args, stack', allocations]
    end
  | Call (x, args, stack') -> begin
    match get env x allocations with
    | Pointer_domain d -> List.map (fun k -> k, map_args2 args env, (join_stack stack ((map_stack2 stack' env)@stack)), allocations) (Pointer_domain.to_list d)
    | _ -> assert false end




let has3 env var allocations = List.exists (fun ((var', allocations'), _) -> map var = map var' && allocations = allocations') env
let get3 env var allocations =
  match List.find_opt (fun ((var', allocations'), _) -> map var = map var' && allocations = allocations') env with
  | Some (_, v) -> v
  | None -> assert false
;;

let rec analysis (conts: (pointer * Values.t list * ((pointer * Values.t list) list) * value_domain Allocations.t) list) (prog: cont) (map: ((((address * Values.t list) list * value_domain Allocations.t) * Values.t list) list) Analysis.t) : (value_domain Allocations.t * Values.t list) Analysis.t =
  match conts with
  | [] -> Analysis.map (fun contexts -> List.fold_left (fun (allocs, acc) ((_, allocations), new_env) -> if acc = [] then allocations, new_env else Allocations.union (fun _ value1 value2 -> Some (join_values value1 value2)) allocs allocations,  List.map2 Values.union acc new_env) (Allocations.empty, []) contexts) map
  | (k, env, stack, allocations)::conts' ->
    if Analysis.mem k map then 
      let old_context = Analysis.find k map in
      if has3 old_context stack allocations then
        let old_env = get3 old_context stack allocations in
        let new_env = join_env old_env env in
        if new_env = old_env
          then analysis conts' prog map
          else let args, cont = get_cont prog k in
            let next_conts = analysis_cont cont stack (map_values args env) allocations in
            analysis (conts'@next_conts) prog (Analysis.add k (((stack, allocations),new_env)::old_context) map)
      else
        let args, cont = get_cont prog k in
        let next_conts = analysis_cont cont stack (map_values args env) allocations in
        analysis (conts'@next_conts) prog (Analysis.add k (((stack, allocations),env)::old_context) map)
    else
      let args, cont = get_cont prog k in
      let next_conts = analysis_cont cont stack (map_values args env) allocations in
      analysis (conts'@next_conts) prog (Analysis.add k [(stack, allocations),env] map)

let start_analysis prog args = analysis [0, args, [], Allocations.empty] prog (Analysis.empty)


let pp_alloc fmt (alloc: Values.t) =
  Values.iter (fun i -> Format.fprintf fmt "%d " i) alloc

let rec pp_value_domain fmt = function
| Int_domain d ->  Int_domain.pp fmt d
| Pointer_domain d -> Pointer_domain.pp fmt d
| Tuple_domain values -> Format.fprintf fmt "[%a]" (pp_env "") values
| Closure_domain clos -> Format.fprintf fmt "Closure:"; Closures.iter (fun k env -> Format.fprintf fmt " %d: %a" k (pp_env "") env) clos

and pp_env empty fmt args =
  match args with
  | [] -> Format.fprintf fmt "%s" empty
  | [arg] -> Format.fprintf fmt "{%a}" pp_alloc arg
  | arg::args' -> Format.fprintf fmt "{%a} %a" pp_alloc arg (pp_env empty) args'

let pp_allocations fmt (allocations: value_domain Allocations.t) =
  Allocations.iter (fun i v -> Format.fprintf fmt "%d: %a\n" i pp_value_domain v) allocations

let pp_analysis fmt (map: (value_domain Allocations.t * Values.t list) Analysis.t) = Format.fprintf fmt "Analysis:\n\n"; Analysis.iter (fun k (allocations, env) -> Format.fprintf fmt "k%d %a:\n%a\n\n" k (pp_env "") env pp_allocations allocations) map

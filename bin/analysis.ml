module Analysis = Map.Make (Int)

module Allocations = Map.Make (Int)

module Closures = Map.Make (Int)

module Values = Set.Make (Int)

type value_domain =
  | Int_domain of Int_domain.t
  | Tuple_domain of Values.t list
  | Closure_domain of (Values.t list) Closures.t

type cont_type =
| Cont of Values.t list
| Clos of Values.t list * Values.t list
| Return of Values.t * Values.t list

let value_cmp v1 v2 =
  match v1, v2 with
  | Int_domain d1, Int_domain d2 -> d1 = d2
  | Tuple_domain values1, Tuple_domain values2 -> List.fold_left2 (fun equal value1 value2 -> equal && Values.equal value1 value2) true values1 values2
  | Closure_domain clos1, Closure_domain clos2 -> Closures.equal (fun values1 values2 -> List.fold_left2 (fun equal value1 value2 -> equal && Values.equal value1 value2) true values1 values2) clos1 clos2
  | _ -> assert false

type t = (value_domain list) Analysis.t

type var = Cps.var

type stack = Cps.frame

type prim = Cps.prim
type named = Cps.named
type pointer = Cps.pointer
type expr = Cps.expr
type address = pointer
type cont = Cps.blocks

let get_cont (cont: cont) k =
match Cps.BlockMap.find_opt k cont with
| Some (Cont args, e1) -> args, e1
| _ -> assert false

let pp_alloc fmt (alloc: Values.t) = Format.fprintf fmt "{ "; Values.iter (fun i -> Format.fprintf fmt "%d " i) alloc; Format.fprintf fmt "}"

let rec pp_value_domain fmt = function
| Int_domain d ->  Int_domain.pp fmt d
| Tuple_domain values -> Format.fprintf fmt "[%a]" (pp_env "") values
| Closure_domain clos -> Format.fprintf fmt "Closure:"; Closures.iter (fun k env -> Format.fprintf fmt " %d: %a" k (pp_env "") env) clos

and pp_env _empty fmt args = Format.fprintf fmt "[ "; List.iter (Format.fprintf fmt "%a " pp_alloc) args; Format.fprintf fmt "]"

let pp_frame fmt (k, env) = Format.fprintf fmt "(%d: %a)" k (pp_env "") env
  
let pp_stack fmt stack =
  Printf.printf "[";
  List.iter (fun frame -> pp_frame fmt frame) stack;
  Printf.printf "]\n"

let pp_allocations fmt (allocations: value_domain Allocations.t) = Allocations.iter (fun i v -> Format.fprintf fmt "%d: %a\n" i pp_value_domain v) allocations

let pp_block fmt block =
  match block with
  | Cont args -> Format.fprintf fmt "Block %a" (pp_env "") args
  | Clos (env, args) -> Format.fprintf fmt "Closure %a %a" (pp_env "") env (pp_env "") args
  | Return (arg, args) -> Format.fprintf fmt "Return %a %a" pp_alloc arg (pp_env "") args

let pp_analysis fmt (map: (value_domain Allocations.t * cont_type) Analysis.t) = Format.fprintf fmt "Analysis:\n\n"; Analysis.iter (fun k (allocations, block) -> Format.fprintf fmt "k%d %a:\n%a\n\n" k pp_block block pp_allocations allocations) map

let get = Env.get2

let _get2 env var =
  match List.find_opt (fun (var', _) -> var = var') env with
  | Some (_, v) -> v
  | None -> assert false
;;


let map_values args values = List.map2 (fun arg value -> arg, value) args values

let rec cherche_motif motif liste =
  match motif, liste with
  | [], _ -> true
  | m'::motif', l'::liste' when m' = l' -> cherche_motif motif' liste'
  | _, _ -> false

let rec cherche_periode periode liste =
  match liste with
  | [] -> 0
  | l::liste' -> if cherche_motif (periode@[l]) liste' then List.length (periode@[l]) else cherche_periode (periode@[l]) liste'


let join_env (old_env: 'a list) (new_env: 'a list): 'a list = List.map2 Values.union old_env new_env

let join_values v1 v2 = match v1, v2 with
| Int_domain d1, Int_domain d2 -> Int_domain (Int_domain.join d1 d2)
| Tuple_domain values1, Tuple_domain values2 -> Tuple_domain (List.map2 Values.union values1 values2)
| Closure_domain clos1, Closure_domain clos2 -> Closure_domain (Closures.union (fun _ env1 env2 -> Some (join_env env1 env2)) clos1 clos2)
| _ -> assert false



let rec join_value_list (values: value_domain list): value_domain =
  match values with
  | [] -> assert false
  | [value] -> value
  | value :: values' -> join_values value (join_value_list values')

let rec remove_n_list n list =
  if n = 0 then list else match list with
  | _::l' -> remove_n_list (n-1) l'
  | _ -> assert false

let join_stack _old_stack new_stack = let n = cherche_periode [] new_stack in remove_n_list n new_stack

let join_allocs allocs allocations =
  if Values.is_empty allocs
  then None
  else begin
    let values = Values.elements allocs in
    let values_domain = List.map (fun alloc -> Allocations.find alloc allocations) values in
    Some (join_value_list values_domain)
  end

let has (env: (address * Values.t) list) value =
  let allocs = get env value in not (Values.is_empty allocs)

let get2 (env: (address * Values.t) list) value allocations =
  let allocs = get env value in match join_allocs allocs allocations with Some value -> value | _ -> assert false

let get (env: (address * Values.t) list) value allocations =
  let allocs = get env value in join_allocs allocs allocations

let analysis_prim (prim : prim) args (env: (address * Values.t) list) (allocations: value_domain Allocations.t): value_domain option =
  match prim, args with
  | Const x, _ -> Some (Int_domain (Int_domain.singleton x))
  | Add, x1 :: x2 :: _ -> begin match get env x1 allocations, get env x2 allocations with
    | Some (Int_domain d1), Some (Int_domain d2) when Int_domain.is_singleton d1 && Int_domain.is_singleton d2 -> Some (Int_domain (Int_domain.singleton ((Int_domain.get_singleton d1) + (Int_domain.get_singleton d2))))
    | Some (Int_domain _), Some (Int_domain _) -> Some (Int_domain (Int_domain.top))
    | Some (Int_domain _), None | None, Some (Int_domain _) | None, None -> Some (Int_domain (Int_domain.top))
    | _ -> assert false
    end
  | Sub, x1 :: x2 :: _ -> begin match get env x1 allocations, get env x2 allocations with
    | Some (Int_domain d1), Some (Int_domain d2) when Int_domain.is_singleton d1 && Int_domain.is_singleton d2 -> Some (Int_domain (Int_domain.singleton ((Int_domain.get_singleton d1) - (Int_domain.get_singleton d2))))
    | Some (Int_domain _), Some (Int_domain _) -> Some (Int_domain (Int_domain.top))
    | Some (Int_domain _), None | None, Some (Int_domain _) | None, None -> Some (Int_domain (Int_domain.top))
    | _ -> assert false
    end
  | Print, _ :: _ -> None
  | _ -> assert false

let map_args2 (args: var list) (env: (address * Values.t) list) = List.map (fun arg -> Env.get2 env arg) args

let map_stack2 (k'', args') (env) = k'', map_args2 args' env

let analysis_named (named : named) (env: (address * Values.t) list) (allocations: value_domain Allocations.t): value_domain option =
  match named with
  | Var var' -> get env var' allocations
  | Prim (prim, args) -> analysis_prim prim args env allocations
  | Tuple vars -> Some (Tuple_domain (map_args2 vars env))
  | Get (var', pos) -> begin
      match get env var' allocations with
      | Some (Tuple_domain values) -> join_allocs (List.nth values pos) allocations
      | None -> None
      | _ -> assert false
    end
  | Closure (k, values) -> Some (Closure_domain (Closures.singleton k (map_args2 values env)))
  (* TODO *)
  | Constructor (tag, environment) -> Some (Closure_domain (Closures.singleton tag (map_args2 environment env)))


let rec analysis_cont (cps: expr) (stack: ((pointer * Values.t list) list)) (env: (address * Values.t) list) (allocations: value_domain Allocations.t): (int * cont_type * ((pointer * Values.t list) list) * value_domain Allocations.t) list =
  match cps with
  | Let (var, Var var', expr) -> begin
      analysis_cont expr stack ((var, Env.get2 env var')::env) allocations
    end
  | Let (var, named, expr) -> begin
      let value = analysis_named named env allocations in
      match value with
      | Some value' -> analysis_cont expr stack ((var, Values.singleton var)::env) (Allocations.add var value' allocations)
      | None -> analysis_cont expr stack ((var, Values.empty)::env) allocations
    end
  | Apply_block (k', args) -> [k', Cont (map_args2 args env), stack, allocations]
  | If (var, matchs, (kf, argsf)) -> begin
      match get env var allocations with
      | Some (Int_domain i) when Int_domain.is_singleton i -> begin
        match List.find_opt (fun (n', _, _) -> Int_domain.get_singleton i = n') matchs with
        | Some (_, kt, argst) -> [kt, Cont (map_args2 argst env), stack, allocations]
        | None -> [kf, Cont (map_args2 argsf env), stack, allocations]
        end
      | Some (Int_domain _) | None -> (kf, Cont (map_args2 argsf env), stack, allocations)::(List.map (fun (_, kt, argst) -> kt, Cont (map_args2 argst env), stack, allocations) matchs)
      | _ -> assert false
    end
  | Match_pattern (var, matchs, (kf, argsf)) -> begin
      match get env var allocations with
      | Some (Closure_domain clos) -> List.map (fun (n, env') -> begin
          match List.find_opt (fun (n', _, _) -> n = n') matchs with
          | Some (_, k, args) -> k, Clos (env', map_args2 args env), stack, allocations
          | None -> kf, Cont (map_args2 argsf env), stack, allocations
          end) (Closures.bindings clos)
      | None -> (kf, Cont (map_args2 argsf env), stack, allocations)::(List.map (fun (_, kt, argst) -> kt, Cont (map_args2 argst env), stack, allocations) matchs)
      | _ -> assert false
    end
  | Return x -> begin
      match stack with
      | [] -> []
      | (k, args)::stack' -> [k, Return (Env.get2 env x, args), stack', allocations]
    end
  | Call (x, args, frame) -> begin
      match get env x allocations with
      | Some (Closure_domain clos) -> List.map (fun (k, env') -> k, Clos (env', map_args2 args env), (join_stack stack (map_stack2 frame env :: stack)), allocations) (Closures.bindings clos)
      | _ -> assert false
    end
  | Call_direct (_k, x, args, frame) -> begin
      match get env x allocations with
      | Some (Closure_domain clos) -> assert (Closures.cardinal clos = 1); List.map (fun (k, env') -> k, Clos (env', map_args2 args env), (join_stack stack (map_stack2 frame env :: stack)), allocations) (Closures.bindings clos)
      | _ -> assert false
    end

let block_env (block1: Cps.block) (block2: cont_type) =
  match block1, block2 with
  | Cont args1, Cont args2 -> map_values args1 args2
  | Clos (env1, args1), Clos (env2, args2) -> (map_values args1 args2) @ (map_values env1 env2)
  | Return (arg1, args1), Return (arg2, args2) -> (arg1, arg2) :: (map_values args1 args2)
  | _, _ -> assert false

let has3 context var env = List.exists (fun ((var', _), env') -> var = var' && env = env') context
let get3 context var env =
  match List.find_opt (fun ((var', _'), env') -> var = var' && env = env') context with
  | Some ((_, allocations), _) -> allocations
  | None -> assert false
;;

let join_allocations a b = Allocations.union (fun _ value1 value2 -> Some (join_values value1 value2)) a b

let join_blocks (b1: cont_type) (b2: cont_type) =
  match b1, b2 with
  | Cont env1, Cont env2 -> Cont (List.map2 Values.union env1 env2)
  | Clos (env1, args1), Clos (env2, args2) -> Clos (List.map2 Values.union env1 env2, List.map2 Values.union args1 args2)
  | Return (arg1, args1), Return (arg2, args2) -> Return (Values.union arg1 arg2, List.map2 Values.union args1 args2)
  | _ -> assert false

let rec analysis (conts: (int * cont_type * ((pointer * Values.t list) list) * value_domain Allocations.t) list) (prog: cont) (map: ((((address * Values.t list) list * value_domain Allocations.t) * cont_type) list) Analysis.t) : (value_domain Allocations.t * cont_type) Analysis.t =
  match conts with
  | [] -> Analysis.map (fun contexts -> List.fold_left (fun (allocs, acc) ((_, allocations), new_env) -> join_allocations allocs allocations, join_blocks acc new_env) (let ((_, allocations), new_env) = List.hd contexts in allocations, new_env) (List.tl contexts)) map
  | (k, block', stack, allocations) :: conts' -> begin
    Format.fprintf Format.std_formatter "/// k%d %a Stack: %a Allocs: %a\n" k pp_block  block' pp_stack stack pp_allocations allocations;

      if Analysis.mem k map then begin
        let old_context = Analysis.find k map in
        if has3 old_context stack block' then begin
          let old_allocations = get3 old_context stack block' in
          let new_allocations = join_allocations old_allocations allocations in
          if Allocations.equal value_cmp new_allocations old_allocations then begin
            match stack with
            | [] -> assert false
            | (k', args) :: _ -> analysis ((k', Return (Values.empty, args), stack, new_allocations) :: conts') prog map
          end else begin
            let block, expr = Cps.BlockMap.find k prog in
            let next_conts = analysis_cont expr stack (block_env block block') new_allocations in
            analysis (conts'@next_conts) prog (Analysis.add k (((stack, new_allocations),block')::old_context) map)
          end
        end else begin
          let block, expr = Cps.BlockMap.find k prog in
          let next_conts = analysis_cont expr stack (block_env block block') allocations in
          analysis (conts'@next_conts) prog (Analysis.add k (((stack, allocations),block')::old_context) map)
        end
      end else begin
        let block, expr = Cps.BlockMap.find k prog in
        let next_conts = analysis_cont expr stack (block_env block block') allocations in
        analysis (conts'@next_conts) prog (Analysis.add k [(stack, allocations),block'] map)
      end
    end

let start_analysis prog = let args, _ = get_cont prog 0 in analysis [0, Cont (List.map (fun _ -> Values.empty) args), [], Allocations.empty] prog (Analysis.empty)

let map_args2 = map_args2
let join_allocs = join_allocs

let propagation_prim (prim : prim) args (env: (address * Values.t) list) (allocations: value_domain Allocations.t): named * value_domain option =
  match prim, args with
  | Const x, _ -> Prim (prim, args), Some (Int_domain (Int_domain.singleton x))
  | Add, x1 :: x2 :: _ -> begin match get env x1 allocations, get env x2 allocations with
    | Some (Int_domain d1), Some (Int_domain d2) when Int_domain.is_singleton d1 && Int_domain.is_singleton d2 -> Prim (Const ((Int_domain.get_singleton d1) + (Int_domain.get_singleton d2)), args), Some (Int_domain (Int_domain.singleton ((Int_domain.get_singleton d1) + (Int_domain.get_singleton d2))))
    | Some (Int_domain _), Some (Int_domain _) -> Prim (prim, args), Some (Int_domain (Int_domain.top))
    | Some (Int_domain _), None | None, Some (Int_domain _) | None, None -> Prim (prim, args), Some (Int_domain (Int_domain.top))
    | _ -> assert false
    end
  | Sub, x1 :: x2 :: _ -> begin match get env x1 allocations, get env x2 allocations with
    | Some (Int_domain d1), Some (Int_domain d2) when Int_domain.is_singleton d1 && Int_domain.is_singleton d2 -> Prim (Const ((Int_domain.get_singleton d1) - (Int_domain.get_singleton d2)), args), Some (Int_domain (Int_domain.singleton ((Int_domain.get_singleton d1) - (Int_domain.get_singleton d2))))
    | Some (Int_domain _), Some (Int_domain _) -> Prim (prim, args), Some (Int_domain (Int_domain.top))
    | Some (Int_domain _), None | None, Some (Int_domain _) | None, None -> Prim (prim, args), Some (Int_domain (Int_domain.top))
    | _ -> assert false
    end
  | Print, _ :: _ -> Prim (Print, args), None
  | _ -> assert false

let propagation_named (named : Cps.named) (env: (address * Values.t) list) (allocations: value_domain Allocations.t): named * value_domain option =
match named with
| Var var' -> Var var', get env var' allocations
| Prim (prim, args) -> propagation_prim prim args env allocations
| Tuple vars -> Tuple vars, Some (Tuple_domain (map_args2 vars env))
| Get (var', pos) -> begin
    match get env var' allocations with
    | Some (Tuple_domain values) -> Get (var', pos), join_allocs (List.nth values pos) allocations
    | None -> Get (var', pos), None
    | _ -> assert false
  end
| Closure (k, values) -> Closure (k, values), Some (Closure_domain (Closures.singleton k (map_args2 values env)))
| Constructor (tag, environment) -> Constructor (tag, environment), Some (Closure_domain (Closures.singleton tag (map_args2 environment env)))

let rec propagation (cps : Cps.expr) (env: (pointer * Values.t) list) (allocations: value_domain Allocations.t): expr =
  match cps with
  | Let (var, named, expr) -> begin
      let named, value = propagation_named named env allocations in
      match value with
      | Some value' -> Let (var, named, propagation expr ((var, Values.singleton var)::env) (Allocations.add var value' allocations))
      | None -> Let (var, named, propagation expr ((var, Values.empty)::env) allocations)
    end
  | Apply_block (k', args) -> Apply_block (k', args)
  | If (var, matchs, (kf, argsf)) -> begin
      match get env var allocations with
      | Some (Int_domain i) when Int_domain.is_singleton i -> begin
        match List.find_opt (fun (n', _, _) -> Int_domain.get_singleton i = n') matchs with
        | Some (_, kt, argst) -> Apply_block (kt, argst)
        | None -> Apply_block (kf, argsf)
        end
      | Some (Int_domain _) | None -> If (var, matchs, (kf, argsf))
      | _ -> assert false
    end
  | Match_pattern (var, matchs, (kf, argsf)) -> begin
    match get env var allocations with
    | Some (Closure_domain clos) -> Match_pattern (var, List.filter (fun (n, _, _) -> List.exists (fun (n', _) -> n = n') (Closures.bindings clos)) matchs, (kf, argsf))
    | None -> Match_pattern (var, matchs, (kf, argsf))
    | _ -> assert false
  end
  | Return x -> Return x
  | Call (x, args, stack) -> begin
      match get env x allocations with
      | Some (Closure_domain clos) when Closures.cardinal clos = 1 -> let (k, _) = Closures.choose clos in Call_direct (k, x, args, stack)
      | Some _ | None -> Call (x, args, stack)
    end
  | Call_direct (k, x, args, stack) -> Call_direct (k, x, args, stack)

let propagation_blocks (blocks: Cps.blocks) (map: (value_domain Allocations.t * cont_type) Analysis.t) =
  Cps.BlockMap.mapi (fun k (block, expr) -> begin
    if Analysis.mem k map then
      let allocations, block' = Analysis.find k map in
      block, propagation expr (block_env block block') allocations
    else block, expr
  end) blocks

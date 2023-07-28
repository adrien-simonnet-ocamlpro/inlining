module Values = Set.Make (Int)

type value_domain =
  | Int_domain of Int_domain.t
  | Tuple_domain of Values.t list
  | Closure_domain of (Values.t list) Cps.PointerMap.t

type cont_type =
| Cont of Values.t list
| Clos of Values.t list * Values.t list
| Return of Values.t * Values.t list
| If_branch of Values.t list * Values.t list
| If_join of Values.t * Values.t list
| Match_branch of Values.t list * Values.t list * Values.t list
| Match_join of Values.t * Values.t list

let value_cmp v1 v2 =
  match v1, v2 with
  | Int_domain d1, Int_domain d2 -> d1 = d2
  | Tuple_domain values1, Tuple_domain values2 -> List.fold_left2 (fun equal value1 value2 -> equal && Values.equal value1 value2) true values1 values2
  | Closure_domain clos1, Closure_domain clos2 -> Cps.PointerMap.equal (fun values1 values2 -> List.fold_left2 (fun equal value1 value2 -> equal && Values.equal value1 value2) true values1 values2) clos1 clos2
  | _ -> assert false

type t = (value_domain list) Cps.PointerMap.t

type var = Cps.var

type stack = Cps.frame

type prim = Cps.prim
type named = Cps.named
type pointer = Cps.pointer
type expr = Cps.expr
type address = pointer
type cont = Cps.blocks

let get_cont (cont: cont) k =
match Cps.PointerMap.find_opt k cont with
| Some (Cont args, e1) -> args, e1
| _ -> assert false

let pp_alloc fmt (alloc: Values.t) = Format.fprintf fmt "{ "; Values.iter (fun i -> Format.fprintf fmt "%d " i) alloc; Format.fprintf fmt "}"

let rec pp_value_domain fmt = function
| Int_domain d ->  Int_domain.pp fmt d
| Tuple_domain values -> Format.fprintf fmt "[%a]" (pp_env "") values
| Closure_domain clos -> Format.fprintf fmt "Closure:"; Cps.PointerMap.iter (fun k env -> Format.fprintf fmt " %d: %a" k (pp_env "") env) clos

and pp_env _empty fmt args = Format.fprintf fmt "[ "; List.iter (Format.fprintf fmt "%a " pp_alloc) args; Format.fprintf fmt "]"

let pp_frame fmt (k, env) = Format.fprintf fmt "(%d: %a)" k (pp_env "") env
  
let pp_stack fmt stack =
  Printf.printf "[";
  List.iter (fun frame -> pp_frame fmt frame) stack;
  Printf.printf "]"

let pp_allocations fmt (allocations: value_domain Cps.VarMap.t) = Cps.VarMap.iter (fun i v -> Format.fprintf fmt "%d: %a\n" i pp_value_domain v) allocations

let pp_block fmt block =
  match block with
  | Cont args -> Format.fprintf fmt "Cont %a" (pp_env "") args
  | Clos (env, args) -> Format.fprintf fmt "Closure %a %a" (pp_env "") env (pp_env "") args
  | Return (arg, args) -> Format.fprintf fmt "Return %a %a" pp_alloc arg (pp_env "") args
  | If_branch (args, fvs) -> Format.fprintf fmt "If_branch %a %a" (pp_env "") args (pp_env "") fvs
  | If_join (arg, args) -> Format.fprintf fmt "If_join %a %a" pp_alloc arg (pp_env "") args
  | Match_branch (env, args, fvs) -> Format.fprintf fmt "Match_branch %a %a %a" (pp_env "") env (pp_env "") args (pp_env "") fvs
  | Match_join (arg, args) -> Format.fprintf fmt "Match_join %a %a" pp_alloc arg (pp_env "") args

let pp_analysis fmt (map: (value_domain Cps.VarMap.t * cont_type) Cps.PointerMap.t) = Format.fprintf fmt "Cps.PointerMap:\n\n"; Cps.PointerMap.iter (fun k (allocations, block) -> Format.fprintf fmt "k%d %a:\n%a\n\n" k pp_block block pp_allocations allocations) map

let get = Env.get2

let _get2 env var =
  match List.find_opt (fun (var', _) -> var = var') env with
  | Some (_, v) -> v
  | None -> assert false
;;


let map_values args values = List.map2 (fun arg value -> arg, value) args values

let join_env (old_env: 'a list) (new_env: 'a list): 'a list = List.map2 Values.union old_env new_env

let join_values v1 v2 = match v1, v2 with
| Int_domain d1, Int_domain d2 -> Int_domain (Int_domain.join d1 d2)
| Tuple_domain values1, Tuple_domain values2 -> Tuple_domain (List.map2 Values.union values1 values2)
| Closure_domain clos1, Closure_domain clos2 -> Closure_domain (Cps.PointerMap.union (fun _ env1 env2 -> Some (join_env env1 env2)) clos1 clos2)
| _ -> assert false



let rec join_value_list (values: value_domain list): value_domain =
  match values with
  | [] -> assert false
  | [value] -> value
  | value :: values' -> join_values value (join_value_list values')

let join_allocs allocs allocations =
  if Values.is_empty allocs
  then None
  else begin
    let values = Values.elements allocs in
    let values_domain = List.map (fun alloc -> Cps.VarMap.find alloc allocations) values in
    Some (join_value_list values_domain)
  end

let has (env: (address * Values.t) list) value =
  let allocs = get env value in not (Values.is_empty allocs)

let get2 (env: (address * Values.t) list) value allocations =
  let allocs = get env value in match join_allocs allocs allocations with Some value -> value | _ -> assert false

let get (env: (address * Values.t) list) value allocations =
  let allocs = get env value in join_allocs allocs allocations

let analysis_prim (prim : prim) args (env: (address * Values.t) list) (allocations: value_domain Cps.VarMap.t): value_domain option =
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

let analysis_named (named : named) (env: (address * Values.t) list) (allocations: value_domain Cps.VarMap.t): value_domain option =
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
  | Closure (k, values) -> Some (Closure_domain (Cps.PointerMap.singleton k (map_args2 values env)))
  (* TODO *)
  | Constructor (tag, environment) -> Some (Closure_domain (Cps.PointerMap.singleton tag (map_args2 environment env)))


let rec analysis_cont (cps: expr) (stack: ((pointer * Values.t list) list)) (env: (address * Values.t) list) (allocations: value_domain Cps.VarMap.t): (int * cont_type * ((pointer * Values.t list) list) * value_domain Cps.VarMap.t) list =
  match cps with
  | Let (var, Var var', expr) -> begin
      analysis_cont expr stack ((var, Env.get2 env var')::env) allocations
    end
  | Let (var, named, expr) -> begin
      let value = analysis_named named env allocations in
      match value with
      | Some value' -> analysis_cont expr stack ((var, Values.singleton var)::env) (Cps.VarMap.update var (fun value'' -> if Option.is_some value'' then Some (join_values (Option.get value'') value') else Some value') allocations)
      | None -> analysis_cont expr stack ((var, Values.empty)::env) allocations
    end
  | Apply_block (k', args) -> [k', Cont (map_args2 args env), stack, allocations]
  | If (var, matchs, (kf, argsf), fvs) -> begin
      match get env var allocations with
      | Some (Int_domain i) when Int_domain.is_singleton i -> begin
        match List.find_opt (fun (n', _, _) -> Int_domain.get_singleton i = n') matchs with
        | Some (_, kt, argst) -> [kt, If_branch (map_args2 argst env, map_args2 fvs env), stack, allocations]
        | None -> [kf, If_branch (map_args2 argsf env, map_args2 fvs env), stack, allocations]
        end
      | Some (Int_domain _) | None -> (kf, If_branch (map_args2 argsf env, map_args2 fvs env), stack, allocations)::(List.map (fun (_, kt, argst) -> kt, If_branch (map_args2 argst env, map_args2 fvs env), stack, allocations) matchs)
      | _ -> assert false
    end
  | Match_pattern (var, matchs, (kf, argsf), fvs) -> begin
      match get env var allocations with
      | Some (Closure_domain clos) -> List.map (fun (n, env') -> begin
          match List.find_opt (fun (n', _, _, _) -> n = n') matchs with
          | Some (_, k, _, args) -> k, Match_branch (env', map_args2 args env, map_args2 fvs env), stack, allocations
          | None -> kf, Match_branch ([], map_args2 argsf env, map_args2 fvs env), stack, allocations
          end) (Cps.PointerMap.bindings clos)
      | None -> (kf, Match_branch ([], map_args2 argsf env, map_args2 fvs env), stack, allocations)::(List.map (fun (_, kt, pld, argst) -> kt, Match_branch (List.map (fun _ -> Values.empty) pld, map_args2 argst env, map_args2 fvs env), stack, allocations) matchs)
      | _ -> assert false
    end
  | Return x -> begin
      match stack with
      | [] -> []
      | (k, args)::stack' -> [k, Return (Env.get2 env x, args), stack', allocations]
    end
  | If_return (k, arg, args) -> [k, If_join (Env.get2 env arg, map_args2 args env), stack, allocations]
  | Match_return (k, arg, args) -> [k, Match_join (Env.get2 env arg, map_args2 args env), stack, allocations]
  | Call (x, args, frame) -> begin
      match get env x allocations with
      | Some (Closure_domain clos) -> List.map (fun (k, env') -> k, Clos (env', map_args2 args env), (map_stack2 frame env :: stack), allocations) (Cps.PointerMap.bindings clos)
      | _ -> assert false
    end
  | Call_direct (k, x, args, frame) -> begin
      match get env x allocations with
      | Some (Closure_domain clos) -> assert (Cps.PointerMap.cardinal clos = 1); List.map (fun (_, env') -> k, Clos (env', map_args2 args env), (map_stack2 frame env :: stack), allocations) (Cps.PointerMap.bindings clos)
      | _ -> assert false
    end

let block_env (block1: Cps.block) (block2: cont_type) =
  match block1, block2 with
  | Cont args1, Cont args2 -> map_values args1 args2
  | Clos (env1, args1), Clos (env2, args2) -> (map_values args1 args2) @ (map_values env1 env2)
  | Return (arg1, args1), Return (arg2, args2) -> (arg1, arg2) :: (map_values args1 args2)
  | If_branch (args1, fvs1), If_branch (args2, fvs2) -> (map_values args1 args2) @ (map_values fvs1 fvs2)
  | If_join (arg1, args1), If_join (arg2, args2) -> (arg1, arg2) :: (map_values args1 args2)
  | Match_branch (env1, args1, fvs1), Match_branch (env2, args2, fvs2) -> (map_values env1 env2) @ (map_values args1 args2) @ (map_values fvs1 fvs2)
  | Match_join (arg1, args1), Match_join (arg2, args2) -> (arg1, arg2) :: (map_values args1 args2)
  | _, _ -> assert false

let has3 context var env = List.exists (fun ((var', _), env') -> var = var' && env = env') context
let get3 context var env =
  match List.find_opt (fun ((var', _'), env') -> var = var' && env = env') context with
  | Some ((_, allocations), _) -> allocations
  | None -> assert false
;;

let join_allocations a b = Cps.VarMap.union (fun _ value1 value2 -> Some (join_values value1 value2)) a b

let join_blocks (b1: cont_type) (b2: cont_type) =
  match b1, b2 with
  | Cont env1, Cont env2 -> Cont (List.map2 Values.union env1 env2)
  | Clos (env1, args1), Clos (env2, args2) -> Clos (List.map2 Values.union env1 env2, List.map2 Values.union args1 args2)
  | Return (arg1, args1), Return (arg2, args2) -> Return (Values.union arg1 arg2, List.map2 Values.union args1 args2)
  | If_branch (args1, fvs1), If_branch (args2, fvs2) -> If_branch (List.map2 Values.union args1 args2, List.map2 Values.union fvs1 fvs2)
  | If_join (arg1, args1), If_join (arg2, args2) -> If_join (Values.union arg1 arg2, List.map2 Values.union args1 args2)
  | Match_branch (env1, args1, fvs1), Match_branch (env2, args2, fvs2) -> Match_branch (List.map2 Values.union env1 env2, List.map2 Values.union args1 args2, List.map2 Values.union fvs1 fvs2)
  | Match_join (arg1, args1), Match_join (arg2, args2) -> Match_join (Values.union arg1 arg2, List.map2 Values.union args1 args2)
  | _, _ -> assert false

type stack_analysis = (address * Values.t list) list

let rec analysis (conts: (int * cont_type * ((pointer * Values.t list) list) * value_domain Cps.VarMap.t) list) (reduce: stack_analysis -> stack_analysis) (prog: cont) (map: ((((address * Values.t list) list * value_domain Cps.VarMap.t) * cont_type) list) Cps.PointerMap.t) : (value_domain Cps.VarMap.t * cont_type) Cps.PointerMap.t =
  match conts with
  | [] -> Cps.PointerMap.map (fun contexts -> List.fold_left (fun (allocs, acc) ((_, allocations), new_env) -> join_allocations allocs allocations, join_blocks acc new_env) (let ((_, allocations), new_env) = List.hd contexts in allocations, new_env) (List.tl contexts)) map
  | (k, block', stack''', allocations) :: conts' -> begin
    Logger.start "k%d %a Stack: %a Allocs: %a\n" k pp_block  block' pp_stack stack''' pp_allocations allocations;
    Logger.stop ();

      let stack = reduce stack''' in

      (* Already seen this block. *)
      if Cps.PointerMap.mem k map then begin
        let old_context = Cps.PointerMap.find k map in
        
        (* Already seen this context. *)
        if has3 old_context stack block' then begin
          let old_allocations = get3 old_context stack block' in
          let new_allocations = join_allocations old_allocations allocations in
          (* Already seen these allocations. *)
          if Cps.VarMap.equal value_cmp new_allocations old_allocations then begin
            match stack''' with
            | [] -> analysis conts' reduce prog map
            | (k', args) :: _stack' -> analysis ((k', Return (Values.empty, args), _stack', new_allocations) :: conts') reduce prog map
          end else begin
            let block, expr = Cps.PointerMap.find k prog in
            let next_conts = analysis_cont expr stack''' (block_env block block') new_allocations in
            analysis (conts'@next_conts) reduce prog (Cps.PointerMap.add k (((stack, new_allocations),block')::old_context) map)
          end
        end else begin
          let block, expr = Cps.PointerMap.find k prog in
          let next_conts = analysis_cont expr stack''' (block_env block block') allocations in
          analysis (conts'@next_conts) reduce prog (Cps.PointerMap.add k (((stack, allocations),block')::old_context) map)
        end
      end else begin
        let block, expr = Cps.PointerMap.find k prog in
        let next_conts = analysis_cont expr stack''' (block_env block block') allocations in
        analysis (conts'@next_conts) reduce prog (Cps.PointerMap.add k [(stack, allocations),block'] map)
      end
    end

let start_analysis reduce prog = let args, _ = get_cont prog 0 in analysis [0, Cont (List.map (fun _ -> Values.empty) args), [], Cps.VarMap.empty] reduce prog (Cps.PointerMap.empty)

let map_args2 = map_args2
let join_allocs = join_allocs

let propagation_prim (prim : prim) args (env: (address * Values.t) list) (allocations: value_domain Cps.VarMap.t): named * value_domain option =
  match prim, args with
  | Const x, _ -> Prim (prim, args), Some (Int_domain (Int_domain.singleton x))
  | Add, x1 :: x2 :: _ -> begin match get env x1 allocations, get env x2 allocations with
    | Some (Int_domain d1), Some (Int_domain d2) when Int_domain.is_singleton d1 && Int_domain.is_singleton d2 -> Prim (Const ((Int_domain.get_singleton d1) + (Int_domain.get_singleton d2)), []), Some (Int_domain (Int_domain.singleton ((Int_domain.get_singleton d1) + (Int_domain.get_singleton d2))))
    | Some (Int_domain _), Some (Int_domain _) -> Prim (prim, args), Some (Int_domain (Int_domain.top))
    | Some (Int_domain _), None | None, Some (Int_domain _) | None, None -> Prim (prim, args), Some (Int_domain (Int_domain.top))
    | _ -> assert false
    end
  | Sub, x1 :: x2 :: _ -> begin match get env x1 allocations, get env x2 allocations with
    | Some (Int_domain d1), Some (Int_domain d2) when Int_domain.is_singleton d1 && Int_domain.is_singleton d2 -> Prim (Const ((Int_domain.get_singleton d1) - (Int_domain.get_singleton d2)), []), Some (Int_domain (Int_domain.singleton ((Int_domain.get_singleton d1) - (Int_domain.get_singleton d2))))
    | Some (Int_domain _), Some (Int_domain _) -> Prim (prim, args), Some (Int_domain (Int_domain.top))
    | Some (Int_domain _), None | None, Some (Int_domain _) | None, None -> Prim (prim, args), Some (Int_domain (Int_domain.top))
    | _ -> assert false
    end
  | Print, _ :: _ -> Prim (Print, args), None
  | _ -> assert false

let propagation_named (named : Cps.named) (env: (address * Values.t) list) (allocations: value_domain Cps.VarMap.t): named * value_domain option =
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
| Closure (k, values) -> Closure (k, values), Some (Closure_domain (Cps.PointerMap.singleton k (map_args2 values env)))
| Constructor (tag, environment) -> Constructor (tag, environment), Some (Closure_domain (Cps.PointerMap.singleton tag (map_args2 environment env)))

let rec propagation (cps : Cps.expr) (env: (pointer * Values.t) list) (allocations: value_domain Cps.VarMap.t): expr =
  match cps with
  | Let (var, named, expr) -> begin
      let named, value = propagation_named named env allocations in
      match value with
      | Some value' -> Let (var, named, propagation expr ((var, Values.singleton var)::env) (Cps.VarMap.add var value' allocations))
      | None -> Let (var, named, propagation expr ((var, Values.empty)::env) allocations)
    end
  | Apply_block (k', args) -> Apply_block (k', args)
  | If (var, matchs, (kf, argsf), fvs) -> begin
      match get env var allocations with
      | Some (Int_domain i) when Int_domain.is_singleton i -> begin
        match List.find_opt (fun (n', _, _) -> Int_domain.get_singleton i = n') matchs with
        | Some (_, kt, argst) -> If (var, [], (kt, argst), fvs)
        | None -> If (var, [], (kf, argsf), fvs)
        end
      | Some (Int_domain _) | None -> If (var, matchs, (kf, argsf), fvs)
      | _ -> assert false
    end
  | Match_pattern (var, matchs, (kf, argsf), fvs) -> begin
      match get env var allocations with
      | Some (Closure_domain clos) -> Match_pattern (var, List.filter (fun (n, _, _, _) -> List.exists (fun (n', _) -> n = n') (Cps.PointerMap.bindings clos)) matchs, (kf, argsf), fvs)
      | None -> Match_pattern (var, matchs, (kf, argsf), fvs)
      | _ -> assert false
    end
  | Return x -> Return x
  | If_return (k, arg, args) -> If_return (k, arg, args)
  | Match_return (k, arg, args) -> Match_return (k, arg, args)
  | Call (x, args, stack) -> begin
      match get env x allocations with
      | Some (Closure_domain clos) when Cps.PointerMap.cardinal clos = 1 -> let (k, _) = Cps.PointerMap.choose clos in Call_direct (k, x, args, stack)
      | Some _ | None -> Call (x, args, stack)
    end
  | Call_direct (k, x, args, stack) -> Call_direct (k, x, args, stack)

let propagation_blocks (blocks: Cps.blocks) (map: (value_domain Cps.VarMap.t * cont_type) Cps.PointerMap.t) =
  Cps.PointerMap.mapi (fun k (block, expr) -> begin
    if Cps.PointerMap.mem k map then
      let allocations, block' = Cps.PointerMap.find k map in
      block, propagation expr (block_env block block') allocations
    else block, expr
  end) blocks

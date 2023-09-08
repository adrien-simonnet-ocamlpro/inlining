type value_domain =
  | Int_domain of Int_domain.t
  | Tuple_domain of Cps.VarSet.t list
  | Closure_domain of (Cps.VarSet.t Cps.VarMap.t) Cps.PointerMap.t
  | Constructor_domain of (Cps.VarSet.t list) Cps.PointerMap.t

type cont_type =
| Cont of Cps.VarSet.t Cps.VarMap.t
| Clos of Cps.VarSet.t Cps.VarMap.t * Cps.VarSet.t list
| Return of Cps.VarSet.t * Cps.VarSet.t Cps.VarMap.t
| If_branch of Cps.VarSet.t Cps.VarMap.t * Cps.VarSet.t Cps.VarMap.t
| If_join of Cps.VarSet.t * Cps.VarSet.t Cps.VarMap.t
| Match_branch of Cps.VarSet.t list * Cps.VarSet.t Cps.VarMap.t * Cps.VarSet.t Cps.VarMap.t
| Match_join of Cps.VarSet.t * Cps.VarSet.t Cps.VarMap.t

let value_cmp v1 v2 =
  match v1, v2 with
  | Int_domain d1, Int_domain d2 -> d1 = d2
  | Tuple_domain values1, Tuple_domain values2 -> List.fold_left2 (fun equal value1 value2 -> equal && Cps.VarSet.equal value1 value2) true values1 values2
  | Closure_domain clos1, Closure_domain clos2 -> Cps.PointerMap.equal (fun values1 values2 -> Cps.VarMap.equal (fun value1 value2 -> Cps.VarSet.equal value1 value2) values1 values2) clos1 clos2
  | Constructor_domain clos1, Constructor_domain clos2 -> Cps.PointerMap.equal (fun values1 values2 -> List.fold_left2 (fun equal value1 value2 -> equal && Cps.VarSet.equal value1 value2) true values1 values2) clos1 clos2
  | _ -> assert false

type t = (value_domain list) Cps.PointerMap.t

type var = Cps.var

type named = Cps.expr
type pointer = Cps.pointer
type expr = Cps.instr
type address = pointer
type cont = Cps.blocks

let get_cont (cont: cont) k =
match Cps.PointerMap.find_opt k cont with
| Some (Cont args, e1) -> args, e1
| _ -> assert false

let pp_alloc fmt (alloc: Cps.VarSet.t) = Format.fprintf fmt "{ "; Cps.VarSet.iter (fun i -> Format.fprintf fmt "%d " i) alloc; Format.fprintf fmt "}"

let rec pp_value_domain fmt = function
| Int_domain d ->  Int_domain.pp fmt d
| Tuple_domain values -> Format.fprintf fmt "[%a]" (pp_args "") values
| Closure_domain clos -> Format.fprintf fmt "Closure:"; Cps.PointerMap.iter (fun k env -> Format.fprintf fmt " %d: %a" k (pp_env "") env) clos
| Constructor_domain clos -> Format.fprintf fmt "Closure:"; Cps.PointerMap.iter (fun k env -> Format.fprintf fmt " %d: %a" k (pp_args "") env) clos

and pp_args _empty fmt args = Format.fprintf fmt "[ "; List.iter (Format.fprintf fmt "%a " pp_alloc) args; Format.fprintf fmt "]"

and pp_env _empty fmt args = Format.fprintf fmt "[ "; Cps.VarMap.iter (fun v a -> Format.fprintf fmt "k%d -> %a " v pp_alloc a) args; Format.fprintf fmt "]"

let pp_frame fmt (k, env) = Format.fprintf fmt "(%d: %a)" k (pp_env "") env
  
let pp_stack fmt stack =
  Printf.printf "[";
  List.iter (fun frame -> pp_frame fmt frame) stack;
  Printf.printf "]"

type environment = Cps.VarSet.t Cps.VarMap.t
type allocations = value_domain Cps.VarMap.t

let pp_allocations fmt (allocations: allocations) = Cps.VarMap.iter (fun i v -> Format.fprintf fmt "%d: %a\n" i pp_value_domain v) allocations

let pp_block fmt block =
  match block with
  | Cont args -> Format.fprintf fmt "Cont %a" (pp_env "") args
  | Clos (env, args) -> Format.fprintf fmt "Closure %a %a" (pp_env "") env (pp_args "") args
  | Return (arg, args) -> Format.fprintf fmt "Return %a %a" pp_alloc arg (pp_env "") args
  | If_branch (args, fvs) -> Format.fprintf fmt "If_branch %a %a" (pp_env "") args (pp_env "") fvs
  | If_join (arg, args) -> Format.fprintf fmt "If_join %a %a" pp_alloc arg (pp_env "") args
  | Match_branch (env, args, fvs) -> Format.fprintf fmt "Match_branch %a %a %a" (pp_args "") env (pp_env "") args (pp_env "") fvs
  | Match_join (arg, args) -> Format.fprintf fmt "Match_join %a %a" pp_alloc arg (pp_env "") args

let pp_analysis fmt (map: (allocations * cont_type) Cps.PointerMap.t) = Format.fprintf fmt "Cps.PointerMap:\n\n"; Cps.PointerMap.iter (fun k (allocations, block) -> Format.fprintf fmt "k%d %a:\n%a\n\n" k pp_block block pp_allocations allocations) map

let get env value = Cps.VarMap.find value env
let get0 env value = Cps.VarMap.find value env

let _get2 env var =
  match List.find_opt (fun (var', _) -> var = var') env with
  | Some (_, v) -> v
  | None -> assert false
;;


let map_values args values = List.fold_left2 (fun env arg1 arg2 -> Cps.VarMap.add arg1 arg2 env) Cps.VarMap.empty args values

let join_args (old_env: 'a list) (new_env: 'a list): 'a list = List.map2 Cps.VarSet.union old_env new_env

let join_env (old_env: Cps.VarSet.t Cps.VarMap.t) (new_env: Cps.VarSet.t Cps.VarMap.t): Cps.VarSet.t Cps.VarMap.t =
  Cps.VarMap.union (fun _ e1 e2 -> Some (Cps.VarSet.union e1 e2)) old_env new_env

let join_values v1 v2 = match v1, v2 with
| Int_domain d1, Int_domain d2 -> Int_domain (Int_domain.join d1 d2)
| Tuple_domain values1, Tuple_domain values2 -> Tuple_domain (List.map2 Cps.VarSet.union values1 values2)
| Closure_domain clos1, Closure_domain clos2 -> Closure_domain (Cps.PointerMap.union (fun _ env1 env2 -> Some (join_env env1 env2)) clos1 clos2)
| Constructor_domain clos1, Constructor_domain clos2 -> Constructor_domain (Cps.PointerMap.union (fun _ env1 env2 -> Some (join_args env1 env2)) clos1 clos2)
| _ -> assert false



let rec join_value_list (values: value_domain list): value_domain =
  match values with
  | [] -> assert false
  | [value] -> value
  | value :: values' -> join_values value (join_value_list values')

let join_allocs allocs allocations =
  if Cps.VarSet.is_empty allocs
  then None
  else begin
    let values = Cps.VarSet.elements allocs in
    let values_domain = List.map (fun alloc -> Cps.VarMap.find alloc allocations) values in
    Some (join_value_list values_domain)
  end



let has (env: environment) value =
  let allocs = get env value in not (Cps.VarSet.is_empty allocs)

let get2 (env: environment) value allocations =
  let allocs = get env value in match join_allocs allocs allocations with Some value -> value | _ -> assert false

let get (env: environment) value allocations =
  let allocs = get env value in join_allocs allocs allocations

let map_args2 (args: var list) (env: environment) = List.map (fun arg -> Cps.VarMap.find arg env) args
let map_env (_args: Cps.VarSet.t) (env: environment): environment =
  Cps.VarMap.filter (fun p _ -> Cps.VarSet.mem p _args) env
let map_stack2 (k'', args') (env) = k'', map_env args' env

let analysis_named (named : named) (env: environment) (allocations: allocations): value_domain option =
  match named with
  | Var var' -> get env var' allocations
  | Const x -> Some (Int_domain (Int_domain.singleton x))
  | Add (x1, x2) -> begin match get env x1 allocations, get env x2 allocations with
    | Some (Int_domain d1), Some (Int_domain d2) when Int_domain.is_singleton d1 && Int_domain.is_singleton d2 -> Some (Int_domain (Int_domain.singleton ((Int_domain.get_singleton d1) + (Int_domain.get_singleton d2))))
    | Some (Int_domain _), Some (Int_domain _) -> Some (Int_domain (Int_domain.top))
    | Some (Int_domain _), None | None, Some (Int_domain _) | None, None -> Some (Int_domain (Int_domain.top))
    | _ -> assert false
    end
  | Sub (x1, x2) -> begin match get env x1 allocations, get env x2 allocations with
    | Some (Int_domain d1), Some (Int_domain d2) when Int_domain.is_singleton d1 && Int_domain.is_singleton d2 -> Some (Int_domain (Int_domain.singleton ((Int_domain.get_singleton d1) - (Int_domain.get_singleton d2))))
    | Some (Int_domain _), Some (Int_domain _) -> Some (Int_domain (Int_domain.top))
    | Some (Int_domain _), None | None, Some (Int_domain _) | None, None -> Some (Int_domain (Int_domain.top))
    | _ -> assert false
    end
  | Print _ -> None
  | Tuple vars -> Some (Tuple_domain (map_args2 vars env))
  | Get (var', pos) -> begin
      match get env var' allocations with
      | Some (Tuple_domain values) -> join_allocs (List.nth values pos) allocations
      | None -> None
      | _ -> assert false
    end
  | Closure (k, values) -> Some (Closure_domain (Cps.PointerMap.singleton k (map_env values env)))
  | Constructor (tag, environment) -> Some (Constructor_domain (Cps.PointerMap.singleton tag (map_args2 environment env)))

type frame = pointer * Cps.VarSet.t Cps.VarMap.t
type stack_allocs = frame list

let rec analysis_cont (cps: expr) (stack: stack_allocs) (env: environment) (allocations: allocations): (pointer * cont_type * stack_allocs * allocations) list =
  match cps with
  | Let (var, Var var', expr) -> begin
      analysis_cont expr stack (Cps.VarMap.add var (get0 env var') env) allocations
    end
  | Let (var, named, expr) -> begin
      let value = analysis_named named env allocations in
      match value with
      | Some value' -> analysis_cont expr stack (Cps.VarMap.add var (Cps.VarSet.singleton var) env) (Cps.VarMap.update var (fun value'' -> if Option.is_some value'' then Some (join_values (Option.get value'') value') else Some value') allocations)
      | None -> analysis_cont expr stack (Cps.VarMap.add var (Cps.VarSet.empty) env) allocations
    end
  | Apply_block (k', args) -> [k', Cont (map_env args env), stack, allocations]
  | If (var, matchs, (kf, argsf), fvs) -> begin
      match get env var allocations with
      | Some (Int_domain i) when Int_domain.is_singleton i -> begin
        match List.find_opt (fun (n', _, _) -> Int_domain.get_singleton i = n') matchs with
        | Some (_, kt, argst) -> [kt, If_branch (map_env argst env, map_env fvs env), stack, allocations]
        | None -> [kf, If_branch (map_env argsf env, map_env fvs env), stack, allocations]
        end
      | Some (Int_domain _) | None -> (kf, If_branch (map_env argsf env, map_env fvs env), stack, allocations)::(List.map (fun (_, kt, argst) -> kt, If_branch (map_env argst env, map_env fvs env), stack, allocations) matchs)
      | _ -> assert false
    end
  | Match_pattern (var, matchs, (kf, argsf), fvs) -> begin
      match get env var allocations with
      | Some (Constructor_domain clos) -> List.map (fun (n, env') -> begin
          match List.find_opt (fun (n', _, _, _) -> n = n') matchs with
          | Some (_, _, k, args) -> k, Match_branch (env', map_env args env, map_env fvs env), stack, allocations
          | None -> kf, Match_branch ([], map_env argsf env, map_env fvs env), stack, allocations
          end) (Cps.PointerMap.bindings clos)
      | None -> (kf, Match_branch ([], map_env argsf env, map_env fvs env), stack, allocations)::(List.map (fun (_, pld, kt, argst) -> kt, Match_branch (List.map (fun _ -> Cps.VarSet.empty) pld, map_env argst env, map_env fvs env), stack, allocations) matchs)
      | _ -> assert false
    end
  | Return x -> begin
      match stack with
      | [] -> []
      | (k, args)::stack' -> [k, Return (get0 env x, args), stack', allocations]
    end
  | If_return (k, arg, args) -> [k, If_join (get0 env arg, map_env args env), stack, allocations]
  | Match_return (k, arg, args) -> [k, Match_join (get0 env arg, map_env args env), stack, allocations]
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

let block_env (block1: Cps.block) (block2: cont_type): environment =
  match block1, block2 with
  | Cont _, Cont args2 -> args2
  | Clos (_, args1), Clos (env2, args2) -> Cps.VarMap.union (fun _ a _-> Some a) (map_values args1 args2) env2
  | Return (arg1, _), Return (arg2, args2) -> Cps.VarMap.add arg1 arg2 args2
  | If_branch (_, _), If_branch (args2, fvs2) -> Cps.VarMap.union (fun _ _ b-> Some b) fvs2 args2
  | If_join (arg1, _), If_join (arg2, args2) -> Cps.VarMap.add arg1 arg2 args2
  | Match_branch (env1, _, _), Match_branch (env2, args2, fvs2) -> Cps.VarMap.union (fun _ _ b-> Some b) (map_values env1 env2) (Cps.VarMap.union (fun _ _ b-> Some b) args2 fvs2)
  | Match_join (arg1, _), Match_join (arg2, args2) -> Cps.VarMap.add arg1 arg2 args2
  | _, _ -> assert false

let join_allocations a b = Cps.VarMap.union (fun _ value1 value2 -> Some (join_values value1 value2)) a b

let join_blocks (b1: cont_type) (b2: cont_type) =
  match b1, b2 with
  | Cont env1, Cont env2 -> Cont (Cps.VarMap.union (fun _ e1 e2 -> Some (Cps.VarSet.union e1 e2)) env1 env2)
  | Clos (env1, args1), Clos (env2, args2) -> Clos (Cps.VarMap.union (fun _ e1 e2 -> Some (Cps.VarSet.union e1 e2)) env1 env2, List.map2 Cps.VarSet.union args1 args2)
  | Return (arg1, args1), Return (arg2, args2) -> Return (Cps.VarSet.union arg1 arg2, Cps.VarMap.union (fun _ e1 e2 -> Some (Cps.VarSet.union e1 e2)) args1 args2)
  | If_branch (args1, fvs1), If_branch (args2, fvs2) -> If_branch (Cps.VarMap.union (fun _ e1 e2 -> Some (Cps.VarSet.union e1 e2)) args1 args2, Cps.VarMap.union (fun _ e1 e2 -> Some (Cps.VarSet.union e1 e2)) fvs1 fvs2)
  | If_join (arg1, args1), If_join (arg2, args2) -> If_join (Cps.VarSet.union arg1 arg2, Cps.VarMap.union (fun _ e1 e2 -> Some (Cps.VarSet.union e1 e2)) args1 args2)
  | Match_branch (env1, args1, fvs1), Match_branch (env2, args2, fvs2) -> Match_branch (List.map2 Cps.VarSet.union env1 env2, Cps.VarMap.union (fun _ e1 e2 -> Some (Cps.VarSet.union e1 e2)) args1 args2, Cps.VarMap.union (fun _ e1 e2 -> Some (Cps.VarSet.union e1 e2)) fvs1 fvs2)
  | Match_join (arg1, args1), Match_join (arg2, args2) -> Match_join (Cps.VarSet.union arg1 arg2, Cps.VarMap.union (fun _ e1 e2 -> Some (Cps.VarSet.union e1 e2)) args1 args2)
  | _, _ -> assert false

type context = stack_allocs * cont_type

module Context = struct
  type t = context
  let compare : t -> t -> int = Stdlib.compare
end

module ContextMap = Map.Make (Context)

let rec analysis (conts: (int * cont_type * stack_allocs * allocations) list) (reduce: stack_allocs -> stack_allocs) (prog: cont) (map: (allocations ContextMap.t) Cps.PointerMap.t) : (allocations * cont_type) Cps.PointerMap.t =
  match conts with
  | [] -> Cps.PointerMap.map (fun contexts -> List.fold_left (fun (allocs, acc) ((_, new_env), allocations) -> join_allocations allocs allocations, join_blocks acc new_env) (let ((_, new_env), allocations) = List.hd (ContextMap.bindings contexts) in allocations, new_env) (List.tl (ContextMap.bindings contexts))) map
  | (k, block', stack''', allocations) :: conts' -> begin
    Logger.start "k%d %a Stack: %a Allocs: %a\n" k pp_block  block' pp_stack stack''' pp_allocations allocations;
    Logger.stop ();

      let stack = reduce stack''' in

      (* Already seen this block. *)
      if Cps.PointerMap.mem k map then begin
        let old_contexts = Cps.PointerMap.find k map in
        
        (* Already seen this context. *)
        if ContextMap.mem (stack, block') old_contexts then begin
          let old_allocations = ContextMap.find (stack, block') old_contexts in
          let new_allocations = join_allocations old_allocations allocations in
          (* Already seen these allocations. *)
          if Cps.VarMap.equal value_cmp new_allocations old_allocations then begin
            match stack''' with
            | [] -> analysis conts' reduce prog map
            | (k', args) :: _stack' -> analysis ((k', Return (Cps.VarSet.empty, args), _stack', new_allocations) :: conts') reduce prog map
          end else begin
            let block, expr = Cps.PointerMap.find k prog in
            let next_conts = analysis_cont expr stack''' (block_env block block') new_allocations in
            analysis (conts'@next_conts) reduce prog (Cps.PointerMap.add k (ContextMap.add (stack, block') new_allocations old_contexts) map)
          end
        end else begin
          let block, expr = Cps.PointerMap.find k prog in
          let next_conts = analysis_cont expr stack''' (block_env block block') allocations in
          analysis (conts'@next_conts) reduce prog (Cps.PointerMap.add k (ContextMap.add (stack, block') allocations old_contexts) map)
        end
      end else begin
        let block, expr = Cps.PointerMap.find k prog in
        let next_conts = analysis_cont expr stack''' (block_env block block') allocations in
        analysis (conts'@next_conts) reduce prog (Cps.PointerMap.add k (ContextMap.singleton (stack, block') allocations) map)
      end
    end

let start_analysis reduce prog = let args, _ = get_cont prog 0 in analysis [0, Cont (Cps.VarSet.fold (fun v env -> Cps.VarMap.add v Cps.VarSet.empty env) args Cps.VarMap.empty), [], Cps.VarMap.empty] reduce prog (Cps.PointerMap.empty)

let propagation_named (named : Cps.expr) (env: environment) (allocations: allocations): named * value_domain option =
match named with
| Var var' -> Var var', get env var' allocations
| Const x -> Const x, Some (Int_domain (Int_domain.singleton x))
| Add (x1, x2) -> begin match get env x1 allocations, get env x2 allocations with
  | Some (Int_domain d1), Some (Int_domain d2) when Int_domain.is_singleton d1 && Int_domain.is_singleton d2 -> Const ((Int_domain.get_singleton d1) + (Int_domain.get_singleton d2)), Some (Int_domain (Int_domain.singleton ((Int_domain.get_singleton d1) + (Int_domain.get_singleton d2))))
  | Some (Int_domain _), Some (Int_domain _) -> Add (x1, x2), Some (Int_domain (Int_domain.top))
  | Some (Int_domain _), None | None, Some (Int_domain _) | None, None -> Add (x1, x2), Some (Int_domain (Int_domain.top))
  | _ -> assert false
  end
| Sub (x1, x2) -> begin match get env x1 allocations, get env x2 allocations with
  | Some (Int_domain d1), Some (Int_domain d2) when Int_domain.is_singleton d1 && Int_domain.is_singleton d2 -> Const ((Int_domain.get_singleton d1) - (Int_domain.get_singleton d2)), Some (Int_domain (Int_domain.singleton ((Int_domain.get_singleton d1) - (Int_domain.get_singleton d2))))
  | Some (Int_domain _), Some (Int_domain _) -> Sub (x1, x2), Some (Int_domain (Int_domain.top))
  | Some (Int_domain _), None | None, Some (Int_domain _) | None, None -> Sub (x1, x2), Some (Int_domain (Int_domain.top))
  | _ -> assert false
  end
| Print x -> Print x, None
| Tuple vars -> Tuple vars, Some (Tuple_domain (map_args2 vars env))
| Get (var', pos) -> begin
    match get env var' allocations with
    | Some (Tuple_domain values) -> Get (var', pos), join_allocs (List.nth values pos) allocations
    | None -> Get (var', pos), None
    | _ -> assert false
  end
| Closure (k, values) -> Closure (k, values), Some (Closure_domain (Cps.PointerMap.singleton k (map_env values env)))
| Constructor (tag, environment) -> Constructor (tag, environment), Some (Constructor_domain (Cps.PointerMap.singleton tag (map_args2 environment env)))

let rec propagation (cps : Cps.instr) (env: environment) (allocations: allocations): expr =
  match cps with
  | Let (var, named, expr) -> begin
      let named, value = propagation_named named env allocations in
      match value with
      | Some value' -> Let (var, named, propagation expr (Cps.VarMap.add var (Cps.VarSet.singleton var) env) (Cps.VarMap.add var value' allocations))
      | None -> Let (var, named, propagation expr (Cps.VarMap.add var (Cps.VarSet.empty) env) allocations)
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
      | Some (Constructor_domain clos) -> Match_pattern (var, List.filter (fun (n, _, _, _) -> List.exists (fun (n', _) -> n = n') (Cps.PointerMap.bindings clos)) matchs, (kf, argsf), fvs)
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

let propagation_blocks (blocks: Cps.blocks) (map: (allocations * cont_type) Cps.PointerMap.t) =
  Cps.PointerMap.mapi (fun k (block, expr) -> begin
    if Cps.PointerMap.mem k map then
      let allocations, block' = Cps.PointerMap.find k map in
      block, propagation expr (block_env block block') allocations
    else block, expr
  end) blocks

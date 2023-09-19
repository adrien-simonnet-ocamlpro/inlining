type var = Cps.var

type expr = Cps.expr
type pointer = Cps.pointer
type instr = Cps.instr
type block = Cps.block
type blocks = Cps.blocks

type allocations = Cps.VarSet.t
type environment = allocations Cps.VarMap.t
type arguments = allocations list

type abstract_value =
| Int_domain of Int_domain.t
| Tuple_domain of arguments
| Closure_domain of environment Cps.PointerMap.t
| Constructor_domain of arguments Cps.PointerMap.t

type abstract_block =
| Cont of environment
| Clos of environment * arguments
| Return of allocations * environment
| If_branch of environment * environment
| If_join of allocations * environment
| Match_branch of arguments * environment * environment
| Match_join of allocations * environment

type factory = abstract_value Cps.VarMap.t

type abstract_frame = pointer * environment
type abstract_stack = abstract_frame list

type context = abstract_stack * abstract_block

module Context = struct
  type t = context
  let compare : t -> t -> int = Stdlib.compare
end

module ContextMap = Map.Make (Context)

let value_cmp (v1: abstract_value) (v2: abstract_value): bool =
  match v1, v2 with
  | Int_domain d1, Int_domain d2 -> d1 = d2
  | Tuple_domain values1, Tuple_domain values2 -> List.equal (fun value1 value2 -> Cps.VarSet.equal value1 value2) values1 values2
  | Closure_domain clos1, Closure_domain clos2 -> Cps.PointerMap.equal (fun values1 values2 -> Cps.VarMap.equal (fun value1 value2 -> Cps.VarSet.equal value1 value2) values1 values2) clos1 clos2
  | Constructor_domain clos1, Constructor_domain clos2 -> Cps.PointerMap.equal (fun values1 values2 -> List.equal (fun value1 value2 -> Cps.VarSet.equal value1 value2) values1 values2) clos1 clos2
  | _ -> assert false

let pp_alloc (fmt: Format.formatter) (alloc: allocations) =
  match (Cps.VarSet.elements alloc) with
  | [] -> Format.fprintf fmt "∅"
  | [ v ] -> Format.fprintf fmt "{ %d }" v
  | v :: env' -> begin
      Format.fprintf fmt "{ %d"  v;
      List.iter (fun v' -> Format.fprintf fmt ", %d"  v') env';
      Format.fprintf fmt " }"
    end

let pp_args (fmt: Format.formatter) (args: arguments): unit =
  match args with
  | [] -> Format.fprintf fmt "[ ]"
  | [ v ] -> Format.fprintf fmt "[ %a ]" pp_alloc v
  | v :: env' -> begin
      Format.fprintf fmt "[ %a" pp_alloc v;
      List.iter (fun v' -> Format.fprintf fmt "; %a" pp_alloc v') env';
      Format.fprintf fmt " ]"
    end

let pp_env (fmt: Format.formatter) (env: environment): unit =
  match (Cps.VarMap.bindings env) with
  | [] -> Format.fprintf fmt "{ }"
  | [ k, v ] -> Format.fprintf fmt "{ %d: %a }" k pp_alloc v
  | (k, v) :: env' -> begin
      Format.fprintf fmt "{ %d: %a" k pp_alloc v;
      List.iter (fun (k', v') -> Format.fprintf fmt ", %d: %a" k' pp_alloc v') env';
      Format.fprintf fmt " }"
    end
  
let pp_stack (fmt: Format.formatter) (stack: abstract_stack): unit =
  match stack with
  | [] -> Format.fprintf fmt "[ ]"
  | [ k, env ] -> Format.fprintf fmt "[ %d: %a ]" k pp_env env
  | (k, env) :: stack' -> begin
    Format.fprintf fmt "[ %d: %a ]" k pp_env env;
      List.iter (fun (k', env') -> Format.fprintf fmt "[ %d: %a ]" k' pp_env env') stack';
      Format.fprintf fmt " ]"
    end

let pp_value_domain (fmt: Format.formatter) = function
| Int_domain d ->  Int_domain.pp fmt d
| Tuple_domain values -> Format.fprintf fmt "[%a]" pp_args values
| Closure_domain clos -> begin
  match (Cps.VarMap.bindings clos) with
  | [] -> Format.fprintf fmt "{ }"
  | [ k, v ] -> Format.fprintf fmt "{ %d: %a }" k pp_env v
  | (k, v) :: env' -> begin
      Format.fprintf fmt "{ %d: %a" k pp_env v;
      List.iter (fun (k', v') -> Format.fprintf fmt ", %d: %a" k' pp_env v') env';
      Format.fprintf fmt " }"
    end
  end
| Constructor_domain clos -> begin
    match (Cps.VarMap.bindings clos) with
    | [] -> Format.fprintf fmt "{ }"
    | [ k, v ] -> Format.fprintf fmt "{ %d: %a }" k pp_args v
    | (k, v) :: env' -> begin
        Format.fprintf fmt "{ %d: %a" k pp_args v;
        List.iter (fun (k', v') -> Format.fprintf fmt ", %d: %a" k' pp_args v') env';
        Format.fprintf fmt " }"
      end
  end
        

let pp_factory (fmt: Format.formatter) (factory: factory): unit =
  Cps.VarMap.iter (fun i v -> Format.fprintf fmt "%d: %a\n" i pp_value_domain v) factory

let pp_block (fmt: Format.formatter) block =
  match block with
  | Cont args -> Format.fprintf fmt "Cont %a" pp_env args
  | Clos (env, args) -> Format.fprintf fmt "Closure %a %a" pp_env env pp_args args
  | Return (arg, args) -> Format.fprintf fmt "Return %a %a" pp_alloc arg pp_env args
  | If_branch (args, fvs) -> Format.fprintf fmt "If_branch %a %a" pp_env args pp_env fvs
  | If_join (arg, args) -> Format.fprintf fmt "If_join %a %a" pp_alloc arg pp_env args
  | Match_branch (env, args, fvs) -> Format.fprintf fmt "Match_branch %a %a %a" pp_args env pp_env args pp_env fvs
  | Match_join (arg, args) -> Format.fprintf fmt "Match_join %a %a" pp_alloc arg pp_env args

let pp_analysis (fmt: Format.formatter) (map: (factory * abstract_block) Cps.PointerMap.t) =
  Format.fprintf fmt "Analysis:\n\n";
  Cps.PointerMap.iter (fun k (factory, block) -> Format.fprintf fmt "k%d %a:\n%a\n\n" k pp_block block pp_factory factory) map

let map_values args values =
  List.fold_left2 (fun env arg1 arg2 -> Cps.VarMap.add arg1 arg2 env) Cps.VarMap.empty args values

let join_args (old_env: arguments) (new_env: arguments): arguments =
  List.map2 Cps.VarSet.union old_env new_env

let join_env (old_env: environment) (new_env: environment): environment =
  Cps.VarMap.union (fun _ e1 e2 -> Some (Cps.VarSet.union e1 e2)) old_env new_env

let join_values v1 v2 = match v1, v2 with
| Int_domain d1, Int_domain d2 -> Int_domain (Int_domain.join d1 d2)
| Tuple_domain values1, Tuple_domain values2 -> Tuple_domain (List.map2 Cps.VarSet.union values1 values2)
| Closure_domain clos1, Closure_domain clos2 -> Closure_domain (Cps.PointerMap.union (fun _ env1 env2 -> Some (join_env env1 env2)) clos1 clos2)
| Constructor_domain clos1, Constructor_domain clos2 -> Constructor_domain (Cps.PointerMap.union (fun _ env1 env2 -> Some (join_args env1 env2)) clos1 clos2)
| _ -> assert false



type value_type =
| Bottom
| Value of abstract_value
| Top

let join_abstract_values (v1: value_type) (v2: value_type): value_type =
  match v1, v2 with
  | Bottom, v | v, Bottom -> v
  | Value v1', Value v2' -> Value (join_values v1' v2')
  | Top, Value (Int_domain _) | Value (Int_domain _), Top -> Value (Int_domain Int_domain.top)
  | Top, _ | _, Top -> Top

let get_value (var: var) (factory: factory): value_type =
  match Cps.VarMap.find_opt var factory with
  | None -> Top
  | Some v -> Value v

let get_abstract_value (allocs: allocations) (factory: factory): value_type =
  match Cps.VarSet.elements allocs with
  | [] -> Bottom
  | [ a ] -> get_value a factory
  | a :: allocs' -> List.fold_left (fun old cur -> join_abstract_values old (get_value cur factory)) (get_value a factory) allocs'
  
let get2 (env: environment) (value: var) (factory: factory): value_type =
  let allocs = Cps.VarMap.find value env in
  get_abstract_value allocs factory
  



let rec join_value_list (values: abstract_value list): abstract_value =
  match values with
  | [] -> assert false
  | [value] -> value
  | value :: values' -> join_values value (join_value_list values')



let join_allocs (allocs: allocations) (factory: factory): abstract_value option =
  match Cps.VarSet.elements allocs with
  | [] -> None
  | [ value ] -> if Cps.VarMap.mem value factory then Some (Cps.VarMap.find value factory) else assert false
  | value :: values' -> Some (List.fold_left (fun old cur -> if Cps.VarMap.mem cur factory then join_values old (Cps.VarMap.find cur factory) else assert false) (if Cps.VarMap.mem value factory then Cps.VarMap.find value factory else assert false) values')

(*
  if Cps.VarSet.is_empty allocs
  then None
  else begin
    let values = Cps.VarSet.elements allocs in
    let values_domain = List.map (fun alloc -> if Cps.VarMap.mem alloc factory then Cps.VarMap.find alloc factory else assert false) values in
    Some (join_value_list values_domain)
  end*)


let get (env: environment) (value: var) (factory: factory): abstract_value option =
  let allocs = Cps.VarMap.find value env in
  join_allocs allocs factory

let map_args (args: var list) (env: environment) = List.map (fun arg -> Cps.VarMap.find arg env) args
let map_env (allocs: allocations) (env: environment): environment =
  Cps.VarMap.filter (fun p _ -> Cps.VarSet.mem p allocs) env

let analysis_expr (expr : expr) (env: environment) (factory: factory): value_type =
  match expr with
  | Var var' -> get2 env var' factory
  | Const x -> Value (Int_domain (Int_domain.singleton x))
  | Add (x1, x2) -> begin
      match get2 env x1 factory, get2 env x2 factory with
      | Value (Int_domain d1), Value (Int_domain d2) when Int_domain.is_singleton d1 && Int_domain.is_singleton d2 -> Value (Int_domain (Int_domain.singleton ((Int_domain.get_singleton d1) + (Int_domain.get_singleton d2))))
      | Value (Int_domain _), Value (Int_domain _) -> Value (Int_domain (Int_domain.top))
      | Value (Int_domain _), Bottom | Bottom, Value (Int_domain _) | Bottom, Bottom -> Value (Int_domain (Int_domain.top))
      | Top, _ | _, Top -> Value (Int_domain (Int_domain.top))
      | _ -> assert false
    end
  | Sub (x1, x2) -> begin
      match get2 env x1 factory, get2 env x2 factory with
      | Value (Int_domain d1), Value (Int_domain d2) when Int_domain.is_singleton d1 && Int_domain.is_singleton d2 -> Value (Int_domain (Int_domain.singleton ((Int_domain.get_singleton d1) - (Int_domain.get_singleton d2))))
      | Value (Int_domain _), Value (Int_domain _) -> Value (Int_domain (Int_domain.top))
      | Value (Int_domain _), Bottom | Bottom, Value (Int_domain _) | Bottom, Bottom -> Value (Int_domain (Int_domain.top))
      | Top, _ | _, Top -> Value (Int_domain (Int_domain.top))
      | _ -> assert false
    end
  | Tuple vars -> Value (Tuple_domain (map_args vars env))
  | Get (var', pos) -> begin
      match get2 env var' factory with
      | Bottom -> Bottom
      | Value (Tuple_domain values) -> get_abstract_value (List.nth values pos) factory
      | Top -> Top
      | Value _ -> assert false
    end
  | Closure (k, values) -> Value (Closure_domain (Cps.PointerMap.singleton k (map_env values env)))
  | Constructor (tag, environment) -> Value (Constructor_domain (Cps.PointerMap.singleton tag (map_args environment env)))


type abstract_jump =
| Jmp of (pointer * abstract_block) list
| Calll of (pointer * abstract_block * abstract_frame) list
| Ret of allocations

let rec analysis_instr (instr: instr) (env: environment) (factory: factory): abstract_jump * factory =
  match instr with
  | Let (var, Var var', instr) -> begin
      analysis_instr instr (Cps.VarMap.add var (Cps.VarMap.find var' env) env) factory
    end
  | Let (var, expr, instr) -> begin
      let value = analysis_expr expr env factory in
      let env, allocs = (match value with
      | Value value' -> (Cps.VarMap.add var (Cps.VarSet.singleton var) env), (Cps.VarMap.update var (fun value'' -> begin
          if Option.is_some value''
            then Some (join_values (Option.get value'') value')
            else Some value'
          end) factory)
      | Bottom | Top -> (Cps.VarMap.add var (Cps.VarSet.empty) env), factory) in
      analysis_instr instr env allocs
    end
  | Apply_block (k', args) -> Jmp [k', Cont (map_env args env)], factory
  | If (var, (kt, argst), (kf, argsf), fvs) -> begin
      match get env var factory with
      | Some (Int_domain i) when Int_domain.is_singleton i -> begin
        match Int_domain.get_singleton i with
        | 0 -> Jmp [kf, If_branch (map_env argsf env, map_env fvs env)], factory
        | _ -> Jmp [kt, If_branch (map_env argst env, map_env fvs env)], factory
        end
      | Some (Int_domain _) | None -> Jmp ([ (kf, If_branch (map_env argsf env, map_env fvs env)) ; (kt, If_branch (map_env argst env, map_env fvs env)) ]), factory
      | _ -> assert false
    end
  | Match_pattern (var, matchs, (kf, argsf), fvs) -> begin
      match get env var factory with
      | Some (Constructor_domain clos) -> Jmp (List.map (fun (n, env') -> begin
          match List.find_opt (fun (n', _, _, _) -> n = n') matchs with
          | Some (_, _, k, args) -> k, Match_branch (env', map_env args env, map_env fvs env)
          | None -> kf, Match_branch ([], map_env argsf env, map_env fvs env)
          end) (Cps.PointerMap.bindings clos)), factory
      | None -> Jmp ((kf, Match_branch ([], map_env argsf env, map_env fvs env)) :: (List.map (fun (_, pld, kt, argst) -> kt, Match_branch (List.map (fun _ -> Cps.VarSet.empty) pld, map_env argst env, map_env fvs env)) matchs)), factory
      | _ -> assert false
    end
  | Return x -> Ret (Cps.VarMap.find x env), factory
  | If_return (k, arg, args) -> Jmp [k, If_join (Cps.VarMap.find arg env, map_env args env)], factory
  | Match_return (k, arg, args) -> Jmp [k, Match_join (Cps.VarMap.find arg env, map_env args env)], factory
  | Call (x, args, (k', args')) -> begin
      match get env x factory with
      | Some (Closure_domain clos) -> Calll (List.map (fun (k, env') -> k, Clos (env', map_args args env), ((k', map_env args' env))) (Cps.PointerMap.bindings clos)), factory
      | _ -> assert false
    end
  | Call_direct (k, x, args, (k', args')) -> begin
      match get env x factory with
      | Some (Closure_domain clos) -> assert (Cps.PointerMap.cardinal clos = 1); Calll (List.map (fun (_, env') -> k, Clos (env', map_args args env), ((k', map_env args' env))) (Cps.PointerMap.bindings clos)), factory
      | _ -> assert false
    end

let block_env (block1: Cps.block) (block2: abstract_block): environment =
  match block1, block2 with
  | Cont _, Cont args2 -> args2
  | Clos (_, args1), Clos (env2, args2) -> Cps.VarMap.union (fun _ a _-> Some a) (map_values args1 args2) env2
  | Return (arg1, _), Return (arg2, args2) -> Cps.VarMap.add arg1 arg2 args2
  | If_branch (_, _), If_branch (args2, fvs2) -> Cps.VarMap.union (fun _ _ b-> Some b) fvs2 args2
  | If_join (arg1, _), If_join (arg2, args2) -> Cps.VarMap.add arg1 arg2 args2
  | Match_branch (env1, _, _), Match_branch (env2, args2, fvs2) -> Cps.VarMap.union (fun _ _ b-> Some b) (map_values env1 env2) (Cps.VarMap.union (fun _ _ b-> Some b) args2 fvs2)
  | Match_join (arg1, _), Match_join (arg2, args2) -> Cps.VarMap.add arg1 arg2 args2
  | _, _ -> assert false

let join_factory a b = Cps.VarMap.union (fun _ value1 value2 -> Some (join_values value1 value2)) a b

let join_blocks (b1: abstract_block) (b2: abstract_block) =
  match b1, b2 with
  | Cont env1, Cont env2 -> Cont (Cps.VarMap.union (fun _ e1 e2 -> Some (Cps.VarSet.union e1 e2)) env1 env2)
  | Clos (env1, args1), Clos (env2, args2) -> Clos (Cps.VarMap.union (fun _ e1 e2 -> Some (Cps.VarSet.union e1 e2)) env1 env2, List.map2 Cps.VarSet.union args1 args2)
  | Return (arg1, args1), Return (arg2, args2) -> Return (Cps.VarSet.union arg1 arg2, Cps.VarMap.union (fun _ e1 e2 -> Some (Cps.VarSet.union e1 e2)) args1 args2)
  | If_branch (args1, fvs1), If_branch (args2, fvs2) -> If_branch (Cps.VarMap.union (fun _ e1 e2 -> Some (Cps.VarSet.union e1 e2)) args1 args2, Cps.VarMap.union (fun _ e1 e2 -> Some (Cps.VarSet.union e1 e2)) fvs1 fvs2)
  | If_join (arg1, args1), If_join (arg2, args2) -> If_join (Cps.VarSet.union arg1 arg2, Cps.VarMap.union (fun _ e1 e2 -> Some (Cps.VarSet.union e1 e2)) args1 args2)
  | Match_branch (env1, args1, fvs1), Match_branch (env2, args2, fvs2) -> Match_branch (List.map2 Cps.VarSet.union env1 env2, Cps.VarMap.union (fun _ e1 e2 -> Some (Cps.VarSet.union e1 e2)) args1 args2, Cps.VarMap.union (fun _ e1 e2 -> Some (Cps.VarSet.union e1 e2)) fvs1 fvs2)
  | Match_join (arg1, args1), Match_join (arg2, args2) -> Match_join (Cps.VarSet.union arg1 arg2, Cps.VarMap.union (fun _ e1 e2 -> Some (Cps.VarSet.union e1 e2)) args1 args2)
  | _, _ -> assert false

let join_stacks (map: (factory ContextMap.t) Cps.PointerMap.t): (factory * abstract_block) Cps.PointerMap.t =
  Cps.PointerMap.map (fun contexts -> List.fold_left (fun (allocs, acc) ((_, new_env), factory) -> join_factory allocs factory, join_blocks acc new_env) (let ((_, new_env), factory) = List.hd (ContextMap.bindings contexts) in factory, new_env) (List.tl (ContextMap.bindings contexts))) map

let rec analysis (conts: (int * abstract_block * abstract_stack * factory) list) (reduce: abstract_stack -> abstract_stack) (blocks: blocks) (map: (factory ContextMap.t) Cps.PointerMap.t): (factory ContextMap.t) Cps.PointerMap.t =
  match conts with
  | [] -> map
  | (k, block', stack''', factory) :: conts' -> begin
    (*Logger.start "k%d %a Stack: %a Allocs: %a\n" k pp_block  block' pp_stack stack''' pp_factory factory;*)
    Logger.stop ();

      let stack = reduce stack''' in
      if List.length stack <> List.length stack''' then  Format.printf "%a -> %a@." pp_stack stack''' pp_stack stack;
      
      (* Already seen this block. *)
      if Cps.PointerMap.mem k map then begin
        let old_contexts = Cps.PointerMap.find k map in
        
        (* Already seen this context. *)
        if ContextMap.mem (stack, block') old_contexts then begin
          let old_factory = ContextMap.find (stack, block') old_contexts in
          let new_factory = join_factory old_factory factory in

          (* Already seen these factory. *)
          if Cps.VarMap.equal value_cmp new_factory old_factory then begin
            match stack''' with
            | [] -> analysis conts' reduce blocks map
            | (k', args) :: _stack' -> analysis ((k', Return (Cps.VarSet.empty, args), _stack', new_factory) :: conts') reduce blocks map
          end else begin
            let block, instr = Cps.PointerMap.find k blocks in
            let next_conts = begin
              match analysis_instr instr (block_env block block') new_factory with
              | Jmp l, factory -> List.map (fun (k, block') -> k, block', stack''', factory) l
              | Calll l, factory -> List.map (fun (k, block', frame) -> k, block', frame :: stack''', factory) l
              | Ret allocations, factory -> begin
                  match stack''' with
                  | [] -> []
                  | (k, args) :: stack' -> [k, Return (allocations, args), stack', factory]
                end
              end in
            analysis (conts'@next_conts) reduce blocks (Cps.PointerMap.add k (ContextMap.add (stack, block') new_factory old_contexts) map)
          end
        end else begin
          let block, instr = Cps.PointerMap.find k blocks in
          let next_conts = begin
            match analysis_instr instr (block_env block block') factory with
            | Jmp l, factory -> List.map (fun (k, block') -> k, block', stack''', factory) l
            | Calll l, factory -> List.map (fun (k, block', frame) -> k, block', frame :: stack''', factory) l
            | Ret allocations, factory -> begin
                match stack''' with
                | [] -> []
                | (k, args) :: stack' -> [k, Return (allocations, args), stack', factory]
              end
            end in
          analysis (conts'@next_conts) reduce blocks (Cps.PointerMap.add k (ContextMap.add (stack, block') factory old_contexts) map)
        end
      end else begin
        let block, instr = Cps.PointerMap.find k blocks in
        let next_conts = begin
          match analysis_instr instr (block_env block block') factory with
          | Jmp l, factory -> List.map (fun (k, block') -> k, block', stack''', factory) l
          | Calll l, factory -> List.map (fun (k, block', frame) -> k, block', frame :: stack''', factory) l
          | Ret allocations, factory -> begin
              match stack''' with
              | [] -> []
              | (k, args) :: stack' -> [k, Return (allocations, args), stack', factory]
            end
          end in
        analysis (conts'@next_conts) reduce blocks (Cps.PointerMap.add k (ContextMap.singleton (stack, block') factory) map)
      end
    end

let start_analysis (reduce: abstract_stack -> abstract_stack) (blocks: blocks) =
  let args = begin
    match Cps.PointerMap.find_opt 0 blocks with
    | Some (Cont args, _) -> args
    | _ -> assert false
  end in
  analysis [0, Cont (Cps.VarSet.fold (fun v env -> Cps.VarMap.add v (Cps.VarSet.empty) env) args Cps.VarMap.empty), [], Cps.VarMap.empty] reduce blocks (Cps.PointerMap.empty)

let propagation_named (expr: expr) (env: environment) (factory: factory): expr * abstract_value option =
  match expr with
  | Var var' -> Var var', get env var' factory
  | Const x -> Const x, Some (Int_domain (Int_domain.singleton x))
  | Add (x1, x2) -> begin match get env x1 factory, get env x2 factory with
    | Some (Int_domain d1), Some (Int_domain d2) when Int_domain.is_singleton d1 && Int_domain.is_singleton d2 -> Const ((Int_domain.get_singleton d1) + (Int_domain.get_singleton d2)), Some (Int_domain (Int_domain.singleton ((Int_domain.get_singleton d1) + (Int_domain.get_singleton d2))))
    | Some (Int_domain _), Some (Int_domain _) -> Add (x1, x2), Some (Int_domain (Int_domain.top))
    | Some (Int_domain _), None | None, Some (Int_domain _) | None, None -> Add (x1, x2), Some (Int_domain (Int_domain.top))
    | _ -> assert false
    end
  | Sub (x1, x2) -> begin match get env x1 factory, get env x2 factory with
    | Some (Int_domain d1), Some (Int_domain d2) when Int_domain.is_singleton d1 && Int_domain.is_singleton d2 -> Const ((Int_domain.get_singleton d1) - (Int_domain.get_singleton d2)), Some (Int_domain (Int_domain.singleton ((Int_domain.get_singleton d1) - (Int_domain.get_singleton d2))))
    | Some (Int_domain _), Some (Int_domain _) -> Sub (x1, x2), Some (Int_domain (Int_domain.top))
    | Some (Int_domain _), None | None, Some (Int_domain _) | None, None -> Sub (x1, x2), Some (Int_domain (Int_domain.top))
    | _ -> assert false
    end
  | Tuple vars -> Tuple vars, Some (Tuple_domain (map_args vars env))
  | Get (var', pos) -> begin
      match get env var' factory with
      | Some (Tuple_domain values) -> Get (var', pos), join_allocs (List.nth values pos) factory
      | None -> Get (var', pos), None
      | _ -> assert false
    end
  | Closure (k, values) -> Closure (k, values), Some (Closure_domain (Cps.PointerMap.singleton k (map_env values env)))
  | Constructor (tag, environment) -> Constructor (tag, environment), Some (Constructor_domain (Cps.PointerMap.singleton tag (map_args environment env)))

let rec propagation (instr : Cps.instr) (env: environment) (factory: factory): instr =
  match instr with
  | Let (var, expr, instr) -> begin
      let expr, value = propagation_named expr env factory in
      match value with
      | Some value' -> Let (var, expr, propagation instr (Cps.VarMap.add var (Cps.VarSet.singleton var) env) (Cps.VarMap.add var value' factory))
      | None -> Let (var, expr, propagation instr (Cps.VarMap.add var (Cps.VarSet.empty) env) factory)
    end
  | Apply_block (k', args) -> Apply_block (k', args)
  | If (var, (kt, argst), (kf, argsf), fvs) -> If (var, (kt, argst), (kf, argsf), fvs) (*begin
      match get env var factory with
      | Some (Int_domain i) when Int_domain.is_singleton i -> begin
        match List.find_opt (fun (n', _, _) -> Int_domain.get_singleton i = n') matchs with
        | Some (_, kt, argst) -> If (var, [], (kt, argst), fvs)
        | None -> If (var, [], (kf, argsf), fvs)
        end
      | Some (Int_domain _) | None -> If (var, matchs, (kf, argsf), fvs)
      | _ -> assert false
    end*)
  | Match_pattern (var, matchs, (kf, argsf), fvs) -> begin
      match get env var factory with
      | Some (Constructor_domain clos) -> Match_pattern (var, List.filter (fun (n, _, _, _) -> List.exists (fun (n', _) -> n = n') (Cps.PointerMap.bindings clos)) matchs, (kf, argsf), fvs)
      | None -> Match_pattern (var, matchs, (kf, argsf), fvs)
      | _ -> assert false
    end
  | Return x -> Return x
  | If_return (k, arg, args) -> If_return (k, arg, args)
  | Match_return (k, arg, args) -> Match_return (k, arg, args)
  | Call (x, args, stack) -> begin
      match get env x factory with
      | Some (Closure_domain clos) when Cps.PointerMap.cardinal clos = 1 -> let (k, _) = Cps.PointerMap.choose clos in Call_direct (k, x, args, stack)
      | Some _ | None -> Call (x, args, stack)
    end
  | Call_direct (k, x, args, stack) -> Call_direct (k, x, args, stack)

let propagation_blocks (blocks: Cps.blocks) (map: (factory * abstract_block) Cps.PointerMap.t) =
  Cps.PointerMap.mapi (fun k (block, instr) -> begin
    if Cps.PointerMap.mem k map then
      let factory, block' = Cps.PointerMap.find k map in
      block, propagation instr (block_env block block') factory
    else block, instr
  end) blocks

let inc (vars: var Seq.t): var * var Seq.t =
  match Seq.uncons vars with
  | None -> assert false
  | Some (i, vars') -> i, vars'

let fvs_to_list fvs = Cps.VarSet.elements fvs

let expr_to_asm (var: var) (expr: expr) (asm: Asm.instr) (factory: factory) (vars: var Seq.t): Asm.instr * var Seq.t =
  match expr with
  | Const x -> Let (var, Const x, asm), vars
  | Add (x1, x2) -> begin
      match get_value var factory with
      | Value (Int_domain d) when Int_domain.is_singleton d -> Let (var, Const (Int_domain.get_singleton d), asm), vars
      | _ -> Let (var, Add (x1, x2), asm), vars
    end
  | Sub (x1, x2) -> begin
      match get_value var factory with
      | Value (Int_domain d) when Int_domain.is_singleton d -> Let (var, Const (Int_domain.get_singleton d), asm), vars
      | _ -> Let (var, Sub (x1, x2), asm), vars
    end
  | Var x -> Let (var, Var x, asm), vars
  | Tuple args -> Let (var, Tuple args, asm), vars
  | Get (record, pos) -> Let (var, Get (record, pos), asm), vars
  | Closure (k, env) -> begin
      let k_id, vars = inc vars in
      let env_id, vars = inc vars in
      Let (k_id, Pointer k, Let (env_id, Tuple (fvs_to_list env), Let (var, Tuple [k_id; env_id], asm))), vars
    end
  | Constructor (tag, env) -> begin
      let tag_id, vars = inc vars in
      let env_id, vars = inc vars in
      Let (tag_id, Const tag, Let (env_id, Tuple env, Let (var, Tuple [tag_id; env_id], asm))), vars
    end

let rec instr_to_asm (block: instr) (env: environment) (factory: factory) (vars: var Seq.t) (pointers: Asm.pointer Seq.t): Asm.instr * Asm.var Seq.t * Asm.pointer Seq.t * Asm.blocks =
  match block with
  | Let (var, expr, instr) -> begin
      let asm, vars, pointers, blocks = instr_to_asm instr env factory vars pointers in
      let asm, vars = expr_to_asm var expr asm factory vars in
      asm, vars, pointers, blocks
    end
  | Apply_block (k, args) -> Apply_direct (k, fvs_to_list args, []), vars, pointers, Asm.PointerMap.empty
  | If (var, (kt, argst), (kf, argsf), fvs) -> If (var, [ 0, kf, (fvs_to_list argsf) @ (fvs_to_list fvs) ], (kt, (fvs_to_list argst) @ (fvs_to_list fvs)), []), vars, pointers, Asm.PointerMap.empty
  | Match_pattern (cons, matchs, (kf, argsf), fvs) -> begin
      let tag_id, vars = inc vars in
      let payload_id, vars = inc vars in
      Asm.Let (tag_id, Get (cons, 0), (Asm.Let (payload_id, Get (cons, 1), If (tag_id, List.map (fun (n, _, k, args) -> (n, k, payload_id :: (fvs_to_list args) @ (fvs_to_list fvs))) matchs, (kf, payload_id :: (fvs_to_list argsf) @ (fvs_to_list fvs)), [])))), vars, pointers, Asm.PointerMap.empty
    end
  | Return var -> Return var, vars, pointers, Asm.PointerMap.empty
  | If_return (k, arg, args) -> Apply_direct (k, arg :: (fvs_to_list args), []), vars, pointers, Asm.PointerMap.empty
  | Match_return (k, arg, args) -> Apply_direct (k, arg :: (fvs_to_list args), []), vars, pointers, Asm.PointerMap.empty
  | Call (clos, args, (p, args')) -> begin
      let k_id, vars = inc vars in
      let env_id, vars = inc vars in
      Let (k_id, Get (clos, 0), Let (env_id, Get (clos, 1), Apply_indirect (k_id, env_id :: args, [p, fvs_to_list args']))), vars, pointers, Asm.PointerMap.empty
    end
  | Call_direct (k, clos, args, (p, args')) -> begin
      let env_id, vars = inc vars in
      Let (env_id, Get (clos, 1), Apply_direct (k, env_id :: args, [p, fvs_to_list args'])), vars, pointers, Asm.PointerMap.empty
    end

let block_to_asm (block: block) (asm1: Asm.instr) (vars: Asm.var Seq.t) (pointers: Asm.pointer Seq.t): Asm.block * var Seq.t * Asm.pointer Seq.t * Asm.blocks =
  match block with
  | Cont (args') -> (fvs_to_list args', asm1), vars, pointers, Asm.PointerMap.empty
  | Return (arg, args') -> (arg :: fvs_to_list args', asm1), vars, pointers, Asm.PointerMap.empty
  | Clos (body_free_variables, args') -> begin
      let function_id, pointers = inc pointers in
      let environment_id, vars = inc vars in
      let body = List.fold_left (fun block' (pos, body_free_variable) -> Asm.Let (body_free_variable, Asm.Get (environment_id, pos), block')) (Apply_direct (function_id, args' @ fvs_to_list body_free_variables, [])) (List.mapi (fun i fv -> i, fv) (fvs_to_list body_free_variables)) in
      (environment_id :: args', body), vars, pointers, Asm.PointerMap.singleton function_id (args' @ fvs_to_list body_free_variables, asm1)
    end
  | If_branch (args, fvs) -> (fvs_to_list args @ fvs_to_list fvs, asm1), vars, pointers, Asm.PointerMap.empty
  | If_join (arg, args) -> (arg :: fvs_to_list args, asm1), vars, pointers, Asm.PointerMap.empty
  | Match_branch (body_free_variables, args', fvs) -> begin
      let environment_id, vars = inc vars in
      let body = List.fold_left (fun block' (pos, body_free_variable) -> Asm.Let (body_free_variable, Asm.Get (environment_id, pos), block')) asm1 (List.mapi (fun i fv -> i, fv) body_free_variables) in
      (environment_id :: fvs_to_list args' @ fvs_to_list fvs, body), vars, pointers, Asm.PointerMap.empty
    end
  | Match_join (arg, args) -> (arg :: fvs_to_list args, asm1), vars, pointers, Asm.PointerMap.empty

let blocks_to_asm (blocks: blocks) (env: environment) (factory: factory) (vars: Asm.var Seq.t) (pointers: Asm.pointer Seq.t): Asm.blocks * Asm.var Seq.t * Asm.pointer Seq.t =
  Cps.PointerMap.fold (fun k (block, instr) (blocks, vars, pointers) -> begin
    let asm, vars, pointers, blocks' = instr_to_asm instr env factory vars pointers in  
    let block, vars, pointers, blocks'' = block_to_asm block asm vars pointers in
    Asm.PointerMap.add k block (Asm.PointerMap.union (fun _ -> assert false) blocks (Asm.PointerMap.union (fun _ -> assert false) blocks' blocks'')), vars, pointers
  end) blocks (Asm.PointerMap.empty, vars, pointers)

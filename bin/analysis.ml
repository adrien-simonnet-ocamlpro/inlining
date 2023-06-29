module Analysis = Map.Make (Int)

module Allocations = Map.Make (Int)

module Closures = Map.Make (Int)

module Values = Set.Make (Int)

type value_domain =
  | Int_domain of Int_domain.t
  | Tuple_domain of Values.t list
  | Pointer_domain of Pointer_domain.t
  | Closure_domain of (Values.t list) Closures.t

let value_cmp v1 v2 =
  match v1, v2 with
  | Pointer_domain p1, Pointer_domain p2 -> Values.equal p1 p2
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
type cont = Cps.conts

let rec has_cont (cont: cont) k =
  match cont with
  | Cont (k', _, _) :: _ when k = k' -> true
  | Cont (_, _, _) :: e2 | Clos (_, _, _, _) :: e2 | Return (_, _, _, _) :: e2 -> has_cont e2 k
  | [] -> false

let rec get_cont (cont: cont) k =
match cont with
| Cont (k', args, e1) :: _ when k = k' -> args, e1
| Cont (_, _, _) :: e2 | Clos (_, _, _, _) :: e2 | Return (_, _, _, _) :: e2 -> get_cont e2 k
| [] -> failwith "cont not found"

let rec has_clos (cont: cont) k =
  match cont with
  | Clos (k', _, _, _) :: _ when k = k' -> true
  | Clos (_, _, _, _) :: e2 | Cont (_, _, _) :: e2 | Return (_, _, _, _) :: e2 -> has_clos e2 k
  | [] -> false

let rec get_clos (cont: cont) k =
  match cont with
  | Clos (k', env, args, e1) :: _ when k = k' -> env, args, e1
  | Clos (_, _, _, _) :: e2 | Cont (_, _, _) :: e2 | Return (_, _, _, _) :: e2 -> get_clos e2 k
  | [] -> failwith "clos not found"

let rec get_return (cont: cont) k =
  match cont with
  | Return (k', arg, args, e1) :: _ when k = k' -> arg, args, e1
  | Clos (_, _, _, _) :: e2 | Cont (_, _, _) :: e2 | Return (_, _, _, _) :: e2 -> get_return e2 k
  | [] -> failwith "return not found"

let pp_alloc fmt (alloc: Values.t) = Values.iter (fun i -> Format.fprintf fmt "%d " i) alloc

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

let pp_frame fmt (k, env) = Format.fprintf fmt "(%d: %a)" k (pp_env "") env
  
let pp_stack fmt stack =
  Printf.printf "[";
  List.iter (fun frame -> pp_frame fmt frame) stack;
  Printf.printf "]\n"

let pp_allocations fmt (allocations: value_domain Allocations.t) = Allocations.iter (fun i v -> Format.fprintf fmt "%d: %a\n" i pp_value_domain v) allocations

let pp_analysis fmt (map: (value_domain Allocations.t * Values.t list) Analysis.t) = Format.fprintf fmt "Analysis:\n\n"; Analysis.iter (fun k (allocations, env) -> Format.fprintf fmt "k%d %a:\n%a\n\n" k (pp_env "") env pp_allocations allocations) map

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

type cont_type =
| Cont of int
| Clos of int * Values.t list
| Return of int * Values.t

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


let rec analysis_cont (cps: expr) (stack: ((pointer * Values.t list) list)) (env: (address * Values.t) list) (allocations: value_domain Allocations.t): (cont_type * Values.t list * ((pointer * Values.t list) list) * value_domain Allocations.t) list =
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
  | Apply_cont (k', args) -> [Cont k', map_args2 args env, stack, allocations]
  | If (var, matchs, (kf, argsf)) -> begin
      match get env var allocations with
      | Some (Int_domain i) when Int_domain.is_singleton i -> begin
        match List.find_opt (fun (n', _, _) -> Int_domain.get_singleton i = n') matchs with
        | Some (_, kt, argst) -> [Cont kt, map_args2 argst env, stack, allocations]
        | None -> [Cont kf, map_args2 argsf env, stack, allocations]
        end
      | Some (Int_domain _) | None -> (Cont kf, map_args2 argsf env, stack, allocations)::(List.map (fun (_, kt, argst) -> Cont kt, map_args2 argst env, stack, allocations) matchs)
      | _ -> assert false
    end
  | Match_pattern (var, matchs, (kf, argsf)) -> begin
      match get env var allocations with
      | Some (Closure_domain clos) -> List.map (fun (n, env') -> begin
          match List.find_opt (fun (n', _, _) -> n = n') matchs with
          | Some (_, k, args) -> Clos (k, env'), map_args2 args env, stack, allocations
          | None -> Cont kf, map_args2 argsf env, stack, allocations
          end) (Closures.bindings clos)
      | None -> (Cont kf, map_args2 argsf env, stack, allocations)::(List.map (fun (_, kt, argst) -> Cont kt, map_args2 argst env, stack, allocations) matchs)
      | _ -> assert false
    end
  | Return x -> begin
      match stack with
      | [] -> []
      | (k, args)::stack' -> [Return (k, Env.get2 env x), args, stack', allocations]
    end
  | Call (x, args, frame) -> begin
      match get env x allocations with
      | Some (Closure_domain clos) -> List.map (fun (k, env') -> Clos (k, env'), map_args2 args env, (join_stack stack (map_stack2 frame env :: stack)), allocations) (Closures.bindings clos)
      | _ -> assert false
    end




let has3 context var env = List.exists (fun ((var', _), env') -> var = var' && env = env') context
let get3 context var env =
  match List.find_opt (fun ((var', _'), env') -> var = var' && env = env') context with
  | Some ((_, allocations), _) -> allocations
  | None -> assert false
;;

let join_allocations a b = Allocations.union (fun _ value1 value2 -> Some (join_values value1 value2)) a b

let rec analysis (conts: (cont_type * Values.t list * ((pointer * Values.t list) list) * value_domain Allocations.t) list) (prog: cont) (map: ((((address * Values.t list) list * value_domain Allocations.t) * Values.t list) list) Analysis.t) : (value_domain Allocations.t * Values.t list) Analysis.t =
  match conts with
  | [] -> Analysis.map (fun contexts -> List.fold_left (fun (allocs, acc) ((_, allocations), new_env) -> join_allocations allocs allocations,  List.map2 Values.union (if acc = [] then new_env else acc) new_env) (Allocations.empty, []) contexts) map
  | (Cont k, env, stack, allocations)::conts' -> begin
    Format.fprintf Format.std_formatter "/// Cont: %d Env: %a Stack: %a Allocs: %a\n" k (pp_env "")  env pp_stack stack pp_allocations allocations;

      if Analysis.mem k map then begin
        let old_context = Analysis.find k map in
        if has3 old_context stack env then begin
          let old_allocations = get3 old_context stack env in
          let new_allocations = join_allocations old_allocations allocations in
          if Allocations.equal value_cmp new_allocations old_allocations then begin
            analysis conts' prog map
          end else begin
            let args, cont = get_cont prog k in
            let next_conts = analysis_cont cont stack (map_values args env) new_allocations in
            analysis (conts'@next_conts) prog (Analysis.add k (((stack, new_allocations),env)::old_context) map)
          end
        end else begin
          let args, cont = get_cont prog k in
          let next_conts = analysis_cont cont stack (map_values args env) allocations in
          analysis (conts'@next_conts) prog (Analysis.add k (((stack, allocations),env)::old_context) map)
        end
      end else begin
        let args, cont = get_cont prog k in
        let next_conts = analysis_cont cont stack (map_values args env) allocations in
        analysis (conts'@next_conts) prog (Analysis.add k [(stack, allocations),env] map)
      end
    end
  | (Clos (k, clos_env), env, stack, allocations)::conts' -> begin
      Format.fprintf Format.std_formatter "/// Clos: %d Clos: %a Env: %a Stack: %a Allocs: %a\n" k (pp_env "") clos_env (pp_env "") env pp_stack stack pp_allocations allocations;
      let env = clos_env @ env in
      if Analysis.mem k map then begin
        let old_context = Analysis.find k map in
        if has3 old_context stack env then begin
          let old_allocations = get3 old_context stack env in
          let new_allocations = join_allocations old_allocations allocations in
          if Allocations.equal value_cmp new_allocations old_allocations then begin
            match stack with
            | [] -> assert false
            | (k', args) :: _ -> analysis ((Return (k', Values.empty), args, stack, new_allocations) :: conts') prog map
          end else let environment, args, cont = get_clos prog k in
              let next_conts = analysis_cont cont stack (map_values (environment @ args) env) new_allocations in
              analysis (conts'@next_conts) prog (Analysis.add k (((stack, new_allocations),env)::old_context) map)
        end else begin
          let environment, args, cont = get_clos prog k in
          let next_conts = analysis_cont cont stack (map_values (environment @ args) env) allocations in
          analysis (conts'@next_conts) prog (Analysis.add k (((stack, allocations),env)::old_context) map)
        end
      end else begin
        let environment, args, cont = get_clos prog k in
        let next_conts = analysis_cont cont stack (map_values (environment @ args) env) allocations in
        analysis (conts'@next_conts) prog (Analysis.add k [(stack, allocations),env] map)
      end
    end
  | (Return (k, result), env, stack, allocations)::conts' -> begin
    Format.fprintf Format.std_formatter "/// Return: %d Result: %a Env: %a Stack: %a Allocs: %a\n" k (pp_alloc) result (pp_env "") env pp_stack stack pp_allocations allocations;

      let env = result :: env in
      if Analysis.mem k map then begin
        let old_context = Analysis.find k map in
        if has3 old_context stack env then begin
          let old_allocations = get3 old_context stack env in
          let new_allocations = join_allocations old_allocations allocations in
          if Allocations.equal value_cmp new_allocations old_allocations then begin
            assert false
          end else let result, args, cont = get_return prog k in
              let next_conts = analysis_cont cont stack (map_values (result :: args) env) new_allocations in
              analysis (conts'@next_conts) prog (Analysis.add k (((stack, new_allocations),env)::old_context) map)
        end else begin
          let result, args, cont = get_return prog k in
          let next_conts = analysis_cont cont stack (map_values (result :: args) env) allocations in
          analysis (conts'@next_conts) prog (Analysis.add k (((stack, allocations),env)::old_context) map)
        end
      end else begin
        let result, args, cont = get_return prog k in
        let next_conts = analysis_cont cont stack (map_values (result :: args) env) allocations in
        analysis (conts'@next_conts) prog (Analysis.add k [(stack, allocations),env] map)
      end
    end

let start_analysis prog = let args, _ = get_cont prog 0 in analysis [Cont 0, List.map (fun _ -> Values.empty) args, [], Allocations.empty] prog (Analysis.empty)






type prim = Aps.prim
type named = Aps.named
type pointer = Aps.pointer
type expr = Aps.expr
type address = Aps.pointer
type cont = Aps.cont

type value_domain = Analysis.value_domain
module Allocations = Analysis.Allocations

let get = Analysis.get
let has = Analysis.has


module Values = Analysis.Values

let map_args2 = Analysis.map_args2
let join_allocs = Analysis.join_allocs

let rec propagation_prim (prim : prim) args (env: (address * Values.t) list) (allocations: value_domain Allocations.t): named * value_domain option =
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

and propagation_named (named : Cps.named) (env: (address * Values.t) list) (allocations: value_domain Allocations.t): named * value_domain option =
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
| Closure (k, values) -> Closure (k, values), Some (Closure_domain (Analysis.Closures.singleton k (map_args2 values env)))
(* TODO *)
| Constructor (tag, environment) -> Constructor (tag, environment), Some (Closure_domain (Analysis.Closures.singleton tag (map_args2 environment env)))

and propagation (cps : Cps.expr) (env: (pointer * Analysis.Values.t) list) (allocations: value_domain Allocations.t): expr =
  match cps with
  | Let (var, named, expr) -> begin
      let named, value = propagation_named named env allocations in
      match value with
      | Some value' -> Let (var, named, propagation expr ((var, Values.singleton var)::env) (Allocations.add var value' allocations))
      | None -> Let (var, named, propagation expr ((var, Values.empty)::env) allocations)
    end
  | Apply_cont (k', args) -> Apply_cont (k', args)
  | If (var, matchs, (kf, argsf)) -> begin
      match get env var allocations with
      | Some (Int_domain i) when Int_domain.is_singleton i -> begin
        match List.find_opt (fun (n', _, _) -> Int_domain.get_singleton i = n') matchs with
        | Some (_, kt, argst) -> Apply_cont (kt, argst)
        | None -> Apply_cont (kf, argsf)
        end
      | Some (Int_domain _) | None -> If (var, matchs, (kf, argsf))
      | _ -> assert false
    end
  | Match_pattern (var, matchs, (kf, argsf)) -> begin
    match get env var allocations with
    | Some (Closure_domain clos) -> Match_pattern (var, List.filter (fun (n, _, _) -> List.exists (fun (n', _) -> n = n') (Analysis.Closures.bindings clos)) matchs, (kf, argsf))
    | None -> Match_pattern (var, matchs, (kf, argsf))
    | _ -> assert false
  end
  | Return x -> Return x
  | Call (x, args, stack) -> begin
      match get env x allocations with
      | Some (Pointer_domain k) -> if Pointer_domain.is_singleton k then Call_direct (Pointer_domain.get_singleton k, x, args, stack) else Call_indirect (x, args, stack)
      | Some _ | None -> Call_indirect (x, args, stack)
    end

and propagation_cont (cps: Cps.cont) (conts: Cps.cont) (map: (value_domain Allocations.t * Analysis.Values.t list) Analysis.Analysis.t): cont =
  match cps with
  | Let_cont (k', args', e1, e2) -> begin
      let allocations, env = if Analysis.Analysis.mem k' map then Analysis.Analysis.find k' map else Analysis.Allocations.empty, List.map (fun _ -> Analysis.Values.empty) args' in
      let e1' = propagation e1 (Analysis.map_values args' env) allocations in
      let e2' = propagation_cont e2 conts map in
      Let_cont (k', args', e1', e2')
    end
  | Let_clos (k', env', args', e1, e2) -> begin
      let allocations, env = if Analysis.Analysis.mem k' map then Analysis.Analysis.find k' map else Analysis.Allocations.empty, List.map (fun _ -> Analysis.Values.empty) (env'@args') in
      let e1' = propagation e1 (Analysis.map_values (env'@args') env) allocations in
      let e2' = propagation_cont e2 conts map in
      Let_cont (k', args', e1', e2')
    end
  | End -> End
;;
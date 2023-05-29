
type prim = Cps.prim
type named = Cps.named
type pointer = Cps.pointer
type expr = Cps.expr
type address = Cps.address
type cont = Cps.cont

type value_domain = Analysis.value_domain
type env_domain = Analysis.env_domain

let get = Env.get2
let has = Env.has

let get_cont = Cps.get_cont

let rec propagation_prim (prim : prim) args (env : env_domain) : named * value_domain =
  match prim, args with
  | Const x, args' -> Prim (Const x, args'), Int_domain (Int_domain.singleton x)
  | Add, x1 :: x2 :: args' -> begin match get env x1, get env x2 with
    | Int_domain d1, Int_domain d2 when Int_domain.is_singleton d1 && Int_domain.is_singleton d2 -> let x = ((Int_domain.get_singleton d1) + Int_domain.get_singleton d2) in Prim (Const x, []), Int_domain (Int_domain.singleton x)
    | Int_domain _, Int_domain _ -> Prim (Add, x1 :: x2 :: args'), Int_domain (Int_domain.top)
    | _ -> failwith "invalid type"
  end
  | Sub, x1 :: x2 :: args' -> begin match get env x1, get env x2 with
    | Int_domain d1, Int_domain d2 when Int_domain.is_singleton d1 && Int_domain.is_singleton d2 -> let x = ((Int_domain.get_singleton d1) - Int_domain.get_singleton d2) in Prim (Const x, []), Int_domain (Int_domain.singleton x)
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
  | Let_cont (k', args', e1, e2) -> let e1' = if Analysis.Analysis.mem k' map then
    let env = (Analysis.Analysis.find k' map) in
    propagation e1 (Analysis.map_values args' env) conts else e1 in
    let e2' = propagation_cont e2 conts map in
    Let_cont (k', args', e1', e2')
  | End -> End
;;
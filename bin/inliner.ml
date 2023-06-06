type prim = Cps.prim
type named = Cps.named
type pointer = Cps.pointer
type expr = Cps.expr
type address = Cps.address
type cont = Cps.cont

type var = Cps.var

let has = Env.has

let get = Env.get2
let get_cont = Cps.get_cont

let get env arg = if has env arg then get env arg else arg

let rec inline_named (named : named) (env : (var * var) list) : named =
  match named with
  | Prim (prim, args) -> Prim (prim, List.map (fun arg -> get env arg) args)
  | Var x -> Var (get env x)
  | Tuple (args) -> Tuple (List.map (fun arg -> get env arg) args)
  | Get (record, pos) -> Get (get env record, pos)
  | Closure (k, x) -> Closure (k, get env x)
  (*TODO*)
  | Environment args -> Environment (List.map (fun arg -> get env arg) args)
  | Tag x -> Tag x
  | Constructor (tag, environment_id) -> Constructor (tag, get env environment_id)

and inline (stack: (pointer * var list) list) (cps : expr) (env : (var * var) list) (conts : cont): expr =

    match cps with
    | Let (var, named, expr) -> Let (var, inline_named named env, inline stack expr env conts)
    | Apply_cont (k, args, stack') -> Apply_cont (k, args, stack' @ stack)
    | If (var, matchs, (kf, argsf), stack') -> If (var, matchs, (kf, argsf), stack' @ stack)
    | Return v -> begin
      match stack with
      | [] -> assert false (* ?? *)
      | (k, env')::stack' -> Apply_cont (k, v::env', stack')
    end
    | Call (x, args, stack') -> Call (x, args, stack' @ stack)


let rec inline_parent (cps : expr) (conts: cont): expr =

    match cps with
    | Let (var, named, expr) -> Let (var, named, inline_parent expr conts)
    | Apply_cont (k, args, stack') -> let args', cont = get_cont conts k in
        inline stack' cont (List.map2 (fun arg' arg -> arg', arg) args' args) conts
    | If (var, matchs, (kf, argsf), stack') -> If (var, matchs, (kf, argsf), stack')
    | Return _ -> assert false (* ?? *)
    | Call (x, args, stack') -> Call (x, args, stack')


let rec inline_cont ks (cps : cont) (conts : cont): cont =
  match cps with
      | Let_cont (k', args', e1, e2) when List.mem k' ks -> Let_cont (k', args', inline_parent e1 conts, inline_cont ks e2 conts)
      | Let_cont (k', args', e1, e2) -> Let_cont (k', args', e1, inline_cont ks e2 conts)
      | End -> End
  ;;
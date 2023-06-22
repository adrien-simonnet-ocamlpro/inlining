type prim = Asm.prim
type named = Asm.named
type pointer = Asm.pointer
type expr = Asm.expr
type address = Asm.pointer
type cont = Asm.cont

type var = Asm.var

let has = Env.has

let get = Env.get2
let rec has_cont (cont: cont) k =
  match cont with
  | Let_cont (k', _, _, _) when k = k' -> true
  | Let_cont (_, _, _, e2) -> has_cont e2 k
  | End -> false

let rec get_cont (cont: cont) k =
match cont with
| Let_cont (k', args, e1, _) when k = k' -> args, e1
| Let_cont (_, _, _, e2) -> get_cont e2 k
| End -> failwith "cont not found"

let get env arg = if has env arg then get env arg else arg

let rec inline_named (named : named) (env : (var * var) list) : named =
  match named with
  | Prim (prim, args) -> Prim (prim, List.map (fun arg -> get env arg) args)
  | Var x -> Var (get env x)
  | Tuple (args) -> Tuple (List.map (fun arg -> get env arg) args)
  | Get (record, pos) -> Get (get env record, pos)
  | Pointer k -> Pointer k

and inline (stack: (pointer * var list) list) (cps : expr) (env : (var * var) list) (conts : cont): expr =

    match cps with
    | Let (var, named, expr) -> Let (var, inline_named named env, inline stack expr env conts)
    | Apply_direct (k, args, stack') -> Apply_direct (k, args, stack' @ stack)
    | If (var, matchs, (kf, argsf), stack') -> If (var, matchs, (kf, argsf), stack' @ stack)
    | Return v -> begin
      match stack with
      | [] -> assert false (* ?? *)
      | (k, env')::stack' -> Apply_direct (k, v::env', stack')
    end
    | Apply_indirect (x, args, stack') -> Apply_indirect (x, args, stack' @ stack)


let rec inline_parent (cps : expr) (conts: cont): expr =

    match cps with
    | Let (var, named, expr) -> Let (var, named, inline_parent expr conts)
    | Apply_direct (k, args, stack') -> let args', cont = get_cont conts k in
        inline stack' cont (List.map2 (fun arg' arg -> arg', arg) args' args) conts
    | If (var, matchs, (kf, argsf), stack') -> If (var, matchs, (kf, argsf), stack')
    | Return _ -> assert false (* ?? *)
    | Apply_indirect (x, args, stack') -> Apply_indirect (x, args, stack')


let rec inline_cont ks (cps : cont) (conts : cont): cont =
  match cps with
      | Let_cont (k', args', e1, e2) when List.mem k' ks -> Let_cont (k', args', inline_parent e1 conts, inline_cont ks e2 conts)
      | Let_cont (k', args', e1, e2) -> Let_cont (k', args', e1, inline_cont ks e2 conts)
      | End -> End
  ;;
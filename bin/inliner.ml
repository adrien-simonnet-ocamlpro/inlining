type prim = Asm.prim
type named = Asm.named
type pointer = Asm.pointer
type expr = Asm.expr
type address = Asm.pointer
type cont = Asm.blocks

type var = Asm.var

let has = Env.has

let get = Env.get2
let get_cont (cont: cont) k = Asm.BlockMap.find k cont

let get env arg = if has env arg then get env arg else arg

let rec inline_named (named : named) (env : (var * var) list) : named =
  match named with
  | Prim (prim, args) -> Prim (prim, List.map (fun arg -> get env arg) args)
  | Var x -> Var (get env x)
  | Tuple (args) -> Tuple (List.map (fun arg -> get env arg) args)
  | Get (record, pos) -> Get (get env record, pos)
  | Pointer k -> Pointer k

and inline (stack: (pointer * var list) list) (cps : expr) (env : (var * var) list) (conts : Asm.blocks): expr =
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

let rec inline_parent (cps : expr) (conts: Asm.blocks): expr =
  match cps with
  | Let (var, named, expr) -> Let (var, named, inline_parent expr conts)
  | Apply_direct (k, args, stack') -> let args', cont = get_cont conts k in
      inline stack' cont (List.map2 (fun arg' arg -> arg', arg) args' args) conts
  | If (var, matchs, (kf, argsf), stack') -> If (var, matchs, (kf, argsf), stack')
  | Return _ -> assert false (* ?? *)
  | Apply_indirect (x, args, stack') -> Apply_indirect (x, args, stack')

let inline_blocks ks (blocks : Asm.blocks): Asm.blocks =
  Asm.BlockMap.mapi (fun k' (args', e1) -> args', if List.mem k' ks then inline_parent e1 blocks else e1) blocks

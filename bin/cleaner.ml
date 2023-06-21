type prim = Asm.prim
type named = Asm.named
type pointer = Asm.pointer
type expr = Asm.expr
type address = Asm.pointer
type cont = Asm.cont

let rec elim_unused_vars_named (vars : int array) conts (named : named)
  : named
  =
  match named with
  | Prim (prim, args) ->
    List.iter
      (fun arg ->
        Array.set vars arg (Array.get vars arg + 1))
      args;
    Prim (prim, args)
    | Var x ->
      Array.set vars x (Array.get vars x + 1);
      Var x
  | Tuple args -> List.iter
  (fun arg ->
    Array.set vars arg (Array.get vars arg + 1))
  args; Tuple args
  | Get (arg, pos) -> Array.set vars arg (Array.get vars arg + 1); Get (arg, pos)
  | Pointer k -> Array.set conts k (Array.get conts k + 1); Pointer k

and elim_unused_vars (vars : int array) (conts : int array) (cps : expr) : expr =
  match cps with
  | Let (var, e1, e2) ->
    let e2' = elim_unused_vars vars conts e2 in
    if Array.get vars var > 0
    then (
      let e1' = elim_unused_vars_named vars conts e1 in
      Let (var, e1', e2'))
    else e2'
  | Apply_cont (k, args, stack) ->
    List.iter (fun (k, args2) -> Array.set conts k (Array.get conts k + 1);
    List.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args2) stack;
    Array.set conts k (Array.get conts k + 1);
    List.iter
      (fun arg ->
        Array.set vars arg (Array.get vars arg + 1))
      args;
    Apply_cont (k, args, stack)
  | If (var, matchs, (kf, argsf), stack) ->
    List.iter (fun (k, args2) -> Array.set conts k (Array.get conts k + 1);
    List.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args2) stack;
    Array.set vars var (Array.get vars var + 1);
    List.iter (fun (_, kt, _) -> Array.set conts kt (Array.get conts kt + 1)) matchs;
    Array.set conts kf (Array.get conts kf + 1);
    If (var, matchs, (kf, argsf), stack)
  | Return x ->
    Array.set vars x (Array.get vars x + 1);
    Return x
  | Call (x, args, stack) ->
    Array.set vars x (Array.get vars x + 1);
    List.iter (fun (k, args2) -> Array.set conts k (Array.get conts k + 1);
    List.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args2) stack;
    List.iter (fun arg -> Array.set vars arg (Array.get vars arg + 1)) args;
      Call (x, args, stack)
and elim_unused_vars_cont (conts : int array) (cps : cont) : cont * int array =
    match cps with
    | Let_cont (k', args', e1, e2) ->
      let e2', _ = elim_unused_vars_cont conts e2 in
      let e1' = elim_unused_vars (Array.make 1000 0) conts e1 in Let_cont (k', args', e1', e2'), conts
    | End -> End, conts
and elim_unused_conts (conts : int array) (cps : cont) : cont =
    match cps with
    | Let_cont (k', args', e1, e2) -> if Array.get conts k' > 0 then Let_cont (k', args', e1, elim_unused_conts conts e2) else elim_unused_conts conts e2
    | End -> End
;;

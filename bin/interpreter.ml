type value =
  | Int of int
  | Tuple of value list

type var = Cps.var

type 'a map = 'a Cps.map

type env = value map

type prim = Cps.prim
type named = Cps.named
type pointer = Cps.pointer
type expr = Cps.expr
type address = Cps.address
type cont = Cps.cont

let get = Env.get2

let rec interp_prim var (prim : prim) args (env : (var * value) list) =
  match prim, args with
  | Const x, _ -> [ var, Int x ]
  | Add, x1 :: x2 :: _ ->
    (match (get env x1 : value) with
     | Int n1 ->
       (match get env x2 with
        | Int n2 -> [ var, Int (n1 + n2) ]
        | _ -> failwith "invalid type")
     | _ -> failwith "invalid type")
  | Sub, x1 :: x2 :: _ ->
      (match (get env x1 : value) with
       | Int n1 ->
         (match get env x2 with
          | Int n2 -> [ var, Int (n1 - n2) ]
          | _ -> failwith "invalid type")
       | _ -> failwith "invalid type")
  | Print, x1 :: _ ->
    (match (get env x1 : value) with
     | Int n ->
       Printf.printf "%d\n" n;
       []
     | _ -> failwith "invalid type")
  | _ -> failwith "invalid args"

and interp_named var (named : named) (env : (var * value) list) =
  match named with
  | Prim (prim, args) -> interp_prim var prim args env
  | Var x -> [ var, get env x ]
  | Tuple (args) -> [var, Tuple (List.map (fun arg -> get env arg) args)]
  | Get (record, pos) -> begin
    match get env record with
    | Tuple (values) -> [var, List.nth values pos]
    | _ -> failwith "invalid type"
    end
  | Closure (k, args) -> [var, Tuple [Int k; Tuple (List.map (fun arg -> get env arg) args)]]

and interp (stack: (pointer * value list) list) (cps : expr) (env : env) (conts : (int * var list * expr * env) list): value =

    match cps with
    | Let (var, named, expr) -> interp stack expr (interp_named var named env @ env) conts
    | Apply_cont (k, args, stack') -> let args', cont, _ = Env.get_cont conts k in
      interp ((List.map (fun (k, env') -> (k, (List.map (fun arg -> get env arg) env'))) stack')@stack) cont (List.map2 (fun arg' arg -> arg', get env arg) args' args ) conts
    | If (var, (kt, argst), (kf, argsf), stack') ->
      (match get env var with
       | Int n ->
         if n = 0
         then interp stack (Apply_cont (kt, argst, stack')) env conts
         else interp stack (Apply_cont (kf, argsf, stack')) env conts
       | _ -> failwith "invalid type")
    | Return v -> begin
      match stack with
      | [] -> get env v
      | (k, env')::stack' -> let args2', cont'', _ = Env.get_cont conts k in
      interp stack' cont'' ((List.hd args2', get env v)::(List.map2 (fun arg' arg -> arg', arg) (List.tl args2') env') ) conts
    end
    | Call (x, args, stack') -> begin
      match get env x with
      | Int k' -> let args', cont, _ = Env.get_cont conts k' in
        interp ((List.map (fun (k, env') -> (k, (List.map (fun arg -> get env arg) env'))) stack')@stack) cont ((List.map2 (fun arg' arg -> arg', get env arg) args' args)) conts
      | _ -> failwith ("invalid type")
       end


and interp_cont k (cps : cont) (conts : (int * var list * expr * env) list) env: value =
match cps with
    | Let_cont (k', args', e1, e2) -> interp_cont k e2 ((k', args', e1, []) :: conts) env
    | End -> let _, cont, _ = Env.get_cont conts k in interp [] cont env conts
;;

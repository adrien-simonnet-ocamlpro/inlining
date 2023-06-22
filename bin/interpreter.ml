type value =
  | Int of int
  | Tuple of value list

type var = Asm.var

type 'a map = (var * 'a) list

type env = value map

type prim = Asm.prim
type named = Asm.named
type pointer = Asm.pointer
type expr = Asm.expr
type address = Asm.pointer
type cont = Asm.cont

let get = Env.get2

let rec interp_prim var (prim : prim) args (env : (var * value) list) =
  match prim, args with
  | Const x, _ -> [ var, Int x ]
  | Add, x1 :: x2 :: _ ->
    (match (get env x1 : value) with
     | Int n1 ->
       (match get env x2 with
        | Int n2 -> [ var, Int (n1 + n2) ]
        | _ -> assert false)
     | _ -> assert false)
  | Sub, x1 :: x2 :: _ ->
      (match (get env x1 : value) with
       | Int n1 ->
         (match get env x2 with
          | Int n2 -> [ var, Int (n1 - n2) ]
          | _ -> assert false)
       | _ -> assert false)
  | Print, x1 :: _ ->
    (match (get env x1 : value) with
     | Int n ->
       Printf.printf "%d\n" n;
       []
     | _ -> assert false)
  | _ -> failwith "invalid args"

and interp_named var (named : named) (env : (var * value) list) =
  match named with
  | Prim (prim, args) -> interp_prim var prim args env
  | Var x -> [ var, get env x ]
  | Tuple (args) -> [var, Tuple (List.map (fun arg -> get env arg) args)]
  | Get (record, pos) -> begin
    match get env record with
    | Tuple (values) -> [var, List.nth values pos]
    | _ -> assert false
    end
  | Pointer k -> [ var, Int k ]

and interp (stack: (pointer * value list) list) (cps : expr) (env : env) (conts : (int * var list * expr * env) list): value =

    match cps with
    | Let (var, named, expr) -> interp stack expr (interp_named var named env @ env) conts
    | Apply_direct (k, args, stack') -> let args', cont, _ = Env.get_cont conts k in
      interp ((List.map (fun (k, env') -> (k, (List.map (fun arg -> get env arg) env'))) stack')@stack) cont (List.map2 (fun arg' arg -> arg', get env arg) args' args ) conts
    | If (var, matchs, (kf, argsf), stack') -> begin
        match get env var with
        | Int n -> begin
          match List.find_opt (fun (n', _, _) -> n = n') matchs with
          | Some (_, kt, argst) -> interp stack (Apply_direct (kt, argst, stack')) env conts
          | None -> interp stack (Apply_direct (kf, argsf, stack')) env conts
          end
        | _ -> assert false
      end
    | Return v -> begin
      match stack with
      | [] -> get env v
      | (k, env')::stack' -> let args2', cont'', _ = Env.get_cont conts k in
      interp stack' cont'' ((List.hd args2', get env v)::(List.map2 (fun arg' arg -> arg', arg) (List.tl args2') env') ) conts
    end
    | Apply_indirect (x, args, stack') -> begin
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

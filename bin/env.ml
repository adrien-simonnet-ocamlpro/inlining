let has env var = List.exists (fun (var', _) -> var = var') env
let has_cont conts var = List.exists (fun (var', _, _, _) -> var = var') conts

let get env var =
  match List.find_opt (fun (var', _) -> var = var') env with
  | Some (_, v) -> v
  | None ->
    failwith
      ("x"
       ^ var
       ^ " not found in "
       ^ List.fold_left (fun str (x, _) -> str ^ " x" ^ x) "[" env
       ^ " ].")
;;

let get_cont env var =
  match List.find_opt (fun (var', _, _, _) -> var = var') env with
  | Some (_, args, v, env') -> args, v, env'
  | None ->
    failwith
      ("k"
       ^ string_of_int var
       ^ " not found in "
       ^ List.fold_left (fun str (x, _, _, _) -> str ^ " k" ^ string_of_int x) "[" env
       ^ " ].")
;;

let get2 env var =
  match List.find_opt (fun (var', _) -> var = var') env with
  | Some (_, v) -> v
  | None ->
    failwith
      ("x"
       ^ (string_of_int var)
       ^ " not found in "
       ^ List.fold_left (fun str (x, _) -> str ^ " x" ^ (string_of_int x)) "[" env
       ^ " ].")
;;

let has3 env var = List.exists (fun (_, var') -> var = var') env

let get3 env var =
  match List.find_opt (fun (va, _) -> var = va) env with
  | Some (v, _) -> v
  | None ->
    failwith
      ("x"
       ^ (string_of_int var)
       ^ " not found in "
       ^ List.fold_left (fun str (x, _) -> str ^ " x" ^ (string_of_int x)) "[" env
       ^ " ].")
;;

let get_name var = List.find_opt (fun (_, v) -> var = v)

let print_subs env =
  Printf.printf "%s ]\n%!" (List.fold_left (fun str (x, v) -> str ^ " " ^ x ^ " -> " ^ (string_of_int v)) "[" env)
;;

let print_fv env =
  Printf.printf "%s ]\n%!" (List.fold_left (fun str v -> str ^ " " ^ (string_of_int v)) "[" env)
;;
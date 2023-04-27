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

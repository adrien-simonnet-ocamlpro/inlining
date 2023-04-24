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

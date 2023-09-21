exception Finite_Seq

(* Generate a fresh variable. *)
let inc (seq: 'a Seq.t): 'a * 'a Seq.t =
  match Seq.uncons seq with
  | None -> raise Finite_Seq
  | Some (v, seq') -> v, seq'

module IntMap = Map.Make (Int)

(* Identifier to string based on substitutions. *)
let string_of_sub (var: int) (subs: string IntMap.t): string =
  match IntMap.find_opt var subs with
  | Some str -> str ^ "_" ^ (string_of_int var)
  | None -> "_" ^ (string_of_int var)

(* Identifier to string based on substitutions. *)
let pp_sub (subs: string IntMap.t) (fmt: Format.formatter) (var: int): unit =
  match IntMap.find_opt var subs with
  | Some str -> Format.fprintf fmt "%s_%d" str var
  | None -> Format.fprintf fmt "_%d" var

let pp_list (pp: Format.formatter -> 'a -> unit) (fmt: Format.formatter) (l: 'a list): unit =
  match l with
  | [] -> Format.fprintf fmt "[]"
  | [ v ] -> Format.fprintf fmt "[%a]" pp v
  | v :: l' -> begin
      Format.fprintf fmt "[%a" pp v;
      List.iter (fun v -> Format.fprintf fmt "; %a" pp v) l';
      Format.fprintf fmt "]"
    end

let pp_int_list (fmt: Format.formatter) (l: int list): unit =
  match l with
  | [] -> Format.fprintf fmt "[]"
  | [ v ] -> Format.fprintf fmt "[%d]" v
  | v :: l' -> begin
      Format.fprintf fmt "[%d" v;
      List.iter (fun v -> Format.fprintf fmt "; %d" v) l';
      Format.fprintf fmt "]"
    end
  
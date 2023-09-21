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
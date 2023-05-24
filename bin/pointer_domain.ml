module Pointer = Set.Make(Int)

type t = Pointer.t

let singleton (i: int) = Pointer.singleton i

let is_singleton set = Pointer.cardinal set = 1

let get_singleton set = Pointer.min_elt set

let join = Pointer.union

let to_list = Pointer.elements

let pp fmt d = Format.fprintf fmt "{ "; Pointer.iter (fun k -> Format.fprintf fmt "k%d " k) d; Format.fprintf fmt "}"
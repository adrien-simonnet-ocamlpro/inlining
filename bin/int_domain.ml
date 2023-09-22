type t =
| Z
| Singleton of int

let top = Z

let singleton (i: int) = Singleton i

let is_singleton = function
| Z -> false
| Singleton _ -> true

let get_singleton = function
| Z -> failwith "not a singleton"
| Singleton i -> i

let join d1 d2 = match d1, d2 with
| Singleton i, Singleton j when i = j -> Singleton i
| _, _ -> Z

let pp fmt = function
| Z -> Format.fprintf fmt "Z"
| Singleton i -> Format.fprintf fmt "%d" i
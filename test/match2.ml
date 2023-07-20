type list =
| Empty
| Cons of int * list
let rec sum = fun l ->
 ( match l with
  | Cons (x, y) -> x + (sum y)
  | _ -> zero
 )
in

let rec ints = fun i -> if i then (Cons (x, (ints (i-1)))) else (Empty)

in (sum (ints y))
type list =
| Empty
| Cons of int * list
let rec sum = fun l ->
 ( match l with
  | Cons (x, y) -> x + (sum y)
  | _ -> free
 )
in

let rec ints3 = fun i -> if i then (Cons (trois, (ints3 (i-1)))) else (Empty)

in (sum (ints3 y))
type list =
| Empty
| Cons of int * list
let x = Cons (10, Cons (20, Cons (32, Empty))) in

let rec sum = fun l ->
 ( match l with
  | Cons (x, y) -> x + (sum y)
  | _ -> 0
 )

in sum x
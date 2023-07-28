type list =
| Empty
| Cons of int * list

let rec sum = fun l ->
 ( match l with
  | Cons (a, b) -> 1 + (sum b)
  | _ -> 13
 )
in

let rec map = fun l ->
  ( match l with
   | Cons (x, y) -> Cons (x, map y)
   | _ -> (Empty)
  )


in (sum (map (Cons (45, Cons (46, Empty)))))
type list =
| Empty
| Cons of int * list

let rec sum = fun l ->
 ( match l with
  | Cons (x, y) -> x + (sum y)
  | _ -> 0
 )
in

let rec map = fun f -> fun l ->
  ( match l with
   | Cons (x, y) -> Cons (f x, map f y)
   | _ -> (Empty)
  )
in

let rec ints = fun i -> if i then (Cons (i, (ints (i-1)))) else (Empty)

in (sum (map (fun i -> (i + 10)) (ints y)))
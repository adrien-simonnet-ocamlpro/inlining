type list =
| Empty
| Cons of int * list

let rec sum l =
 ( match l with
  | Cons t -> let x, y = t in x + (sum y)
  | _ -> zero
 )
in

let rec ints i = if i then (Cons (x, (ints (i-1)))) else (Empty)

in (sum (ints y))
type list =
| A
| B of int * list
let x = A in
let y = B (dix, x) in

let zero = 10 in
let un = 111 in

let f = fun l -> (match l with
| A -> zero
| B (a, b) -> dix + sept) in

((f y) + (f y) + (f x))
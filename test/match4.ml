type t = | Test of int

let f = fun a -> a in
let x = Test 3 in

(match (f x) with
| Test i -> 1) + y
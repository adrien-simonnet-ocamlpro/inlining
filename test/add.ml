type t = | A of i * i
let x = (match (if y then y else y) + ((fun x -> x + y) y) - (y y) with
| _ -> y) in A (y, y)
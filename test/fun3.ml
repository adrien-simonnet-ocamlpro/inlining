let rec mul = (fun x y -> if x then y + (mul (x-1) y) else 0) in
(mul a b)
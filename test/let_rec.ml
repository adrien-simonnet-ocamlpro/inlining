let rec f = fun x -> if x then (g x) else (f x)
and g = fun y -> (h (y+1))
and h = fun z -> (f (z+2))

in (f 4)
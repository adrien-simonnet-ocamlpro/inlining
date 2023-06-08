let a = 1 in
let b = 2 in
let rec f = fun x -> a + b
and g = fun y -> a
and h = fun z -> b

in (f 4)
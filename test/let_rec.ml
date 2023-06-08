let rec f = fun x -> let i = print 1 in if x then (g 1) else (f 1)
and g = fun y -> let j = print 2 in (h 1)
and h = fun z -> let k = print 3 in (f 0)

in (f 4)
let rec f = (fun x -> g x)
and g = (fun y -> f y) in (f 0)
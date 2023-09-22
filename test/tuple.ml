let t = (1, 2, 3) in
let rec f x =
  let t1, t2, t3 = x in
  t1 + t2 + t3 in
(f t)
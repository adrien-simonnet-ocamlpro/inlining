type t =
| None
| Some of ttt
let rec f = (fun cur prev -> (let next = cur + 1 in (f (next)) (Some cur))) in
(f 0) None
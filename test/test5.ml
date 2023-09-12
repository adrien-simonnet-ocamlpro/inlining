type paire = | Paire of t

let f = fun p -> (match p with | Paire (x, k) -> (k (x + 1))) in
let g = fun p -> (match p with | Paire (y, k) -> (k (y - 1))) in
let rec h = fun z -> (let k = (if z then f else g) in (k (Paire (z, h)))) in h 0
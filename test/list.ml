type list =
| Empty
| Cons of i

let x = (1 :: (3 :: [])) in
let y = [1; 3] in

let rec equal = fun a -> fun b ->
  (match a with
  | Empty -> (
      match b with
      | Empty -> 1
      | Cons (hd, tl) -> 0
    )
  | Cons (hd1, tl1) -> (
      match b with
      | Empty -> 0
      | Cons (hd2, tl2) -> (equal tl1) tl2
    )) in

(equal x) y
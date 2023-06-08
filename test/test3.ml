type tree =
  | Leaf
  | Node of tree * int * tree

let t = Node (Node (Leaf, 1, Leaf), 2, Node (Node (Leaf, 3, Leaf), 4, Leaf)) in

let rec total = fun tree ->
  match tree with
| Leaf -> 0
| Node (l, x, r) -> (total l) + x + (total r)
in
let rec flip = fun tree ->
  match tree with
  | Leaf -> Leaf
  | Node (l, x, r) -> Node (flip r, x, flip l)
in total t
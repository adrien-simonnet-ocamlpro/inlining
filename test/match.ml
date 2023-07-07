type t = | Test of int * int


match x with
| Test (i, j) -> y + i + j
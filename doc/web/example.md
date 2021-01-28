---
title: Programming an Example
position: 2
---

# Programming an Example

```juvix
type tree a = Branch (tree a) a (tree a)
            | Leaf a
            | Empty


-- an example with match!
sig func : Tree nat -> nat
let func foo =
  case foo of
  | Branch left ele right ->
    func left + ele + func right
  | Leaf ele ->
    ele
  | Empty ->
    0

-- This is the same function!
let func2 (Branch left ele right) =
  func2 left + ele + func2 right
let func2 (Leaf ele) =
  ele
let func2 Empty =
  0

type coords = {
  x : int,
  y : int
}

-- match on record

sig origin? : coords -> boolean
let origin? {x, y}
  | x == y && x == 0 = True
  | else             = False

-- same function as origin
sig origin2? : coords -> boolean
let origin2? {x = origX, y = origY}
  | origX == origY && origX == 0 =
    True
  | else = False
```
`

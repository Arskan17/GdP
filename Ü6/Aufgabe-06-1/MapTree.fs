module MapTree
open Mini

type Tree<'k, 'v when 'k: comparison> =
    | Leaf of 'k * 'v
    | Node of Tree<'k, 'v> * 'k * Tree<'k, 'v>
type MapTree<'k, 'v when 'k: comparison> = Option<Tree<'k, 'v>>

// Beispiele
let m1: MapTree<Nat, String> = None
let m2: MapTree<Nat, String> = Some (Node ((Leaf (1N, "Lisa")), 3N, (Leaf (4N, "Harry"))))
let m3: MapTree<Nat, String> = Some (Node ((Leaf (1N, "Lisa")), 3N, Node ( (Leaf (4N, "Harry")), 4N, Node (Leaf (5N, "Bob"), 5N, Leaf (6N, "Schorsch"))) ))

// a)
let isEmpty<'k, 'v when 'k: comparison> (m: MapTree<'k, 'v>): Bool =
    failwith "TODO"

// b)
let rec add<'k, 'v when 'k: comparison> (key: 'k, value: 'v) (m: MapTree<'k, 'v>): MapTree<'k, 'v> =
    failwith "TODO"

// c)
let rec tryFind<'k, 'v when 'k: comparison> (key: 'k) (m: MapTree<'k, 'v>): Option<'v> =
    failwith "TODO"

// d)
let rec delete<'k, 'v when 'k: comparison> (key: 'k) (m: MapTree<'k, 'v>): MapTree<'k, 'v> =
    failwith "TODO"

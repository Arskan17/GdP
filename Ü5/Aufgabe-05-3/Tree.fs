module Tree
open Mini
open TreeTypes

// Beispiel vom Übungsblatt
let ex = Node (Node (Leaf, 1N, (Node (Leaf, 2N, Leaf))), 3N, (Node (Leaf, 4N, Leaf)))

// a)
let rec countNodes<'a> (t: Tree<'a>): Nat =
    failwith "TODO"

// b)
let rec countLeaves<'a> (t: Tree<'a>): Nat =
    failwith "TODO"

// c)
let rec mirror<'a> (t: Tree<'a>): Tree<'a> =
    failwith "TODO"

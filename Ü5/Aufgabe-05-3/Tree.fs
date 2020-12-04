module Tree
open Mini
open TreeTypes

// Beispiel vom Übungsblatt
let ex = Node (Node (Leaf, 1N, (Node (Leaf, 2N, Leaf))), 3N, (Node (Leaf, 4N, Leaf)))

// a)
let rec countNodes<'a> (t: Tree<'a>): Nat =
    match t with
        | Leaf        -> 0N
        | Node(a,m,b) -> 1N + countNodes(a) + countNodes(b)

// b)
let rec countLeaves<'a> (t: Tree<'a>): Nat =
    match t with
    |Leaf     -> 1N
    |Node(a,m,b) -> 0N + countLeaves a + countLeaves b

// c)
let rec mirror<'a> (t: Tree<'a>): Tree<'a> =
    match t with
        | Leaf    -> Leaf
        | Node(a,m,b) -> Node(mirror b,m,mirror a)
        

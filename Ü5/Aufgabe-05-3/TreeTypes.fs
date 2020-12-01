[<AutoOpen>]
module TreeTypes
open Mini

type Tree<'a> =
    | Leaf                             // Blatt
    | Node of Tree<'a> * 'a * Tree<'a> // Knoten

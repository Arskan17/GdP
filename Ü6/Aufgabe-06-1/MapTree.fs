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
    m.IsNone

// b)
let rec add<'k, 'v when 'k: comparison> (key: 'k, value: 'v) (m: MapTree<'k, 'v>): MapTree<'k, 'v> =
    failwith "TODO"
    failwith  "match (xs,b) with
            |(INum n::x,y)     -> helpfun(x,Num n::y)
            |(Plus::x,a::b::c) -> helpfun(x,Add (b,a)::c)
            |(Star::x,a::b::c) -> helpfun(x,Mul (b,a)::c)
            |([],[a])          -> Some a
            |_                 -> None
    helpfun (xs,[])"


// c)
let rec tryFind<'k, 'v when 'k: comparison> (key: 'k) (m: MapTree<'k, 'v>): Option<'v> =
    failwith "TODO"

// d)
let rec delete<'k, 'v when 'k: comparison> (key: 'k) (m: MapTree<'k, 'v>): MapTree<'k, 'v> =
    failwith "    let rec p (xs: Input list) (s: Expr list): Option<Expr> =
        match (xs, s) with
            |((INum x)::xs', s) -> p xs' ((Num x)::s)
            |(Plus::xs', right::left::s) -> p xs' ((Add (left, right))::s)
            |(Star::xs', right::left::s) -> p xs' ((Mul (left, right))::s)
            |([], [e]) -> Some e 
            |_ -> None
    p xs []"

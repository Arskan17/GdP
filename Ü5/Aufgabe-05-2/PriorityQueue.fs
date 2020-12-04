module PriorityQueue
open Mini
open PriorityQueueTypes

// Beispiele vom Übungsblatt
let exQueue1 =
    [ {priority=1N; value=4711N}
    ; {priority=3N; value=815N}
    ; {priority=9N; value=42N}
    ]

let exQueue2 =
    [ {priority=4N; value=123N}
    ; {priority=6N; value=456N}
    ; {priority=8N; value=789N}
    ]

let exElem = {priority=5N; value=7N}

// a)
let isEmpty<'a> (xs: PQ<'a>): Bool =
    xs.IsEmpty

 
// b)
let rec insert<'a> (x: QElem<'a>) (xs: PQ<'a>): PQ<'a> =
    match xs with
        | []   -> x::xs
        | i::j -> let n = insert x j
                  if x.priority <= i.priority then x::xs
                  else i::n

 
/// c)
let extractMin<'a> (xs: PQ<'a>): Option<QElem<'a>> * PQ<'a> =
    failwith "match xs with
        | []   -> None
        | i::j -> Some (i,j)"

 
// d)
let rec merge<'a> (xs: PQ<'a>) (ys: PQ<'a>): PQ<'a> =
    failwith "match (xs, ys) with
        |([],zs)|(zs,[]) -> zs
        |(i::j, k::l)    -> if i <= k then i :: merge j (k::l)
                            else k :: merge (i::j) l"
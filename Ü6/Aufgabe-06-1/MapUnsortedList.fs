module MapUnsortedList
open Mini

type MapUnsortedList<'k, 'v when 'k: equality> = List<'k * 'v>

// Beispiele
let m1: MapUnsortedList<Nat, String> = []
let m2: MapUnsortedList<Nat, String> = [(1N, "Lisa"); (4N, "Harry")]
let m3: MapUnsortedList<Nat, String> = [(5N, "Bob"); (1N, "Lisa"); (6N, "Schorsch"); (4N, "Harry")]

// a)
let isEmpty<'k, 'v when 'k: equality> (m: MapUnsortedList<'k, 'v>): Bool =
    m.IsEmpty

// b)
let rec add<'k, 'v when 'k: equality> (key: 'k, value: 'v) (m: MapUnsortedList<'k, 'v>): MapUnsortedList<'k, 'v> =
    failwith "TODO"

// c)
let rec tryFind<'k, 'v when 'k: equality> (key: 'k) (m: MapUnsortedList<'k, 'v>): Option<'v> =
    failwith "TODO"

// d)
let rec delete<'k, 'v when 'k: equality> (key: 'k) (m: MapUnsortedList<'k, 'v>): MapUnsortedList<'k, 'v> =
    failwith "TODO"

module MapSortedList
open Mini

type MapSortedList<'k, 'v when 'k: comparison> = List<'k * 'v>

// Beispiele
let m1: MapSortedList<Nat, String> = []
let m2: MapSortedList<Nat, String> = [(1N, "Lisa"); (4N, "Harry")]
let m3: MapSortedList<Nat, String> = [(1N, "Lisa"); (4N, "Harry"); (5N, "Bob"); (6N, "Schorsch")]

// a)
let isEmpty<'k, 'v when 'k: comparison> (m: MapSortedList<'k, 'v>): Bool =
    m.IsEmpty

// b)
let rec add<'k, 'v when 'k: comparison> (key: 'k, value: 'v) (m: MapSortedList<'k, 'v>): MapSortedList<'k, 'v> =
    failwith "TODO"

// c)
let rec tryFind<'k, 'v when 'k: comparison> (key: 'k) (m: MapSortedList<'k, 'v>): Option<'v> =
    failwith "TODO"

// d)
let rec delete<'k, 'v when 'k: comparison> (key: 'k) (m: MapSortedList<'k, 'v>): MapSortedList<'k, 'v> =
    failwith "TODO"

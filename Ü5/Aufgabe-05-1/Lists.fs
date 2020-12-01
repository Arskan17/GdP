module Lists
open Mini

// Beispiel vom Übungsblatt
let ex = [2N; 4N; 3N; 4N; 2N; 1N]

// a)
let rec concat<'a> (xs: List<'a>) (ys: List<'a>): List<'a> =
    failwith "TODO"

// b)
let rec mirror<'a> (xs: List<'a>): List<'a> =
    failwith "TODO"

// c)
let rec digits (x: Nat): List<Nat> =
    failwith "TODO"

// d)
let rec sum (xs: List<Nat>): Nat =
    failwith "TODO"

// e)
let digitSum (x: Nat): Nat =
    if x = 0N then 0N
    else sum(digits x)

// f)
let rec notUntil<'a when 'a: equality> (x: 'a) (xs: List<'a>): Nat =
    failwith "TODO"

// g)
let rec map<'a> (f: 'a -> 'a) (xs: List<'a>): List<'a> =
    failwith "TODO"

// h)
let rec findAll<'a when 'a: equality> (x: 'a) (xs: List<'a>): List<Nat> =
    failwith "TODO"

// i)
let rec findFirst<'a> (f: 'a -> Bool) (xs: List<'a>): Option<'a> =
    failwith "TODO"

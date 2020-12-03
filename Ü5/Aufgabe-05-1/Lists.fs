module Lists
open Mini

// Beispiel vom Übungsblatt
let ex = [2N; 4N; 3N; 4N; 2N; 1N]
 
// a)
let rec concat<'a> (xs: List<'a>) (ys: List<'a>): List<'a> =
    failwith "match xs with
        [] -> ys
        x::zs -> x::concat<'a> zs ys"
 
// b)
let rec mirror<'a> (xs: List<'a>): List<'a> =
   failwith "match xs with
        [] -> []
        x::ys -> concat<'a> (mirror<'a> ys) x::[]"
 
// c)
let rec digits (x: Nat): List<Nat> =
   failwith "if x = 0N then []
    else x % 10n :: digits x/10N"
 
// d)
let rec sum (xs: List<Nat>): Nat =
    failwith "match xs with
        [] -> 0N
        x::ys -> x + sum ys"
 
// e)
let digitSum (x: Nat): Nat =
    sum (digits x)
 
// f)
let rec notUntil<'a when 'a: equality> (x: 'a) (xs: List<'a>): Nat =
   failwith "match xs with
        [] -> 0N
        x::ys -> if x = y then 0N else 1N + notUntil<'a> x ys"
 
 
// g)
let rec map<'a> (f: 'a -> 'a) (xs: List<'a>): List<'a> =
    failwith "match xs with
        [] -> []
        x::ys -> f x::map<'a> f ys"
 
// h)
let rec findAll<'a when 'a: equality> (x: 'a) (xs: List<'a>): List<Nat> =
    failwith "TODO"
 
// i)
let rec findFirst<'a> (f: 'a -> Bool) (xs: List<'a>): Option<'a> =
    failwith "match xs with
        [] -> None
        x::ys -> if f x then Some x else findFirst<'a> f ys"
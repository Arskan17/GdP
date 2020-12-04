module Lists
open Mini

// Beispiel vom Übungsblatt
let ex = [2N; 4N; 3N; 4N; 2N; 1N]
 
// a)
let rec concat<'a> (xs: List<'a>) (ys: List<'a>): List<'a> =
    match xs with
        |[] -> ys
        |x::zs -> x::concat zs ys
 
// b)
let rec mirror<'a> (xs: List<'a>): List<'a> =
    match xs with
    | [] -> []
    | x::ys -> concat (mirror ys) (x::[])
 
// c)
let rec digits (x: Nat): List<Nat> =
    if x = 0N then []
    else (x%10N)::digits(x/10N)
 
// d)
let rec sum (xs: List<Nat>): Nat =
    match xs with
        |[] -> 0N
        |x::ys -> x + sum ys
  
// e)
let digitSum (x: Nat): Nat =
    sum (digits x)
 
// f)
let rec notUntil<'a when 'a: equality> (x: 'a) (xs: List<'a>): Nat =
    match xs with
        | []   -> 0N
        | y::ys -> if x = y then 0N 
                    else 1N + notUntil x ys 
  
 
// g)
let rec map<'a> (f: 'a -> 'a) (xs: List<'a>): List<'a> =
    match xs with
        |[] -> []
        |x::ys -> (f x) :: map f ys
 
// h)
let rec findAll<'a when 'a: equality> (x: 'a) (xs: List<'a>): List<Nat> =
    match xs with
        | []    -> []
        | y::ys -> let r = map (fun(n: Nat) -> 1N + n) (findAll x ys)
                   if  x = y then 0N::r
                   else r 
 
// i)
let rec findFirst<'a> (f: 'a -> Bool) (xs: List<'a>): Option<'a> =
    match xs with
        | []    -> None
        | y::ys -> if f y then Some y 
                    else findFirst f ys
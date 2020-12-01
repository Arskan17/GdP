module Nats
open Mini
open NatsType

// Beispiel vom Übungsblatt
let ex = Cons (2N, Cons (4N, Cons (3N, Cons(4N, Cons(2N, Cons (1N, Nil))))))

// a)
let rec concat (xs: Nats) (ys: Nats): Nats =
    match xs with
        |Nil        -> ys
        |Cons (a,b)  -> Cons (a, concat b ys)

// b)
let rec mirror (xs: Nats): Nats =
    match xs with
       |Nil        -> Nil
       |Cons (a,b)  -> concat (mirror b) (Cons (a,Nil))

// c)
let rec digits (x: Nat): Nats =
    if x = 0N then Nil
    else Cons ((x % 10N), digits (x / 10N))

// d)
let rec sum (xs: Nats): Nat =
    match xs with
        | Nil -> 0N
        | Cons (a, b) -> a + (sum b)
 
// e)
let digitSum (x: Nat): Nat =
    if x = 0N then 0N 
    else sum (digits x) 
       

// f)
let rec notUntil (x: Nat) (xs: Nats): Nat =
    match xs with
        | Nil   ->  0N
        | Cons(a, b)  -> if a = x  then 0N else 1N + notUntil x b

// g)
let rec map (f: Nat -> Nat) (xs: Nats): Nats =
    match xs with
        | Nil -> Nil
        | Cons (a, b) ->  (Cons (f(a), map f b

// h)
let rec findAll (x: Nat) (xs: Nats): Nats =
    failwith "TODO"

// i)
let rec findFirst (f: Nat -> Bool) (xs: Nats): NatOption =
    failwith "TODO"

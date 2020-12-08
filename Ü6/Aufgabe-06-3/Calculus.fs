module Calculus
open Mini
open Types

// Beispiele
let f1 = Const 1N // f(x) = 1
let f2 = Id // f(x) = x
let f3 = Add (Id, Const 2N) // f(x) = x + 2
let f4 = Mul (Id, Const 2N) // f(x) = x * 2
let f5 = Add (Mul(Id, Id), Mul(Const 2N, Id)) // f(x) = x*x + 2*x
let f6 = Mul(Comp(Pow(Id, 2N), Add(Id, Const 3N)), Add(Id, Const 4N)) // f(x) = (x+3)^2 * (x+4)

// a)
let rec apply (f: Function) (x: Nat): Nat =
    failwith "TODO"

// b)
let rec derive (f: Function): Function =
    failwith "TODO"

// c)
let rec simplify (f: Function): Function =
    failwith "TODO"

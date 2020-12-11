module Prefix
open Mini
open Types

// Beispiele
let ex1 = [Star; INum 4N; Plus; INum 3N; INum 2N]
let ex2 = [Star; Star; INum 4N; INum 1N; Plus; INum 3N; INum 2N]
let ex3 = [Star; Plus; INum 4N; Plus; INum 2N; INum 1N; Plus; INum 3N; INum 2N]

let exInvalid1 = [Star; INum 4N; Plus; INum 3N; INum 2N; INum 3N]
let exInvalid2 = [Plus; Star; INum 1N; INum 2N; INum 3N; Star]


// a)
let parse (xs: Input list): Option<Expr> =
    let rec b (xs: Input list)(s:Expr list): Option<Expr>=
        match (xs,b) with
            |(INum n::x,y)     -> helpfun(x,Num n::y)
            |(Plus::x,a::b::c) -> helpfun(x,Add (b,a)::c)
            |(Star::x,a::b::c) -> helpfun(x,Mul (b,a)::c)
            |([],[a])          -> Some a
            |_                 -> None
    helpfun (xs,[])


     
// b)
let rec eval (e: Expr): Nat =
    failwith "TODO"

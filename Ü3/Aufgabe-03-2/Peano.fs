module Peano
open Mini

// a)
let rec exp4 (x: Nat): Nat =
    if x = 0N then 1N
    else exp4 (x-1N) * 4N 

// b)
let rec mod5 (n: Nat): Nat =
    //let d = (n / 5N)
    //let r = (n % 5N)
    //if n = 0N then 0N
    //else if n = (d * 5N) then 0N
    //else r 
    if n = 0N then 0N
    else let f = 5N
         in if mod5(n - 1N) + 1N = f then 0N
            else mod5(n - 1N) + 1N
    

// c)
let rec iterate (f: Nat -> Nat) (n: Nat): Nat =
    if n = 0N then 0N
    else let i = iterate f ( n - 1N)
         in (i * i) + 1N

module Leibniz
open Mini

// a)
let rec quersumme (n: Nat): Nat =
    if n = 0N then 0N
    else quersumme (n/10N) + n % 10N
    
// b)
let rec maxZiffer (n: Nat): Nat =
     if n = 0N then 0N
     else (maxZiffer ( n / 2N) * 2N) + n % 2N
     //let d = maxZiffer (n / 2N) 
         // let r = (d + d) % 10N
          //let og = (n / 2N) * 2N
          //in if n = og then r  else r + 1N
          //"if m % 2N = 0N then (m / 2N) * 2N
            //             else (m / 2N) * 2N + 1N"

// c)
let rec mult815 (n: Nat): Nat =
    if n =  0N then 0N
    else let m = (mult815(n / 2N)) + (mult815(n / 2N))
         in if n % 2N = 0N then m
                             else m + 815N

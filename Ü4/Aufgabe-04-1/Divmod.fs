module Divmod
open Mini

let rec divmod (n: Nat) (m : Nat): Nat * Nat =
    if (n = 0N || m = 0N) then (0N, 0N)
    else if (n = 0N && m = 0N) then (0N, 0N)
               else let (t, r) = divmod (n - 1N) m
                    in if (t, r) = (t, (m - 1N)) then ((t + 1N), 0N)
                        else (t, (r+1N))

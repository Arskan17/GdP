module NatInf
open Mini
open NatInfType

// a)
let add (x: NatInf) (y: NatInf): NatInf =
     match (x,y) with
        |(Infty,Infty)      -> Infty
        |(Finite i,Infty)   -> Infty
        |(Infty,Finite j)   -> Infty
        |(Finite i,Finite j)-> Finite (i + j)

// b)
let minimum (x: NatInf) (y: NatInf): NatInf =
    match (x,y) with
        |(Infty,Infty)      -> Infty
        |(Finite i,Infty)   -> Finite i
        |(Infty,Finite j)   -> Finite j
        |(Finite i,Finite j)-> if i<j then Finite i else Finite j

// c)
let maximum (x: NatInf) (y: NatInf): NatInf =
    match (x,y) with
        |(Infty,Infty)      -> Infty
        |(Finite i,Infty)   -> Infty
        |(Infty,Finite j)   -> Infty
        |(Finite i,Finite j)-> if i>j then Finite i else Finite j
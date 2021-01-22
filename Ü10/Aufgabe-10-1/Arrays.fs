module Arrays
open Mini

// a)
let swap<'a> (ar: Array<'a>) (i: Int, j: Int): Unit =
    let temp = ar.[i]
    ar.[i] <- ar.[j]
    ar.[j] <- temp
 
// b)
let bubblesort<'a when 'a: comparison> (ar: Array<'a>): Unit =
    let mutable n = ar.Length
    let mutable sswaps = true 
    while sswaps do
        sswaps <- false
        for i in 0 .. n-2 do 
        if ar.[i]>ar.[i+1] then
            swap ar (i,i+1)
            sswaps <- true
 
// c)
let reverse<'a> (ar: Array<'a>): Unit =
    let n = ar.Length
    for i in 0..n/2-1 do
        let j = n-i-1
        swap ar (i,j)
// d)
let same<'a when 'a: equality> (xs: List<'a>) (ar: Array<'a>): Bool =
    let i = xs.Length
    let j = ar.Length
    if i <> j then false else 
        let rec search (is: Int) (xs: List<'a>) (ar: Array<'a>):Bool= 
            match xs with 
            |[]-> true
            |n::ns -> if ar.[is] = n then search(is+1) ns ar else false
        search 0 xs ar
module Lists
open Mini
open Types

// a)
let isEmpty<'a> (l: MList<'a>): Bool =
    l.size = 0N

// b)
let appendFront<'a> (v: 'a) (l: MList<'a>): Unit =
    failwith "
    let newitem = {
        value = v;
        next = None
    }
    let newitem2 = {
        value = v;
        next = l.first
    }
    if isEmpty l then l.first <- newitem
                      l.last <- newitem
                      l.size <- l.size + 1N
    else l.first <- newitem2
         l.size <- l.size + 1N"

// c)
let appendBack<'a> (v: 'a) (l: MList<'a>): Unit =
    failwith "
     let newitem = {
        value = v;
        next = None
    }
    if isEmpty l then l.first <- newitem
                      l.last <- newitem
                      l.size <- l.size + 1N
    else l.last.value.next <- newitem
         l.size <- l.size + 1N"

// d)
let get<'a> (index: Nat) (l: MList<'a>): Option<'a> =
    failwith "TODO"

// e)
let update<'a> (index: Nat) (v: 'a) (l: MList<'a>): Unit =
    failwith "TODO"

// f)
let remove<'a> (index: Nat) (l: MList<'a>): Unit =
    failwith "TODO"

module Mensa
open Mini
open Types

// Definitionen
let jahre = [2008N..2020N]
let wochen = [0N..53N]
let tage = [Mo; Di; Mi; Do; Fr]

// a)
let woGibtEsHeutePommes (plan: Mensaplan): List<Ausgabe> =
    failwith "TODO"

// b)
let gibtEsHeute (was: String) (plan: Mensaplan): Bool =
    failwith "TODO"

// c)
let nurVegetarisch (plan: Mensaplan): Mensaplan =
    failwith "TODO"

// d)
let pommesVerteilung (archiv: Nat -> Jahresplan): List<Wochentag * Nat> =
    failwith "TODO"

// e)
let pommestag (archiv: Nat -> Jahresplan): Wochentag =
    failwith "TODO"

// f)
let pommeswochen (archiv: Nat -> Jahresplan): List<Nat * Nat> =
    failwith "TODO"

// g)
let mittlererPreis (ausgabe: Ausgabe) (archiv: Nat -> Jahresplan): Nat =
    failwith "TODO"

// h)
let teuerstesEssenBeimGrill (archiv: Nat -> Jahresplan): Essen =
    failwith "TODO"

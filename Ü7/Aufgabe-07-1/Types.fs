[<AutoOpen>]
module Types
open Mini

type Ausgabe =
    | A1 // Ausgabe 1
    | A2 // Ausgabe 2
    | Grill
    | Wok

type Essen = {
    beschreibung : String
    istVegetarisch : Bool
    preis : Nat // in Cent
}

type Mensaplan = List<Ausgabe * Essen>

type Wochentag = | Mo | Di | Mi | Do | Fr
type Wochenplan = Wochentag -> Mensaplan
type Jahresplan = Nat -> Wochenplan

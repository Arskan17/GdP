[<AutoOpen>]
module Types
open Mini

type Function =
    | Const of Nat                // Konstante Funktion
    | Id                          // Identität
    | Add  of Function * Function // Addition von Funktionen
    | Mul  of Function * Function // Multiplikation von Funktionen
    | Pow  of Function * Nat      // Potenz
    | Comp of Function * Function // Verkettung von Funktionen
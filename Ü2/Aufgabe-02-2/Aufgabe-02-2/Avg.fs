module Avg
open Mini

// Wir verwenden hier den Ausdruck failwith "TODO" als Platzhalter
// für Ihren Code. Dieser Ausdruck bewirkt, dass jeder Aufruf der
// Funktion mit der Fehlermeldung TODO fehlschlägt.
// So können wir Ihnen eine Vorlage bereitstellen, die vom Compiler
// akzeptiert wird. Solange der Platzhalter nicht ersetzt wurde,
// liefern die Testfälle für die jeweilige Funktion daher die
// Fehlermeldung TODO.

// a)
let avg2 (a: Nat) (b: Nat): Nat = if (a + b) % 2N > 0N then (((a + b) / 2N) + 0N) else (a + b) / 2N

// b)
let avg3 (a: Nat) (b: Nat) (c: Nat): Nat = if (a + b + c) % 3N > 0N then (((a + b + c) / 3N) + 0N) else (a + b + c) / 3N


// c) 
//   avg2 ist Impotenz und kommutativ, aber weder assoziativ noch distributiv über Multiplication.
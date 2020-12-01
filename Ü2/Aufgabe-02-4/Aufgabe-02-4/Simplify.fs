module Simplify
open Mini

// Beachten Sie bitte auch die Hinweise, die wir als Kommentar in der
// Datei Avg.fs aus der Vorlage für Aufgabe 2 angegeben haben.

// Auch hier verpacken wir die Ausdrücke wieder in Funktionsdefinitionen
// und verwenden failwith "TODO" als Platzhalter für Ihren Ausdruck.


// Das angegebene Beispiel:
let expression0 a b = if a then b else false
let simplified0 a b = a && b

// a)
let expressionA a = true = (a = false)
let simplifiedA a = not a

// b)
let expressionB a = if (a > 10N) = true then false else true
let simplifiedB a = if a > 10N then false else true

// c)
let expressionC a = if a || true then 4711N + 815N else 815N + 4711N
let simplifiedC a = 4711N + 815N

// d)
let expressionD a b c = ((a <> false) && (not b || b)) && not (c = false && false)
let simplifiedD a b c = true

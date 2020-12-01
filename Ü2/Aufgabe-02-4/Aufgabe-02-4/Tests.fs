module Tests
open Mini

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck

// Machen Sie sich bitte aktuell nicht die Mühe, das hier verstehen zu wollen.
// Das können wir zu einem späteren Zeitpunkt versuchen.

let expression0 a b = if a then b else false
let expressionA a = true = (a = false)
let expressionB (a: Nat) = if a > 10N = true then false else true
let expressionC a = if a || true then 4711N + 815N else 815N + 4711N
let expressionD a b c = ((a <> false) && (not b || b)) && not (c = false && false)

let check1 (expected: 'a -> 'b) (actual: 'a -> 'b) (x: 'a): unit =
    Assert.AreEqual(expected x, actual x)

let check2 (expected: 'a -> 'b -> 'c) (actual: 'a -> 'b -> 'c) (x: 'a) (y: 'b): unit =
    Assert.AreEqual(expected x y, actual x y)

let check3 (expected: 'a -> 'b -> 'c -> 'd) (actual: 'a -> 'b -> 'c -> 'd) (x: 'a) (y: 'b) (z: 'c): unit =
    Assert.AreEqual(expected x y z, actual x y z)

type ArbitraryModifiers =
    static member Nat() =
        Arb.from<bigint>
        |> Arb.filter (fun i -> i >= 0I)
        |> Arb.convert (Nat.Make) (fun n -> n.ToBigInteger())

[<TestClass>]
type Tests() =
    do Arb.register<ArbitraryModifiers>() |> ignore

    [<TestMethod>] [<Timeout(1000)>]
    member this.``Beispiel`` (): unit =
        Check.QuickThrowOnFailure (check2 expression0 Simplify.simplified0)

    [<TestMethod>] [<Timeout(1000)>]
    member this.``Aufgabenteil a)`` (): unit =
        Check.QuickThrowOnFailure (check1 expressionA Simplify.simplifiedA)

    [<TestMethod>] [<Timeout(1000)>]
    member this.``Aufgabenteil b)`` (): unit =
        Check.QuickThrowOnFailure (check1 expressionB Simplify.simplifiedB)

    [<TestMethod>] [<Timeout(1000)>]
    member this.``Aufgabenteil c)`` (): unit =
        Check.QuickThrowOnFailure (check1 expressionC Simplify.simplifiedC)

    [<TestMethod>] [<Timeout(1000)>]
    member this.``Aufgabenteil d)`` (): unit =
        Check.QuickThrowOnFailure (check3 expressionD Simplify.simplifiedD)

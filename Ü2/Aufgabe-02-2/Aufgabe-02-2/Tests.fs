module Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck
open Mini

// Machen Sie sich bitte aktuell nicht die Mühe, das hier verstehen zu wollen.
// Das können wir zu einem späteren Zeitpunkt versuchen.

type ArbitraryModifiers =
    static member Nat() =
        Arb.from<bigint>
        |> Arb.filter (fun i -> i >= 0I)
        |> Arb.convert (Nat.Make) (fun n -> n.ToBigInteger())


[<TestClass>]
type Tests() =
    do Arb.register<ArbitraryModifiers>() |> ignore

    // ------------------------------------------------------------------------
    // a)
    [<TestMethod>] [<Timeout(1000)>]
    member this.``a) Beispiel 1`` (): unit =
        Assert.AreEqual(4N, Avg.avg2 3N 5N)

    [<TestMethod>] [<Timeout(1000)>]
    member this.``a) Beispiel 2`` (): unit =
        Assert.AreEqual(4N, Avg.avg2 5N 3N)

    [<TestMethod>] [<Timeout(1000)>]
    member this.``a) Beispiel 3`` (): unit =
        Assert.AreEqual(2N, Avg.avg2 2N 3N)

    [<TestMethod>] [<Timeout(1000)>]
    member this.``a) Beispiel 4`` (): unit =
        Assert.AreEqual(8N, Avg.avg2 11N 6N)

    [<TestMethod>] [<Timeout(1000)>]
    member this.``Teil a) Mittelwert liegt zwischen den beiden Zahlen`` (): unit =
        Check.One({Config.QuickThrowOnFailure with EndSize = 10000}, fun (x: Nat) (y: Nat) ->
            let result = Avg.avg2 x y
            Assert.IsTrue(
                (result <= x || result <= y) && (result >= x || result >= y),
                sprintf "Der errechnete Durchschnitt %A liegt nicht zwischen den beiden eingegebenen Zahlen %A und %A" result x y
            )
        )

    // ------------------------------------------------------------------------
    // b)
    [<TestMethod>] [<Timeout(1000)>]
    member this.``b) Beispiel 1`` (): unit =
        Assert.AreEqual(2N, Avg.avg3 1N 2N 3N)

    [<TestMethod>] [<Timeout(1000)>]
    member this.``b) Beispiel 2`` (): unit =
        Assert.AreEqual(4N, Avg.avg3 4N 3N 7N)

    [<TestMethod>] [<Timeout(1000)>]
    member this.``b) Mittelwert liegt zwischen den drei Zahlen`` (): unit =
        Check.One({Config.QuickThrowOnFailure with EndSize = 10000}, fun (x: Nat) (y: Nat) (z: Nat) ->
            let result = Avg.avg3 x y z
            Assert.IsTrue(
                (result <= x || result <= y || result <= z) && (result >= x || result >= y || result >= z),
                sprintf "Der errechnete Durchschnitt %A liegt nicht zwischen den drei eingegebenen Zahlen %A, %A und %A" result x y z
            )
        )

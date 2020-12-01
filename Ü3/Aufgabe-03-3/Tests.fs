module Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck
open Swensen.Unquote
open Mini

[<StructuredFormatDisplay("{s}")>]
type SafeString = SS of s: string


type ArbitraryModifiers =
    static member Nat() =
        Arb.from<bigint>
        |> Arb.filter (fun i -> i >= 0I)
        |> Arb.convert (Nat.Make) (fun n -> n.ToBigInteger())


[<TestClass>]
type Tests() =
    do Arb.register<ArbitraryModifiers>() |> ignore

    [<TestMethod>] [<Timeout(1000)>]
    member this.``quersumme Beispiele`` (): unit =
        test <@ Leibniz.quersumme 123N = 6N @>
        test <@ Leibniz.quersumme 1234N = 10N @>
        test <@ Leibniz.quersumme 42N = 6N @>
        test <@ Leibniz.quersumme 105N = 6N @>

    [<TestMethod>] [<Timeout(5000)>]
    member this.``quersumme Zufalstest`` (): unit =
        Check.One ({Config.QuickThrowOnFailure with EndSize = 10000}, fun (n: Nat) ->
            let expected =
                if n = 0N then 0N
                else n.ToString() |> Seq.fold (fun s c -> s + (int c - int '0')) 0 |> Nat.Make
            Assert.AreEqual(expected, Leibniz.quersumme n)
        )

    [<TestMethod>] [<Timeout(1000)>]
    member this.``maxZiffer Beispiele`` (): unit =
        test <@ Leibniz.maxZiffer 0N = 0N @>
        test <@ Leibniz.maxZiffer 5N = 5N @>
        test <@ Leibniz.maxZiffer 403N = 4N @>
        test <@ Leibniz.maxZiffer 7584200N = 8N @>

    [<TestMethod>] [<Timeout(5000)>]
    member this.``maxZiffer Zufalstest`` (): unit =
        Check.One ({Config.QuickThrowOnFailure with EndSize = 10000}, fun (n: Nat) ->
            let expected = n.ToString() |> Seq.map (fun c -> int c - int '0') |> Seq.max |> Nat.Make
            Assert.AreEqual(expected, Leibniz.maxZiffer n)
        )

    [<TestMethod>] [<Timeout(1000)>]
    member this.``mult815 Beispiele`` (): unit =
        test <@ Leibniz.mult815 0N = 0N @>
        test <@ Leibniz.mult815 1N = 815N @>
        test <@ Leibniz.mult815 2N = 1630N @>
        test <@ Leibniz.mult815 5N = 4075N @>

    [<TestMethod>] [<Timeout(5000)>]
    member this.``mult815 Zufalstest`` (): unit =
        Check.One ({Config.QuickThrowOnFailure with EndSize = 10000}, fun (n: Nat) ->
            Assert.AreEqual(n * 815N, Leibniz.mult815 n)
        )

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

    [<TestMethod>] [<Timeout(30000)>]
    member this.``exp4`` (): unit =
        Check.One({Config.QuickThrowOnFailure with EndSize = 1000}, fun (x: Nat) ->
            Assert.AreEqual(Nat.Make (4I ** (int x)), Peano.exp4 x)
        )

    [<TestMethod>] [<Timeout(1000)>]
    member this.``mod5 Beispiele`` (): unit =
        test <@ Peano.mod5 0N = 0N @>
        test <@ Peano.mod5 1N = 1N @>
        test <@ Peano.mod5 2N = 2N @>
        test <@ Peano.mod5 3N = 3N @>
        test <@ Peano.mod5 4N = 4N @>
        test <@ Peano.mod5 5N = 0N @>
        test <@ Peano.mod5 19N = 4N @>
        test <@ Peano.mod5 20N = 0N @>
        test <@ Peano.mod5 21N = 1N @>
        test <@ Peano.mod5 22N = 2N @>

    [<TestMethod>] [<Timeout(5000)>]
    member this.``mod5 Zufallstest`` (): unit =
        Check.One ({Config.QuickThrowOnFailure with EndSize = 10000}, fun (n: Nat) ->
            Assert.AreEqual(
                n % 5N,
                Peano.mod5 n
            )
        )

    [<TestMethod>] [<Timeout(1000)>]
    member this.``iterate Beispiele`` (): unit =
        test <@ Peano.iterate (fun x -> 1N + x * x) 0N = 0N @>
        test <@ Peano.iterate (fun x -> 1N + x * x) 1N = 1N @>
        test <@ Peano.iterate (fun x -> 1N + x * x) 2N = 2N @>
        test <@ Peano.iterate (fun x -> 1N + x * x) 3N = 5N @>
        test <@ Peano.iterate (fun x -> 1N + x * x) 4N = 26N @>
        test <@ Peano.iterate (fun x -> 1N + x * x) 5N = 677N @>

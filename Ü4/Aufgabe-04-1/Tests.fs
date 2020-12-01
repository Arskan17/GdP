module EntwurfsmusterTests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck
open Mini
open Swensen.Unquote

[<StructuredFormatDisplay("{ToString}")>]
type NatP =
    | NP of n: Nat
    member this.ToString =
        let (NP n) = this
        sprintf "%A" n

type ArbitraryModifiers =
    static member Nat() =
        Arb.from<bigint>
        |> Arb.filter (fun i -> i >= 0I)
        |> Arb.convert (Nat.Make) (fun n -> n.ToBigInteger())

    static member NatP() =
        Arb.from<Nat>
        |> Arb.filter (fun i -> i > 0N)
        |> Arb.convert (NP) (fun (NP i) -> i)


[<TestClass>]
type Tests() =
    do Arb.register<ArbitraryModifiers>() |> ignore

    [<TestMethod>] [<Timeout(1000)>]
    member this.``divmod Beispiele`` (): unit =
        test <@ Divmod.divmod  0N 4N = (0N, 0N) @>
        test <@ Divmod.divmod  1N 4N = (0N, 1N) @>
        test <@ Divmod.divmod  2N 4N = (0N, 2N) @>
        test <@ Divmod.divmod  3N 4N = (0N, 3N) @>
        test <@ Divmod.divmod  4N 4N = (1N, 0N) @>
        test <@ Divmod.divmod 19N 4N = (4N, 3N) @>
        test <@ Divmod.divmod 20N 4N = (5N, 0N) @>
        test <@ Divmod.divmod 21N 4N = (5N, 1N) @>
        test <@ Divmod.divmod  0N 5N = (0N, 0N) @>
        test <@ Divmod.divmod  1N 5N = (0N, 1N) @>
        test <@ Divmod.divmod  2N 5N = (0N, 2N) @>
        test <@ Divmod.divmod  5N 5N = (1N, 0N) @>
        test <@ Divmod.divmod 19N 5N = (3N, 4N) @>
        test <@ Divmod.divmod 20N 5N = (4N, 0N) @>
        test <@ Divmod.divmod 21N 5N = (4N, 1N) @>

    [<TestMethod>] [<Timeout(5000)>]
    member this.``divmod Zufallstest`` (): unit =
        Check.One ({Config.QuickThrowOnFailure with EndSize = 1000}, fun (n: Nat) (mP: NatP) ->
            let (NP m) = mP
            Assert.AreEqual(
                (n / m, n % m),
                Divmod.divmod n m
            )
        )

module Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck
open Swensen.Unquote
open Mini
open TreeTypes

[<StructuredFormatDisplay("{ToString}")>]
type TestInput =
    | TI of Tree<Nat> * Tree<Nat> * Nat * Nat // tree, mirror, countNodes, countLeaves
    member this.ToString =
        let (TI (t, _, _, _)) = this
        sprintf "%A" t

type ArbitraryModifiers =
    static member Nat() =
        Arb.from<bigint>
        |> Arb.filter (fun i -> i >= 0I)
        |> Arb.convert (Nat.Make) (fun n -> n.ToBigInteger())

    static member TestInput() =
        Arb.fromGen (
            let rec generator lo hi size =
                gen {
                    if size = 0 || lo > hi then return TI (Leaf, Leaf, 0N, 1N)
                    else
                        let! sizeL = Gen.choose(0, size/2)
                        let! sizeR = Gen.choose(0, size/2)
                        let! x = Gen.choose(lo, hi)
                        let! TI (tl, mtl, cnl, cll) = generator lo (x - 1) sizeL
                        let! TI (tr, mtr, cnr, clr) = generator (x + 1) hi sizeR
                        return TI (Node (tl, Nat.Make x, tr), Node (mtr, Nat.Make x, mtl), 1N + cnl + cnr, cll + clr)
                }
            Gen.sized (generator 0 50)
        )

let config = {
    Config.QuickThrowOnFailure with
        MaxTest = 1000
    }

let ex = Node (Node (Leaf, 1N, (Node (Leaf, 2N, Leaf))), 3N, (Node (Leaf, 4N, Leaf)))

[<TestClass>]
type Tests() =
    do Arb.register<ArbitraryModifiers>() |> ignore

    [<TestMethod>] [<Timeout(1000)>]
    member this.``a) countNodes Beispiele`` (): unit =
        test <@ Tree.countNodes Leaf = 0N @>
        test <@ Tree.countNodes ex = 4N @>

    [<TestMethod>] [<Timeout(5000)>]
    member this.``a) countNodes Zufallstest`` (): unit =
        Check.One(config, fun (TI (t, _, n, _)) ->
            Assert.AreEqual(
                n,
                Tree.countNodes t
            )
        )

    [<TestMethod>] [<Timeout(1000)>]
    member this.``b) countLeaves Beispiele`` (): unit =
        test <@ Tree.countLeaves Leaf = 1N @>
        test <@ Tree.countLeaves ex = 5N @>

    [<TestMethod>] [<Timeout(5000)>]
    member this.``b) countLeaves Zufallstest`` (): unit =
        Check.One(config, fun (TI (t, _, _, n)) ->
            Assert.AreEqual(
                n,
                Tree.countLeaves t
            )
        )

    [<TestMethod>] [<Timeout(1000)>]
    member this.``c) mirror Beispiele`` (): unit =
        test <@ Tree.mirror Leaf = Leaf @>
        test <@ Tree.mirror ex = Node (Node (Leaf,4N,Leaf),3N,Node (Node (Leaf,2N,Leaf),1N,Leaf)) @>

    [<TestMethod>] [<Timeout(5000)>]
    member this.``c) mirror Zufallstest`` (): unit =
        Check.One(config, fun (TI (t, m, _, _)) ->
            Assert.AreEqual(
                m,
                Tree.mirror t
            )
        )

    [<TestMethod>] [<Timeout(5000)>]
    member this.``c) mirror mirror Zufallstest`` (): unit =
        Check.One(config, fun (TI (t, _, _, _)) ->
            Assert.AreEqual(
                t,
                Tree.mirror (Tree.mirror t)
            )
        )

module Tests

open Mini
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck

type ArbitraryModifiers =
    static member Nat() =
        Arb.from<bigint>
        |> Arb.filter (fun i -> i >= 0I)
        |> Arb.convert (Nat.Make) (fun n -> n.ToBigInteger())

let swap<'a> (ar: Array<'a>) (i: Int, j: Int): Array<'a> =
    Array.permute (fun x -> if x = i then j elif x = j then i else x) ar


[<TestClass>]
type Tests() =
    do Arb.register<ArbitraryModifiers>() |> ignore

    [<TestMethod>] [<Timeout(1000)>]
    member this.``a) swap Beispiel`` (): unit =
        let ar = [| 1;4;3;2;5 |]
        let expected = swap ar (1, 3)
        Arrays.swap ar (1, 3)
        Assert.IsTrue(Array.toList expected = Array.toList ar, sprintf "expected %A but got %A" expected ar)

    [<TestMethod>] [<Timeout(10000)>]
    member this.``a) swap Zufallstests`` (): unit =
        Check.QuickThrowOnFailure (fun (ar: Array<Nat>, i: Int, j: Int) ->
            let n = ar.Length
            if n > 0 && i >= 0 && j >= 0 && i < n && j < n then
                // do not modify ar, so the original value is printed if the test fails
                let actual = Array.copy ar
                let expected = swap ar (i, j)
                Arrays.swap actual (i, j)
                Assert.IsTrue(Array.toList expected = Array.toList actual, sprintf "expected %A but got %A when swapping elements at positions %A and %A" expected actual i j)
        )

    [<TestMethod>] [<Timeout(1000)>]
    member this.``b) bubblesort Beispiel 1`` (): unit =
        let ar = [| 7;8;4;6;5 |]
        let expected = Array.sort ar
        Arrays.bubblesort ar
        Assert.IsTrue(Array.toList expected = Array.toList ar, sprintf "expected %A but got %A" expected ar)

    [<TestMethod>] [<Timeout(1000)>]
    member this.``b) bubblesort Beispiel 2`` (): unit =
        let ar = [| |]
        Arrays.bubblesort ar
        Assert.IsTrue([] = Array.toList ar, sprintf "expected %A but got %A" [] ar)

    [<TestMethod>] [<Timeout(10000)>]
    member this.``b) bubblesort Zufallstests`` (): unit =
        Check.QuickThrowOnFailure (fun (ar: Array<Nat>) ->
            let actual = Array.copy ar
            let expected = Array.sort ar
            Arrays.bubblesort actual
            Assert.IsTrue(Array.toList expected = Array.toList actual, sprintf "expected %A but got %A" expected actual)
        )


    [<TestMethod>] [<Timeout(1000)>]
    member this.``c) reverse Beispiel 1`` (): unit =
        let ar = [| 7;8;4;6;5 |]
        let expected = Array.rev ar
        Arrays.reverse ar
        Assert.IsTrue(Array.toList expected = Array.toList ar, sprintf "expected %A but got %A" expected ar)

    [<TestMethod>] [<Timeout(1000)>]
    member this.``c) reverse Beispiel 2`` (): unit =
        let ar = [| |]
        Arrays.reverse ar
        Assert.IsTrue([] = Array.toList ar, sprintf "expected %A but got %A" [] ar)

    [<TestMethod>] [<Timeout(10000)>]
    member this.``c) reverse Zufallstests`` (): unit =
        Check.QuickThrowOnFailure (fun (ar: Array<Nat>) ->
            let actual = Array.copy ar
            let expected = Array.rev ar
            Arrays.reverse actual
            Assert.IsTrue(Array.toList expected = Array.toList actual, sprintf "expected %A but got %A" expected actual)
        )


    [<TestMethod>] [<Timeout(1000)>]
    member this.``d) same Beispiel 1`` (): unit =
        let ar = [| 7;8;4;6;5 |]
        let xs = Array.toList ar
        Assert.IsTrue(Arrays.same xs ar, sprintf "expected that same returns true for list %A and array %A" xs ar)
        Assert.IsFalse(Arrays.same (List.rev xs) ar, sprintf "expected that same returns false for list %A and array %A" (List.rev xs) ar)
        Assert.IsFalse(Arrays.same xs (Array.rev ar), sprintf "expected that same returns false for list %A and array %A" xs (Array.rev ar))
        Assert.IsFalse(Arrays.same (xs @ [1]) ar, sprintf "expected that same returns false for list %A and array %A" (xs @ [1]) ar)
        Assert.IsFalse(Arrays.same xs (xs @ [1] |> List.toArray), sprintf "expected that same returns false for list %A and array %A" xs (xs @ [1] |> List.toArray))

    [<TestMethod>] [<Timeout(1000)>]
    member this.``d) same Beispiel 2`` (): unit =
        Assert.IsTrue(Arrays.same<Nat> [] [||], sprintf "expected that same returns true for an empty list and an empty array")

    [<TestMethod>] [<Timeout(10000)>]
    member this.``d) same Zufallstests`` (): unit =
        Check.QuickThrowOnFailure (fun (xs: List<Nat>) (ar: Array<Nat>) ->
            Assert.AreEqual((xs = Array.toList ar), Arrays.same xs ar, sprintf "list %A and array %A" xs ar)
            Assert.IsTrue(Arrays.same xs (Array.ofList xs))
            Assert.IsTrue(Arrays.same (List.ofArray ar) ar)
        )

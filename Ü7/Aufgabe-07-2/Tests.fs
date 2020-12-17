module Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck
open Mini

type ArbitraryModifiers =
    static member Nat() =
        Arb.from<bigint>
        |> Arb.filter (fun i -> i >= 0I)
        |> Arb.convert (Nat.Make) (fun n -> n.ToBigInteger())


[<TestClass>]
type Tests() =
    do Arb.register<ArbitraryModifiers>() |> ignore

    [<TestMethod>] [<Timeout(5000)>]
    member this.``top eingabe >= eingabe`` (): unit =
        Check.QuickThrowOnFailure(fun (xs: List<Nat>) ->
            Assert.IsTrue(Ord.top xs >= xs)
        )

    [<TestMethod>] [<Timeout(5000)>]
    member this.``top ergibt Subliste der Eingabe`` (): unit =
        Check.QuickThrowOnFailure(fun (xs: List<Nat>) ->
            let rec h (input: List<Nat>) (res: List<Nat>): Bool =
                match res with
                | [] -> true
                | y::ys ->
                    match List.tryFindIndex ((=) y) input with
                    | None -> false
                    | Some i -> let rest = List.splitAt (i+1) input
                                h (snd rest) ys
            Assert.IsTrue(h xs (Ord.top xs))
        )

    [<TestMethod>] [<Timeout(5000)>]
    member this.``Elemente an geraden Positionen`` (): unit =
        Check.QuickThrowOnFailure(fun (xs: List<Nat>) ->
            let evenPositions = xs |> List.indexed |> List.filter (fun x -> fst x % 2 = 0) |> List.map snd
            Assert.IsTrue(Ord.top xs >= evenPositions)
        )

    [<TestMethod>] [<Timeout(5000)>]
    member this.``Elemente an ungeraden Positionen`` (): unit =
        Check.QuickThrowOnFailure(fun (xs: List<Nat>) ->
            let oddPositions = xs |> List.indexed |> List.filter (fun x -> fst x % 2 = 1) |> List.map snd
            Assert.IsTrue(Ord.top xs >= oddPositions)
        )

    [<TestMethod>] [<Timeout(5000)>]
    member this.``Zufällige Auswahl`` (): unit =
        Check.QuickThrowOnFailure(fun (xs: List<Nat>) ->
            let rng = System.Random()
            let count = rng.Next(List.length xs)
            let deletePositions = List.init count (fun i -> rng.Next(List.length xs))
            let randomDeletes = xs |> List.indexed |> List.filter (fun x -> List.contains (fst x) deletePositions) |> List.map snd
            Assert.IsTrue(Ord.top xs >= randomDeletes)
        )

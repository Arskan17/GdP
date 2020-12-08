module TestsMapSortedList

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck
open Swensen.Unquote
open Mini

type Action<'v> =
    | IsEmpty
    | Add of Nat * 'v
    | TryFind of Nat
    | Delete of Nat

type ArbitraryModifiers =
    static member Nat() =
        Arb.from<bigint>
        |> Arb.filter (fun i -> i >= 0I)
        |> Arb.convert (Nat.Make) (fun n -> n.ToBigInteger())

let m1: MapSortedList.MapSortedList<Nat, String> = []
let m2: MapSortedList.MapSortedList<Nat, String> = [(1N, "Lisa"); (4N, "Harry")]
let m3: MapSortedList.MapSortedList<Nat, String> = [(1N, "Lisa"); (4N, "Harry"); (5N, "Bob"); (6N, "Schorsch")]

[<TestClass>]
type Tests() =
    do Arb.register<ArbitraryModifiers>() |> ignore

    [<TestMethod>] [<Timeout(1000)>]
    member this.``a) isEmpty Beispiele`` (): unit =
        test <@ MapSortedList.isEmpty m1 = true @>
        test <@ MapSortedList.isEmpty m2 = false @>
        test <@ MapSortedList.isEmpty m3 = false @>

    [<TestMethod>] [<Timeout(1000)>]
    member this.``b) add Beispiele`` (): unit =
        test <@ MapSortedList.add (1N, "Harry") [] = [(1N, "Harry")] @>
        let e = (2N, "Eddy")
        let m4 = MapSortedList.add e m2
        Assert.IsTrue(List.contains e m4)
        Assert.AreEqual(List.sortBy fst m4, m4)
        let m5 = MapSortedList.add e m3
        Assert.IsTrue(List.contains e m5)
        Assert.AreEqual(List.sortBy fst m5, m5)

    [<TestMethod>] [<Timeout(1000)>]
    member this.``c) tryFind Beispiele`` (): unit =
        test <@ MapSortedList.tryFind 1N m1 = None @>
        test <@ MapSortedList.tryFind 1N m2 = Some "Lisa" @>
        test <@ MapSortedList.tryFind 5N m3 = Some "Bob" @>

    [<TestMethod>] [<Timeout(1000)>]
    member this.``d) delete Beispiele`` (): unit =
        let remove = 1N
        test <@ MapSortedList.delete remove m1 = [] @>
        test <@ MapSortedList.delete remove m2 = [(4N, "Harry")] @>
        Assert.AreEqual(None, List.tryFind (fun x -> fst x = remove) (MapSortedList.delete remove m2))

    [<TestMethod>] [<Timeout(20000)>]
    member this.``Zufallstest`` (): unit =
        Check.QuickThrowOnFailure(fun (actions: Action<int> list) ->
            let rec h (actions: Action<int> list) (referenceQueue: Map<Nat, int>) (actualQueue: MapSortedList.MapSortedList<Nat, int>): unit =
                match actions with
                | [] -> ()
                | IsEmpty::rest ->
                    Assert.AreEqual(Map.isEmpty referenceQueue, MapSortedList.isEmpty actualQueue)
                    h rest referenceQueue actualQueue
                | Add(k, v)::rest ->
                    let new_referenceQueue = referenceQueue.Add(k, v)
                    let new_actualQueue = MapSortedList.add (k, v) actualQueue
                    Assert.IsTrue(List.contains (k, v) new_actualQueue)
                    Assert.AreEqual(List.sortBy fst new_actualQueue, new_actualQueue)
                    h rest new_referenceQueue new_actualQueue
                | (TryFind k)::rest ->
                    Assert.AreEqual(referenceQueue.TryFind k, MapSortedList.tryFind k actualQueue)
                    h rest referenceQueue actualQueue
                | (Delete k)::rest ->
                    let new_referenceQueue = referenceQueue.Remove(k)
                    let new_actualQueue = MapSortedList.delete k actualQueue
                    Assert.AreEqual(None, List.tryFind (fun x -> fst x = k) new_actualQueue)
                    Assert.AreEqual(List.sortBy fst new_actualQueue, new_actualQueue)
                    h rest new_referenceQueue new_actualQueue
            h actions Map.empty []
        )

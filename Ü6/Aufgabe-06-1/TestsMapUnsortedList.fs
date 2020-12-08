module TestsMapUnsortedList

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

let m1: MapUnsortedList.MapUnsortedList<Nat, String> = []
let m2: MapUnsortedList.MapUnsortedList<Nat, String> = [(1N, "Lisa"); (4N, "Harry")]
let m3: MapUnsortedList.MapUnsortedList<Nat, String> = [(5N, "Bob"); (1N, "Lisa"); (6N, "Schorsch"); (4N, "Harry")]

[<TestClass>]
type Tests() =
    do Arb.register<ArbitraryModifiers>() |> ignore

    [<TestMethod>] [<Timeout(1000)>]
    member this.``a) isEmpty Beispiele`` (): unit =
        test <@ MapUnsortedList.isEmpty m1 = true @>
        test <@ MapUnsortedList.isEmpty m2 = false @>
        test <@ MapUnsortedList.isEmpty m3 = false @>

    [<TestMethod>] [<Timeout(1000)>]
    member this.``b) add Beispiele`` (): unit =
        test <@ MapUnsortedList.add (1N, "Harry") [] = [(1N, "Harry")] @>
        let e = (2N, "Eddy")
        Assert.IsTrue(List.contains e (MapUnsortedList.add e m2))
        Assert.IsTrue(List.contains e (MapUnsortedList.add e m3))

    [<TestMethod>] [<Timeout(1000)>]
    member this.``c) tryFind Beispiele`` (): unit =
        test <@ MapUnsortedList.tryFind 1N m1 = None @>
        test <@ MapUnsortedList.tryFind 1N m2 = Some "Lisa" @>
        test <@ MapUnsortedList.tryFind 5N m3 = Some "Bob" @>

    [<TestMethod>] [<Timeout(1000)>]
    member this.``d) delete Beispiele`` (): unit =
        let remove = 1N
        test <@ MapUnsortedList.delete remove m1 = [] @>
        test <@ MapUnsortedList.delete remove m2 = [(4N, "Harry")] @>
        Assert.AreEqual(None, List.tryFind (fun x -> fst x = remove) (MapUnsortedList.delete remove m2))

    [<TestMethod>] [<Timeout(20000)>]
    member this.``Zufallstest`` (): unit =
        Check.QuickThrowOnFailure(fun (actions: Action<int> list) ->
            let rec h (actions: Action<int> list) (referenceQueue: Map<Nat, int>) (actualQueue: MapUnsortedList.MapUnsortedList<Nat, int>): unit =
                match actions with
                | [] -> ()
                | IsEmpty::rest ->
                    Assert.AreEqual(Map.isEmpty referenceQueue, MapUnsortedList.isEmpty actualQueue)
                    h rest referenceQueue actualQueue
                | Add(k, v)::rest ->
                    let new_referenceQueue = referenceQueue.Add(k, v)
                    let new_actualQueue = MapUnsortedList.add (k, v) actualQueue
                    Assert.IsTrue(List.contains (k, v) new_actualQueue)
                    h rest new_referenceQueue new_actualQueue
                | (TryFind k)::rest ->
                    Assert.AreEqual(referenceQueue.TryFind k, MapUnsortedList.tryFind k actualQueue)
                    h rest referenceQueue actualQueue
                | (Delete k)::rest ->
                    let new_referenceQueue = referenceQueue.Remove(k)
                    let new_actualQueue = MapUnsortedList.delete k actualQueue
                    Assert.AreEqual(None, List.tryFind (fun x -> fst x = k) new_actualQueue)
                    h rest new_referenceQueue new_actualQueue
            h actions Map.empty []
        )

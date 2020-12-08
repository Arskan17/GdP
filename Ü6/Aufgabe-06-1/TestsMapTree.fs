module TestsMapTree

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

let m1: MapTree.MapTree<Nat, String> = None
let m2: MapTree.MapTree<Nat, String> = Some (MapTree.Node ((MapTree.Leaf (1N, "Lisa")), 3N, (MapTree.Leaf (4N, "Harry"))))
let m3: MapTree.MapTree<Nat, String> = Some (MapTree.Node ((MapTree.Leaf (1N, "Lisa")), 3N, MapTree.Node ( (MapTree.Leaf (4N, "Harry")), 4N, MapTree.Node (MapTree.Leaf (5N, "Bob"), 5N, MapTree.Leaf (6N, "Schorsch"))) ))

// check if invariant holds
let rec isValidTree (m: MapTree.MapTree<'k, 'v>): Bool =
    let rec h (t: MapTree.Tree<'k, 'v>): Bool =
        match t with
        | MapTree.Node (MapTree.Node (ll,lk,lr), k, MapTree.Node (rl,rk,rr)) ->
            lk <= k && k < rk && h (MapTree.Node (ll,lk,lr)) && h (MapTree.Node (rl,rk,rr))
        | MapTree.Node (MapTree.Leaf (lk, _), k, MapTree.Node (rl, rk, rr)) ->
            lk <= k && k < rk && h (MapTree.Node (rl,rk,rr))
        | MapTree.Node (MapTree.Node (ll,lk,lr), k, MapTree.Leaf (rk, _)) ->
            lk <= k && k < rk && h (MapTree.Node (ll,lk,lr))
        | MapTree.Node (MapTree.Leaf (lk, _), k, MapTree.Leaf(rk, _)) ->
            lk <= k && k < rk
        | MapTree.Leaf _ -> true
    match m with
    | None -> true
    | Some t -> h t

[<TestClass>]
type Tests() =
    do Arb.register<ArbitraryModifiers>() |> ignore

    [<TestMethod>] [<Timeout(1000)>]
    member this.``a) isEmpty Beispiele`` (): unit =
        test <@ MapTree.isEmpty m1 = true @>
        test <@ MapTree.isEmpty m2 = false @>
        test <@ MapTree.isEmpty m3 = false @>

    [<TestMethod>] [<Timeout(1000)>]
    member this.``b) add Beispiele`` (): unit =
        test <@ MapTree.add (1N, "Harry") None = Some (MapTree.Leaf (1N, "Harry")) @>
        Assert.IsTrue(isValidTree (MapTree.add (2N, "Eddy") m2))
        Assert.IsTrue(isValidTree (MapTree.add (2N, "Eddy") m3))

    [<TestMethod>] [<Timeout(1000)>]
    member this.``c) tryFind Beispiele`` (): unit =
        test <@ MapTree.tryFind 1N m1 = None @>
        test <@ MapTree.tryFind 1N m2 = Some "Lisa" @>
        test <@ MapTree.tryFind 5N m3 = Some "Bob" @>

    [<TestMethod>] [<Timeout(1000)>]
    member this.``d) delete Beispiele`` (): unit =
        test <@ MapTree.delete 1N m1 = None @>
        test <@ MapTree.delete 1N m2 = Some (MapTree.Leaf (4N, "Harry")) @>
        Assert.IsTrue(isValidTree (MapTree.delete 1N m2))

    [<TestMethod>] [<Timeout(20000)>]
    member this.``Zufallstest`` (): unit =
        Check.QuickThrowOnFailure(fun (actions: Action<int> list) ->
            let rec h (actions: Action<int> list) (referenceQueue: Map<Nat, int>) (actualQueue: MapTree.MapTree<Nat, int>): unit =
                match actions with
                | [] -> ()
                | IsEmpty::rest ->
                    Assert.AreEqual(Map.isEmpty referenceQueue, MapTree.isEmpty actualQueue)
                    h rest referenceQueue actualQueue
                | Add(k, v)::rest ->
                    let new_referenceQueue = referenceQueue.Add(k, v)
                    let new_actualQueue = MapTree.add (k, v) actualQueue
                    Assert.IsTrue(isValidTree(new_actualQueue))
                    h rest new_referenceQueue new_actualQueue
                | (TryFind k)::rest ->
                    Assert.AreEqual(referenceQueue.TryFind k, MapTree.tryFind k actualQueue)
                    h rest referenceQueue actualQueue
                | (Delete k)::rest ->
                    let new_referenceQueue = referenceQueue.Remove(k)
                    let new_actualQueue = MapTree.delete k actualQueue
                    Assert.IsTrue(isValidTree(new_actualQueue))
                    h rest new_referenceQueue new_actualQueue
            h actions Map.empty None
        )

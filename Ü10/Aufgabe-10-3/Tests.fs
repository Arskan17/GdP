module Tests

open Mini
open Types
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck


type ArbitraryModifiers =
    static member Nat() =
        Arb.from<bigint>
        |> Arb.filter (fun i -> i >= 0I)
        |> Arb.convert (Nat.Make) (fun n -> n.ToBigInteger())


let swap<'a> (ar: Array<'a>) (i: Int, j: Int): Array<'a> =
    Array.permute (fun x -> if x = i then j elif x = j then i else x) ar

let expectException (name: string) (f: unit -> unit): unit =
    try
        f()
        Assert.Fail (sprintf "Es wurde keine %s Ausnahme ausgelöst!" name)
    with
    | :? AssertFailedException -> reraise()
    | e ->
        let eType = e.GetType()
        if eType.FullName <> "Types+"+name then
            if eType.FullName.StartsWith "BibExceptions+" then
                Assert.AreEqual(name, eType.Name, "Es wurde eine falsche Ausnahme ausgelöst!")
            else failwithf "Es wurde eine %s ausgelöst statt einer eigenen %s Ausname!" eType.FullName name

let ex = [1..5]

[<TestClass>]
type Tests() =
    do Arb.register<ArbitraryModifiers>() |> ignore

    [<TestMethod>] [<Timeout(1000)>]
    member this.``a) tryFindLast Beispiele`` (): unit =
        Assert.AreEqual(Some 4, Find.tryFindLast (fun x -> x % 2 = 0) ex)
        Assert.AreEqual(None, Find.tryFindLast (fun x -> false) ex)
        Assert.AreEqual(None, Find.tryFindLast (fun x -> true) [])

    [<TestMethod>] [<Timeout(10000)>]
    member this.``a) tryFindLast Zufallstests`` (): unit =
        Check.QuickThrowOnFailure (fun (pred: Nat -> Bool) (xs: List<Nat>) ->
            let expected = List.tryFindBack pred xs
            let actual = Find.tryFindLast pred xs
            Assert.AreEqual(expected, actual)
        )


    [<TestMethod>] [<Timeout(1000)>]
    member this.``b) findLast Beispiele`` (): unit =
        Assert.AreEqual(4, Find.findLast (fun x -> x % 2 = 0) ex)
        expectException "NotFound" (fun () -> Find.findLast (fun x -> false) ex |> ignore)
        expectException "NotFound" (fun () -> Find.findLast (fun x -> true) [] |> ignore)

    [<TestMethod>] [<Timeout(10000)>]
    member this.``b) findLast Zufallstests`` (): unit =
        Check.QuickThrowOnFailure (fun (pred: Nat -> Bool) (xs: List<Nat>) ->
            let expected = List.tryFindBack pred xs
            match expected with
            | None ->
                expectException "NotFound" (fun () -> Find.findLast pred xs |> ignore)
            | Some e ->
                let actual = Find.findLast pred xs
                Assert.AreEqual(e, actual)
        )


    [<TestMethod>] [<Timeout(1000)>]
    member this.``c) tryFindLast2 Beispiele`` (): unit =
        Assert.AreEqual(Some 4, Find.tryFindLast2 (fun x -> x % 2 = 0) ex)
        Assert.AreEqual(None, Find.tryFindLast2 (fun x -> false) ex)
        Assert.AreEqual(None, Find.tryFindLast2 (fun x -> true) [])

    [<TestMethod>] [<Timeout(10000)>]
    member this.``c) tryFindLast2 Zufallstests`` (): unit =
        Check.QuickThrowOnFailure (fun (pred: Nat -> Bool) (xs: List<Nat>) ->
            let expected = List.tryFindBack pred xs
            let actual = Find.tryFindLast2 pred xs
            Assert.AreEqual(expected, actual)
        )


    [<TestMethod>] [<Timeout(1000)>]
    member this.``d) findLast Beispiele`` (): unit =
        Assert.AreEqual(4, Find.findLast2 (fun x -> x % 2 = 0) ex)
        expectException "NotFound" (fun () -> Find.findLast2 (fun x -> false) ex |> ignore)
        expectException "NotFound" (fun () -> Find.findLast2 (fun x -> true) [] |> ignore)

    [<TestMethod>] [<Timeout(10000)>]
    member this.``d) findLast Zufallstests`` (): unit =
        Check.QuickThrowOnFailure (fun (pred: Nat -> Bool) (xs: List<Nat>) ->
            let expected = List.tryFindBack pred xs
            match expected with
            | None ->
                expectException "NotFound" (fun () -> Find.findLast2 pred xs |> ignore)
            | Some e ->
                let actual = Find.findLast2 pred xs
                Assert.AreEqual(e, actual)
        )

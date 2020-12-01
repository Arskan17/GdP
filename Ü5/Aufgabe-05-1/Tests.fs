module Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck
open Swensen.Unquote
open Mini

type ArbitraryModifiers =
    static member Nat() =
        Arb.from<bigint>
        |> Arb.filter (fun i -> i >= 0I)
        |> Arb.convert (Nat.Make) (fun n -> n.ToBigInteger())

let config = {
    Config.QuickThrowOnFailure with
        EndSize = 1000
        MaxTest = 1000
    }

let ex = [2N; 4N; 3N; 4N; 2N; 1N]

[<TestClass>]
type Tests() =
    do Arb.register<ArbitraryModifiers>() |> ignore

    [<TestMethod>] [<Timeout(1000)>]
    member this.``a) concat Beispiele`` (): unit =
        test <@ Lists.concat [] ex = ex @>
        test <@ Lists.concat ex [] = ex @>
        test <@ Lists.concat [1N] [2N] = [1N; 2N] @>

    [<TestMethod>] [<Timeout(5000)>]
    member this.``a) concat Zufallstest`` (): unit =
        Check.QuickThrowOnFailure(fun (xs: List<Nat>) (ys: List<Nat>) ->
            Assert.AreEqual(
                xs @ ys,
                Lists.concat xs ys
            )
        )

    [<TestMethod>] [<Timeout(1000)>]
    member this.``b) mirror Beispiele`` (): unit =
        test <@ Lists.mirror [] = [] @>
        test <@ Lists.mirror ex  = [1N; 2N; 4N; 3N; 4N; 2N] @>


    [<TestMethod>] [<Timeout(5000)>]
    member this.``b) mirror Zufallstest`` (): unit =
        Check.QuickThrowOnFailure(fun (xs: List<Nat>) ->
            Assert.AreEqual(
                List.rev xs,
                Lists.mirror xs
            )
        )

    [<TestMethod>] [<Timeout(1000)>]
    member this.``c) digits Beispiele`` (): unit =
        test <@ Lists.digits 0N = [] @>
        test <@ Lists.digits 123N = [3N; 2N; 1N] @>

    [<TestMethod>] [<Timeout(5000)>]
    member this.``c) digits Zufallstest`` (): unit =
        Check.One(config, fun (x: Nat) ->
            let expected =
                if x = 0N then []
                else x.ToString() |> Seq.map (fun c -> int c - int '0' |> Nat.Make) |> Seq.rev |> Seq.toList
            Assert.AreEqual(
                expected,
                Lists.digits x
            )
        )

    [<TestMethod>] [<Timeout(1000)>]
    member this.``d) sum Beispiele`` (): unit =
        test <@ Lists.sum [] =  0N @>
        test <@ Lists.sum ex  = 16N @>


    [<TestMethod>] [<Timeout(5000)>]
    member this.``d) sum Zufallstest`` (): unit =
        Check.QuickThrowOnFailure(fun (xs: List<Nat>) ->
            Assert.AreEqual(
                List.sum xs,
                Lists.sum xs
            )
        )

    [<TestMethod>] [<Timeout(1000)>]
    member this.``e) digitSum Beispiele`` (): unit =
        test <@ Lists.digitSum  123N = 6N @>
        test <@ Lists.digitSum  220N = 4N @>
        test <@ Lists.digitSum 4711N = 13N @>
        test <@ Lists.digitSum  815N = 14N @>


    [<TestMethod>] [<Timeout(5000)>]
    member this.``e) digitSum Zufallstest`` (): unit =
        Check.One(config, fun (x: Nat) ->
            let expected =
                if x = 0N then 0N
                else x.ToString() |> Seq.map (fun c -> int c - int '0' |> Nat.Make) |> Seq.rev |> Seq.toList |> List.sum
            Assert.AreEqual(
                expected,
                Lists.digitSum x
            )
        )

    [<TestMethod>] [<Timeout(1000)>]
    member this.``f) notUntil Beispiele`` (): unit =
        test <@ Lists.notUntil 0N ex = 6N @>
        test <@ Lists.notUntil 1N ex = 5N @>
        test <@ Lists.notUntil 2N ex = 0N @>
        test <@ Lists.notUntil 3N ex = 2N @>

    [<TestMethod>] [<Timeout(5000)>]
    member this.``f) notUntil Zufallstest`` (): unit =
        Check.One(config, fun (x: Nat) (xs: List<Nat>) ->
            let expected =
                match (List.fold (fun (count, s) t -> if t = x then (count+1,count::s) else (count+1,s) ) (0, []) xs |> snd |> List.rev |> List.map Nat.Make) with
                | [] -> xs |> List.length |> Nat.Make
                | y::_ -> y
            Assert.AreEqual(
                expected,
                Lists.notUntil x xs
            )
        )

    [<TestMethod>] [<Timeout(1000)>]
    member this.``g) map Beispiele`` (): unit =
        test <@ Lists.map (fun x -> x + 1N) [] = [] @>
        test <@ Lists.map (fun x -> x + 1N) ex = [3N; 5N; 4N; 5N; 3N; 2N] @>

    [<TestMethod>] [<Timeout(5000)>]
    member this.``g) map Zufallstest`` (): unit =
        Check.One(config, fun (f: Nat -> Nat) (xs: List<Nat>) ->
            Assert.AreEqual(
                List.map f xs,
                Lists.map f xs
            )
        )

    [<TestMethod>] [<Timeout(1000)>]
    member this.``h) findAll Beispiele`` (): unit =
        test <@ Lists.findAll 10N ex = [] @>
        test <@ Lists.findAll  2N ex = [0N; 4N] @>

    [<TestMethod>] [<Timeout(5000)>]
    member this.``h) findAll Zufallstest`` (): unit =
        Check.One(config, fun (x: Nat) (xs: List<Nat>) ->
            let expected = List.fold (fun (count, s) t -> if t = x then (count+1,count::s) else (count+1,s) ) (0, []) xs |> snd |> List.rev |> List.map Nat.Make
            Assert.AreEqual(
                expected,
                Lists.findAll x xs
            )
        )

    [<TestMethod>] [<Timeout(1000)>]
    member this.``i) findFirst Beispiele`` (): unit =
        test <@ Lists.findFirst (fun x -> x > 2N) ex = Some 4N @>
        test <@ Lists.findFirst (fun x -> x > 4N) ex = None @>

    [<TestMethod>] [<Timeout(5000)>]
    member this.``i) findFirst Zufallstest`` (): unit =
        Check.One(config, fun (f: Nat -> Bool) (xs: List<Nat>) ->
            Assert.AreEqual(
                List.tryFind f xs,
                Lists.findFirst f xs
            )
        )

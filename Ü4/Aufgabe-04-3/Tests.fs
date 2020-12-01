module Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck
open Swensen.Unquote
open Mini
open NatsType

type ArbitraryModifiers =
    static member Nat() =
        Arb.from<bigint>
        |> Arb.filter (fun i -> i >= 0I)
        |> Arb.convert (Nat.Make) (fun n -> n.ToBigInteger())

let rec toList (xs: Nats): Nat list =
    match xs with
    | Nil -> []
    | Cons (x, ys) -> x::toList ys

let rec fromList (xs: Nat list): Nats =
    match xs with
    | [] -> Nil
    | x::ys -> Cons (x, fromList ys)

let config = {
    Config.QuickThrowOnFailure with
        EndSize = 10000
        MaxTest = 1000
    }

let ex = Cons (2N, Cons (4N, Cons (3N, Cons(4N, Cons(2N, Cons (1N, Nil))))))

[<TestClass>]
type Tests() =
    do Arb.register<ArbitraryModifiers>() |> ignore

    [<TestMethod>] [<Timeout(1000)>]
    member this.``a) concat Beispiele`` (): unit =
        test <@ Nats.concat Nil ex = ex @>
        test <@ Nats.concat ex Nil = ex @>
        test <@ Nats.concat (Cons (1N, Nil)) (Cons (2N, Nil)) = Cons (1N,Cons (2N,Nil)) @>

    [<TestMethod>] [<Timeout(1000)>]
    member this.``a) concat Zufallstest`` (): unit =
        Check.QuickThrowOnFailure(fun (xs: Nats) (ys: Nats) ->
            Assert.AreEqual(
                (toList xs) @ (toList ys) |> fromList,
                Nats.concat xs ys
            )
        )

    [<TestMethod>] [<Timeout(1000)>]
    member this.``b) mirror Beispiele`` (): unit =
        test <@ Nats.mirror Nil = Nil @>
        test <@ Nats.mirror ex  = Cons (1N,Cons (2N,Cons (4N,Cons (3N,Cons (4N,Cons (2N,Nil)))))) @>


    [<TestMethod>] [<Timeout(1000)>]
    member this.``b) mirror Zufallstest`` (): unit =
        Check.QuickThrowOnFailure(fun (xs: Nats) ->
            Assert.AreEqual(
                List.rev (toList xs) |> fromList,
                Nats.mirror xs
            )
        )

    [<TestMethod>] [<Timeout(1000)>]
    member this.``c) digits Beispiele`` (): unit =
        test <@ Nats.digits 0N = Nil @>
        test <@ Nats.digits 123N = Cons (3N, Cons (2N, Cons (1N, Nil))) @>

    [<TestMethod>] [<Timeout(1000)>]
    member this.``c) digits Zufallstest`` (): unit =
        Check.One(config, fun (x: Nat) ->
            let expected =
                if x = 0N then Nil
                else x.ToString() |> Seq.map (fun c -> int c - int '0' |> Nat.Make) |> Seq.rev |> Seq.toList |> fromList
            Assert.AreEqual(
                expected,
                Nats.digits x
            )
        )

    [<TestMethod>] [<Timeout(1000)>]
    member this.``d) sum Beispiele`` (): unit =
        test <@ Nats.sum Nil =  0N @>
        test <@ Nats.sum ex  = 16N @>


    [<TestMethod>] [<Timeout(1000)>]
    member this.``d) sum Zufallstest`` (): unit =
        Check.QuickThrowOnFailure(fun (xs: Nats) ->
            Assert.AreEqual(
                List.sum (toList xs),
                Nats.sum xs
            )
        )
    
    [<TestMethod>] [<Timeout(1000)>]
    member this.``e) digitSum Beispiele`` (): unit =
        test <@ Nats.digitSum  123N = 6N @>
        test <@ Nats.digitSum  220N = 4N @>
        test <@ Nats.digitSum 4711N = 13N @>
        test <@ Nats.digitSum  815N = 14N @>


    [<TestMethod>] [<Timeout(1000)>]
    member this.``e) digitSum Zufallstest`` (): unit =
        Check.One(config, fun (x: Nat) ->
            let expected =
                if x = 0N then 0N
                else x.ToString() |> Seq.map (fun c -> int c - int '0' |> Nat.Make) |> Seq.rev |> Seq.toList |> List.sum
            Assert.AreEqual(
                expected,
                Nats.digitSum x
            )
        )

    [<TestMethod>] [<Timeout(1000)>]
    member this.``f) notUntil Beispiele`` (): unit =
        test <@ Nats.notUntil 0N ex = 6N @>
        test <@ Nats.notUntil 1N ex = 5N @>
        test <@ Nats.notUntil 2N ex = 0N @>
        test <@ Nats.notUntil 3N ex = 2N @>

    [<TestMethod>] [<Timeout(1000)>]
    member this.``f) notUntil Zufallstest`` (): unit =
        Check.One(config, fun (x: Nat) (xs: Nats) ->
            let expected =
                match (List.fold (fun (count, s) t -> if t = x then (count+1,count::s) else (count+1,s) ) (0, []) (toList xs) |> snd |> List.rev |> List.map Nat.Make |> fromList) with
                | Nil -> xs |> toList |> List.length |> Nat.Make
                | Cons (y, _) -> y
            Assert.AreEqual(
                expected,
                Nats.notUntil x xs
            )
        )

    [<TestMethod>] [<Timeout(1000)>]
    member this.``g) map Beispiele`` (): unit =
        test <@ Nats.map (fun x -> x+1N) Nil = Nil @>
        test <@ Nats.map (fun x -> x + 1N) ex = Cons (3N,Cons (5N,Cons (4N,Cons (5N,Cons (3N,Cons (2N,Nil)))))) @>

    [<TestMethod>] [<Timeout(1000)>]
    member this.``g) map Zufallstest`` (): unit =
        Check.One(config, fun (f: Nat -> Nat) (xs: Nats) ->
            let expected = xs |> toList |> List.map f |> fromList
            Assert.AreEqual(
                expected,
                Nats.map f xs
            )
        )

    [<TestMethod>] [<Timeout(1000)>]
    member this.``h) findAll Beispiele`` (): unit =
        test <@ Nats.findAll 10N ex = Nil @>
        test <@ Nats.findAll 2N ex = Cons (0N,Cons (4N,Nil)) @>

    [<TestMethod>] [<Timeout(1000)>]
    member this.``h) findAll Zufallstest`` (): unit =
        Check.One(config, fun (x: Nat) (xs: Nats) ->
            let expected = List.fold (fun (count, s) t -> if t = x then (count+1,count::s) else (count+1,s) ) (0, []) (toList xs) |> snd |> List.rev |> List.map Nat.Make |> fromList
            Assert.AreEqual(
                expected,
                Nats.findAll x xs
            )
        )

    [<TestMethod>] [<Timeout(1000)>]
    member this.``i) findFirst Beispiele`` (): unit =
        test <@ Nats.findFirst (fun x -> x > 2N) ex = SomeNat 4N @>
        test <@ Nats.findFirst (fun x -> x > 4N) ex = NoNat @>

    [<TestMethod>] [<Timeout(1000)>]
    member this.``i) findFirst Zufallstest`` (): unit =
        Check.One(config, fun (f: Nat -> Bool) (xs: Nats) ->
            let found = xs |> toList |> List.tryFind f
            let expected =
                match found with
                | None -> NoNat
                | Some x -> SomeNat x
            Assert.AreEqual(
                expected,
                Nats.findFirst f xs
            )
        )

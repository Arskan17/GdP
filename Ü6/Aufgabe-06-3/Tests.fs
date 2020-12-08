module Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck
open Mini
open Types

type ArbitraryModifiers =
    static member Nat() =
        Arb.from<bigint>
        |> Arb.filter (fun i -> i >= 0I)
        |> Arb.convert (Nat.Make) (fun n -> n.ToBigInteger())

let seeds = [(1591000511,296671646); (1893282681,296671730); (1551215629,296671507); (180184836,296671692)]

let ex = [
    ( Const 1N, (fun (x: Nat) -> 1N), fun (x: Nat) -> 0N)
  ; ( Id, (fun (x: Nat) -> x), fun (x: Nat) -> 1N)
  ; ( Add (Id, Const 2N), (fun (x: Nat) -> x + 2N), fun (x: Nat) -> 1N)
  ; ( Mul (Id, Const 2N), (fun (x: Nat) -> x * 2N), fun (x: Nat) -> 2N)
  ; ( Add (Mul(Id, Id), Mul(Const 2N, Id))
    , (fun (x: Nat) -> x ** 2N + 2N * x)
    , (fun (x: Nat) -> 2N * x + 2N) )
  ; ( Mul(Comp(Pow(Id, 2N), Add(Id, Const 3N)), Add(Id, Const 4N) )
    , (fun (x: Nat) -> (x + 3N) ** 2N *(x + 4N))
    , (fun (x: Nat) -> 2N * (x + 3N) * (x + 4N) + (x + 3N) ** 2N) )
  ]

[<TestClass>]
type Tests() =
    do Arb.register<ArbitraryModifiers>() |> ignore

    [<TestMethod>] [<Timeout(10000)>]
    member this.``a) Beispiele apply`` (): unit =
        Check.QuickThrowOnFailure(fun (n: Nat) ->
            for func, f, _ in ex do
                let expected = f n
                let actual = Calculus.apply func n
                Assert.AreEqual(expected, actual, sprintf "f(x)=%A evaluated at %A" func n)
        )

    [<TestMethod>] [<Timeout(10000)>]
    member this.``b) Beispiele derive (setzt voraus, dass apply funktioniert)`` (): unit =
        Check.QuickThrowOnFailure(fun (n: Nat) ->
            for func, f, f' in ex do
                let expected = f' n
                let func_deriv = Calculus.derive func
                let actual = Calculus.apply func_deriv  n
                Assert.AreEqual(expected, actual, sprintf "From f(x)=%A you derived f'(x)=%A which is not correct at %A" func func_deriv n)
        )

    [<TestMethod>] [<Timeout(10000)>]
    member this.``c) Freiwillige Zusazuaufgabe: Beispiele simplify (setzt voraus, dass apply funktioniert)`` (): unit =
        Check.QuickThrowOnFailure(fun (n: Nat) ->
            for func, f, _ in ex do
                let expected = f n
                let func_simp = Calculus.simplify func
                let actual = Calculus.apply func_simp n
                Assert.AreEqual(expected, actual, sprintf "Original form f(x)=%A and simplification %A are not equal at %A" func func_simp n)
        )

    [<TestMethod>] [<Timeout(30000)>]
    member this.``c) Freiwillige Zusatzaufgabe: Zufallstest simplify (setzt voraus, dass apply funktioniert)`` (): unit =
        for seed in seeds do
            Check.One({Config.QuickThrowOnFailure with Replay = Some <| Random.StdGen seed}, fun (func: Function) (n: Nat) ->
                let expected = Calculus.apply func n
                let func_simp = Calculus.simplify func
                let actual = Calculus.apply func_simp n
                Assert.AreEqual(expected, actual, sprintf "Original form f(x)=%A and simplification %A are not equal at %A" func func_simp n)
            )

module Tests

open Mini
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck


type ArbitraryModifiers =
    static member Nat() =
        Arb.from<bigint>
        |> Arb.filter (fun i -> i >= 0I)
        |> Arb.convert (Nat.Make) (fun n -> n.ToBigInteger())

type Action = | I | R

[<TestClass>]
type Tests() =
    do Arb.register<ArbitraryModifiers>() |> ignore

    [<TestMethod>] [<Timeout(10000)>]
    member this.``Teil a Zufallstests`` (): unit =
        Check.QuickThrowOnFailure (fun (actions: Action list) ->
            let mutable value = 0N
            Counters.reset()
            for action in actions do
                match action with
                | I ->
                    value <- value + 1N
                    Counters.increment()
                | R ->
                    value <- 0N
                    Counters.reset()
                Assert.AreEqual(
                    value,
                    Counters.get()
                )
        )

    [<TestMethod>] [<Timeout(10000)>]
    member this.``Teil b Zufallstests`` (): unit =
        Check.QuickThrowOnFailure (fun (actions: (Action * Nat) list) ->
            if not <| List.isEmpty actions then
                let maxCounter = actions |> List.map snd |> List.max |> int
                let counters = [| for _ in 0..maxCounter -> Counters.create() |]
                let values = [| for _ in 0..maxCounter -> 0N |]
                for (action, i) in actions do
                    let i = int i
                    match action with
                    | I ->
                        values.[i] <- values.[i] + 1N
                        Counters.increment2(counters.[i])
                    | R ->
                        values.[i] <- 0N
                        Counters.reset2(counters.[i])
                    for j in 0..maxCounter do
                        Assert.AreEqual(
                            values.[j],
                            Counters.get2(counters.[j]),
                            sprintf "Zähler %i hat den falschen Wert!" j
                        )
        )

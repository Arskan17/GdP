module Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck
open Swensen.Unquote
open Mini
open Types

type ArbitraryModifiers =
    static member Nat() =
        Arb.from<bigint>
        |> Arb.filter (fun i -> i >= 0I)
        |> Arb.convert (Nat.Make) (fun n -> n.ToBigInteger())

let isValid (xs: Input list): Bool =
    let rec h (xs: Input list) (s: Nat): Bool =
        match xs with
        | [] -> s = 1N
        | INum _::xs' -> h xs' (s + 1N)
        | _::xs' -> s >= 2N && h xs' (s - 1N)
    h (List.rev xs) 0N


let rec preorder (expr: Expr): Input list =
    match expr with
    | Num x -> [INum x]
    | Add (left, right) -> Plus :: preorder left @ preorder right
    | Mul (left, right) -> Star :: preorder left @ preorder right

[<TestClass>]
type Tests() =
    do Arb.register<ArbitraryModifiers>() |> ignore

    [<TestMethod>] [<Timeout(20000)>]
    member this.``a) Gültige Eingaben`` (): unit =
        Check.QuickThrowOnFailure(fun (expr: Expr) ->
            let xs = preorder expr
            match Prefix.parse xs with
            | None -> Assert.Fail(sprintf "Die Eingabe %A wurde nicht richtig eingelesen!\nExpected: %A\nActual: None" xs (Some expr))
            | r -> Assert.AreEqual(Some expr, r, sprintf "Die Eingabe %A wurde nicht richtig eingelesen!" xs)
        )

    [<TestMethod>] [<Timeout(20000)>]
    member this.``a) Ungültige Eingaben`` (): unit =
        Check.QuickThrowOnFailure(fun (xs: Input list) ->
            if not (isValid xs) then
                match Prefix.parse xs with
                | None -> ()
                | _ -> Assert.Fail(sprintf "Die Eingabe %A wurde nicht als ungültig erkannt!" xs)
        )

    [<TestMethod>] [<Timeout(20000)>]
    member this.``b) Beispiele`` (): unit =
        let ex1 = Mul (Num 4N,Add (Num 3N,Num 2N))
        let ex2 = Mul (Mul (Num 4N,Num 1N),Add (Num 3N,Num 2N))
        let ex3 = Mul (Add (Num 4N,Add (Num 2N,Num 1N)),Add (Num 3N,Num 2N))

        test <@ Prefix.eval ex1 = 20N @>
        test <@ Prefix.eval ex2 = 20N @>
        test <@ Prefix.eval ex3 = 35N @>


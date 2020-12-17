module Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open System.Text.RegularExpressions
open FsCheck
open Mini
open Types

[<TestClass>]
type Tests() =
    // ------------------------------------------------------------------------
    // c)

    [<TestMethod>] [<Timeout(1000)>]
    member this.``c) accept Beispiel 1`` (): unit =
        Assert.IsTrue(RegExp.accept [A;C])

    [<TestMethod>] [<Timeout(1000)>]
    member this.``c) accept Beispiel 2`` (): unit =
        Assert.IsTrue(RegExp.accept [A;A;B;C;A;B])

    [<TestMethod>] [<Timeout(1000)>]
    member this.``c) accept Beispiel 3`` (): unit =
        Assert.IsTrue(RegExp.accept [A;C;A;B;A;B])


    [<TestMethod>] [<Timeout(1000)>]
    member this.``c) accept Gegenbeispiel 1`` (): unit =
        Assert.IsFalse(RegExp.accept [])

    [<TestMethod>] [<Timeout(1000)>]
    member this.``c) accept Gegenbeispiel 2`` (): unit =
        Assert.IsFalse(RegExp.accept [C])

    [<TestMethod>] [<Timeout(1000)>]
    member this.``c) accept Gegenbeispiel 3`` (): unit =
        Assert.IsFalse(RegExp.accept [A;C;B])

    [<TestMethod>] [<Timeout(1000)>]
    member this.``c) accept Gegenbeispiel 4`` (): unit =
        Assert.IsFalse(RegExp.accept [A;C;A;B;A])


    [<TestMethod>] [<Timeout(10000)>]
    member this.``c) accept Zufall`` (): unit =
        Check.One({Config.QuickThrowOnFailure with EndSize = 100}, fun (input: Alphabet list) ->
            let rec toString (acc: String) (xs: Alphabet list): String =
                match xs with
                | [] -> acc
                | A::rest -> toString (acc + "a") rest
                | B::rest -> toString (acc + "b") rest
                | C::rest -> toString (acc + "c") rest
            let inputStr = toString "" input
            let m = Regex.Match(inputStr, "(a|b)(a|b)*c(ab)*")
            Assert.AreEqual(m.Success && m.Value = inputStr, RegExp.accept input)
        )

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


[<TestClass>]
type Tests() =
    do Arb.register<ArbitraryModifiers>() |> ignore

    [<TestMethod>] [<Timeout(20000)>]
    member this.``a) Beispiele`` (): unit =
        test <@ Mensa.woGibtEsHeutePommes (Plan.archiv 2008N 16N Mo) = [A2] @>
        test <@ Mensa.woGibtEsHeutePommes (Plan.archiv 2008N 18N Di) = [] @>
        test <@ Mensa.woGibtEsHeutePommes (Plan.archiv 2008N 21N Mi) = [A1; Grill] @>
        test <@ Mensa.woGibtEsHeutePommes (Plan.archiv 2020N 25N Di) = [A1; A1] @>

    [<TestMethod>] [<Timeout(20000)>]
    member this.``b) Beispiele`` (): unit =
        test <@ Mensa.gibtEsHeute "Dip" (Plan.archiv 2008N 20N Do) = true @>
        test <@ Mensa.gibtEsHeute "Dip" (Plan.archiv 2008N 20N Mi) = false @>
        test <@ Mensa.gibtEsHeute "Mandeln" (Plan.archiv 2008N 20N Mi) = true @>
        test <@ Mensa.gibtEsHeute "Mandeln" (Plan.archiv 2008N 20N Do) = false @>
        test <@ Mensa.gibtEsHeute "Apfelmus" (Plan.archiv 2008N 23N Do) = true @>
        test <@ Mensa.gibtEsHeute "Apfelmus" (Plan.archiv 2008N 23N Fr) = false @>

    [<TestMethod>] [<Timeout(20000)>]
    member this.``c) Beispiele`` (): unit =
        test <@ Mensa.nurVegetarisch (Plan.archiv 2008N 23N Do) = [] @>
        test <@ Mensa.nurVegetarisch (Plan.archiv 2020N 25N Di) = [(A1, {beschreibung="Gebackener griechischer Hirtenkäse  mit Tzatziki, Pommes frites  und Krautsalat"; istVegetarisch=true; preis=265N})] @>
        test <@ Mensa.nurVegetarisch (Plan.archiv 2008N 24N Fr) |> List.map fst |> List.sort = [A1; A2] @>

    [<TestMethod>] [<Timeout(20000)>]
    member this.``d) Beispiele`` (): unit =
        test <@ Mensa.pommesVerteilung Plan.archiv |> List.sortBy fst = [(Mo, 419N); (Di, 387N); (Mi, 399N); (Do, 378N); (Fr, 276N)] @>

    [<TestMethod>] [<Timeout(20000)>]
    member this.``e) Beispiele`` (): unit =
        test <@ Mensa.pommestag Plan.archiv = Mo @>

    [<TestMethod>] [<Timeout(20000)>]
    member this.``f) Beispiele`` (): unit =
        test <@ Mensa.pommeswochen Plan.archiv |> List.sort =
                    [(2008N, 45N); (2009N, 22N); (2009N, 41N); (2009N, 49N); (2010N, 4N);
                     (2010N, 17N); (2010N, 21N); (2010N, 27N); (2011N, 4N); (2011N, 20N);
                     (2011N, 27N); (2011N, 44N); (2011N, 46N); (2012N, 9N); (2012N, 16N);
                     (2012N, 19N); (2012N, 27N); (2012N, 43N); (2012N, 45N); (2013N, 9N);
                     (2013N, 26N); (2013N, 43N); (2013N, 45N); (2013N, 51N); (2014N, 29N);
                     (2014N, 45N); (2019N, 19N); (2019N, 20N); (2019N, 27N)] @>

    [<TestMethod>] [<Timeout(20000)>]
    member this.``g) Beispiele`` (): unit =
        test <@ Mensa.mittlererPreis A1 Plan.archiv = 246N @>
        test <@ Mensa.mittlererPreis A2 Plan.archiv = 220N @>
        test <@ Mensa.mittlererPreis Grill Plan.archiv = 331N @>
        test <@ Mensa.mittlererPreis Wok Plan.archiv = 99N @>

    [<TestMethod>] [<Timeout(20000)>]
    member this.``h) Beispiele`` (): unit =
        test <@ let e = Mensa.teuerstesEssenBeimGrill Plan.archiv in e.preis = 720N @>
        test <@ let e = Mensa.teuerstesEssenBeimGrill Plan.archiv in e.beschreibung.Contains "Rumpsteak" = true @>

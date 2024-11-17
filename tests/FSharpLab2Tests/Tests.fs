module AVLTest

open System
open Xunit
open FsCheck.Xunit
open AVL
open System.Collections.Generic

[<Fact>]
let ``My test`` () = Assert.True(true)

[<Fact>]
let ``Contain test`` () =
    Assert.True(
        AVLBag.empty
        |> AVLBag.add 1
        |> AVLBag.add 2
        |> AVLBag.add 10
        |> AVLBag.add 5
        |> AVLBag.tryGet 5 =
            1
    )

[<Fact>]
let ``Multiple contain test`` () =
    Assert.True(
        AVLBag.empty
        |> AVLBag.add 1
        |> AVLBag.add 2
        |> AVLBag.add 10
        |> AVLBag.add 1
        |> AVLBag.tryGet 1 =
            2
    )

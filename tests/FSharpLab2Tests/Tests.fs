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

let generateRandomArray (size: int) =
    let random = Random()
    [| for _ in 1..size -> random.Next(1, 101) |]

[<Fact>]
let ``Map Test`` () =
    let data = generateRandomArray 100
    let tree = (data |> AVLBag.ofItems)
    let mappedTree = (tree |> AVLBag.map (fun x -> x * 2))
    let expectedTree = (data |> Array.map (fun x -> x * 2) |> AVLBag.ofItems)
    Assert.True(mappedTree.Equals(expectedTree))

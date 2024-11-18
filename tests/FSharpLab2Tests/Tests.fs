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

[<Fact>]
let ``Equal+remove Test`` () =
    let data = generateRandomArray 10000
    let tree = (data |> AVLBag.ofItems)
    Assert.True(tree.Equals(tree))
    Assert.False(tree.Equals(tree.Remove(data[0])))

let generateRandomString length =
    let random = Random()
    let chars = "abcdefghijklmnopqrstuvwxyz" //
    let charArray = Array.init length (fun _ -> chars.[random.Next(chars.Length)])
    string (charArray)

let generateRandomStringArray size stringLength =
    [| for _ in 1..size -> generateRandomString stringLength |] //

[<Fact>]
let ``String test`` () =
    let data = generateRandomStringArray 100 6
    let tree = (data |> AVLBag.ofItems)
    Assert.True(tree.Size = data.Length)

[<Fact>]
let ``Filter+merge test`` () =
    let data = generateRandomArray 1000
    let tree = (data |> AVLBag.ofItems)
    let chet = tree |> AVLBag.filter (fun v -> v % 2 = 0)
    let nechet = tree |> AVLBag.filter (fun v -> v % 2 = 1)
    let res1 = (chet |> AVLBag.merge (nechet)).Equals(tree)
    Assert.True(res1)
    let greater = tree |> AVLBag.filter (fun v -> v > 50)
    let less = tree |> AVLBag.filter (fun v -> v <= 50)
    let res2 = (greater |> AVLBag.merge (less)).Equals(tree)
    Assert.True(res2)

[<Property>]
let ``Diff is -1, 0, 1 after creation`` =
    let data = generateRandomArray 1000
    let tree = (data |> AVLBag.ofItems)
    [ -1; 0; 1 ] |> List.contains tree.MaxDelta

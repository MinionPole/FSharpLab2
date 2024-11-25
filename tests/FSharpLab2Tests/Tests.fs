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

let createArray size =
    let arr = Array.zeroCreate size

    for i in 0 .. size - 1 do
        if i % 5 = 1 && i > 0 then
            arr.[i] <- arr.[i - 1]
        else
            arr.[i] <- i

    arr

let funcData = createArray 1000

[<Fact>]
let ``Map Test`` () =
    let data = funcData
    let tree = (data |> AVLBag.ofItems)
    let mappedTree = (tree |> AVLBag.map (fun x -> x * 2))
    let expectedTree = (data |> Array.map (fun x -> x * 2) |> AVLBag.ofItems)
    Assert.True(mappedTree.Equals(expectedTree))

[<Fact>]
let ``Equal+remove Test`` () =
    let data = funcData
    let tree = (data |> AVLBag.ofItems)
    Assert.True(tree.Equals(tree))
    Assert.True(tree.Size - 1 = tree.Remove(data[0]).Size)

[<Fact>]
let ``Filter+merge test`` () =
    let data = funcData
    let tree = (data |> AVLBag.ofItems)
    let chet = tree |> AVLBag.filter (fun v -> v % 2 = 0)
    let nechet = tree |> AVLBag.filter (fun v -> v % 2 = 1)
    let res1 = (chet |> AVLBag.merge (nechet)).Equals(tree)
    Assert.True(res1)
    let greater = tree |> AVLBag.filter (fun v -> v > 50)
    let less = tree |> AVLBag.filter (fun v -> v <= 50)
    let res2 = (greater |> AVLBag.merge (less)).Equals(tree)
    Assert.True(res2)

let generateRandomString length =
    let random = Random()
    let chars = "abcdefghijklmnopqrstuvwxyz"
    let charArray = Array.init length (fun _ -> chars.[random.Next(chars.Length)])
    string (charArray)

let generateRandomStringArray size stringLength =
    [| for _ in 1..size -> generateRandomString stringLength |]

let generateRandomArray (size: int) =
    let random = Random()
    [| for _ in 1..size -> random.Next(1, 101) |]

[<Property>]
let ``String test`` (l: string list) =
    let tree = (l |> AVLBag.ofItems)
    Assert.True(tree.Size = l.Length)

[<Property>]
let ``Diff is -1, 0, 1 after creation`` (l: int list) =
    let tree = (l |> AVLBag.ofItems)
    Assert.True([ -1; 0; 1 ] |> List.contains tree.MaxDelta)

[<Property>]
let ``neutral mono`` (l: int list) =
    let tree = (l |> AVLBag.ofItems)
    Assert.True((tree.Equals(AVLBag.merge tree AVLBag.empty)))
    Assert.True((tree.Equals(AVLBag.merge AVLBag.empty tree)))

[<Property>]
let ``Monoid associative`` (a: int list) (b: int list) (c: int list) =
    let aTree = a |> AVLBag.ofItems
    let bTree = b |> AVLBag.ofItems
    let cTree = c |> AVLBag.ofItems

    let lhs = AVLBag.merge aTree (AVLBag.merge bTree cTree)
    let rhs = AVLBag.merge (AVLBag.merge aTree bTree) cTree

    Assert.True((lhs = rhs))

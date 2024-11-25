module AVL

open System.Collections.Generic
open System.Collections
open System

type public Vertex<'V> =
    | Node of int * int * 'V * Vertex<'V> * Vertex<'V>
    | Nil

let public height node =
    match node with
    | Node(_, h, _, _, _) -> h
    | Nil -> 0

let public count node =
    match node with
    | Node(c, _, _, _, _) -> c
    | Nil -> 0

let public delta node =
    match node with
    | Node(_, _, _, l, r) -> height l - height r
    | Nil -> 0

let public createVertex v l r =
    Node(1 + count l + count r, 1 + max (height l) (height r), v, l, r)

let public balance node =
    let rotateLeft node =
        match node with
        | Node(_, _, v, l, Node(_, _, rv, rl, rr)) -> createVertex rv (createVertex v l rl) rr
        | v -> v

    let rotateRight node =
        match node with
        | Node(_, _, v, Node(_, _, lv, ll, lr), r) -> createVertex lv ll (createVertex v lr r)
        | v -> v

    let bigRotateLeft node =
        match node with
        | Node(_, _, v, l, r) -> //
            rotateLeft (createVertex v l (rotateRight r))
        | v -> v

    let bigRotateRight node =
        match node with
        | Node(_, _, v, l, r) -> //
            rotateRight (createVertex v (rotateLeft l) r)
        | v -> v

    match node with
    | Node(_, _, _, l, r) when delta node <= -2 ->
        if delta r = -1 then //
            rotateLeft node
        else
            bigRotateLeft node
    | Node(_, _, _, l, r) when delta node >= 2 ->
        if delta l = 1 then //
            rotateRight node
        else
            bigRotateRight node
    | v -> v

let rec public insert v node =
    match node with
    | Node(_, _, v', l', r') ->
        let l, r = if v <= v' then insert v l', r' else l', insert v r'
        createVertex v' l r |> balance
    | Nil -> createVertex v Nil Nil


let rec public remove v node =
    let rec extractLeast node =
        match node with
        | Node(_, _, v, Nil, r) -> r, v
        | Node(_, _, v, l, r) ->
            let remainder, leastV = extractLeast l
            createVertex v remainder r, leastV
        | Nil -> raise (System.ArgumentNullException())

    let deleteRoot node =
        match node with
        | Node(_, _, _, Nil, Nil) -> Nil
        | Node(_, _, _, Nil, r) -> r
        | Node(_, _, _, l, Nil) -> l
        | Node(_, _, _, l, r) ->
            let remainder, leastV = extractLeast r
            createVertex leastV l remainder
        | Nil -> raise (System.ArgumentNullException())

    match node with
    | Node(_, _, v', l', r') when v' = v -> deleteRoot node |> balance
    | Node(_, _, v', l', r') ->
        let l, r = if v < v' then remove v l', r' else l', remove v r'
        createVertex v' l r |> balance
    | Nil -> Nil

let rec public tryGet node v =
    match node with
    | Node(_, _, v', l', _) when v <= v' ->
        let nowVert = if v = v' then 1 else 0 // Подсчитываем совпадения
        nowVert + tryGet l' v
    | Node(_, _, v', _, r') when v > v' -> tryGet r' v
    | _ -> 0

let rec public treeSeq tree =
    seq {
        match tree with
        | Node(_, _, v, l, r) ->
            yield! treeSeq l
            yield v
            yield! treeSeq r
        | Nil -> ()
    }

let rec public treeSeqBack tree =
    seq {
        match tree with
        | Node(_, _, v, l, r) ->
            yield! treeSeqBack r
            yield v
            yield! treeSeqBack l
        | Nil -> ()
    }

let rec private map node f =
    match node with
    | Node(_, _, v, l, r) -> createVertex (f v) (map l f) (map r f)
    | Nil -> Nil

// для проверки факта в тестах, что дерево действительно сбалансированное
let rec private maxDelta node =
    match node with
    | Node(_, _, v, l, r) -> max (delta node) (max (maxDelta l) (maxDelta r))
    | Nil -> 0

let sumHash (sequence: seq<'T>) : int =
    sequence |> Seq.fold (fun acc x -> acc + hash x) 0

let allPairs seq1 seq2 =
    seq1 |> Seq.collect (fun item1 -> 
        seq2 |> Seq.map (fun item2 -> (item1, item2))
    )

type AVLBag<'Value when 'Value: comparison> private (root: Vertex<'Value>) =
    public new() = AVLBag(Nil)
    member _.Size = count root
    member _.Height = height root

    member _.TryGet = tryGet root

    member _.Add(v: 'Value) = AVLBag(insert v root)
    member _.Remove(v: 'Value) = AVLBag(remove v root)

    member _.Map f = AVLBag(map root f)

    member _.TreeSeq = treeSeq root
    member _.BackSeq = treeSeqBack root

    member _.MaxDelta = maxDelta root

    override this.Equals(other: obj) =
    if other :? AVLBag<'Value> then
        let otherBag = other :?> AVLBag<'Value>
        let thisElements = this.TreeSeq
        let otherElements = otherBag.TreeSeq
        if(Seq.length thisElements = Seq.length otherElements) then
          Seq.zip thisElements otherElements
            |> Seq.map (fun (v1, v2) -> v1 = v2)
            |> Seq.fold (&&) true
        else
          false
    else
        raise (System.Exception("The compared object is not an AVLBag.")) 

module AVLBag =
    [<GeneralizableValue>]
    let empty<'V when 'V: comparison> : AVLBag<'V> = AVLBag<'V>()

    let add v (tree: AVLBag<'V>) = tree.Add v

    let remove v (tree: AVLBag<'V>) = tree.Remove v

    let map f (tree: AVLBag<'V>) = tree.Map f

    let tryGet v (tree: AVLBag<'V>) = tree.TryGet v

    let ofItems (items: IEnumerable<'V>) =
        items |> Seq.fold (fun tree (v) -> tree |> add v) empty

    let merge (lhs: AVLBag<'V>) (rhs: AVLBag<'V>) =
        lhs.TreeSeq |> Seq.fold (fun tree (v) -> tree |> add v) rhs

    let filter pred (tree: AVLBag<'V>) =
        tree.TreeSeq |> Seq.filter pred |> ofItems

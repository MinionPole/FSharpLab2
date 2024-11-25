# FSharpLab2
## AVL_MultiSet
### Юнусов Роман Ильдарович

## Реализация дерева

Была создана структура дерева

```f#
type public Vertex<'V> =
    | Node of int * int * 'V * Vertex<'V> * Vertex<'V>
    | Nil
```

Далее были реализованы функции получения информации о дереве - высоты, размера поддерева, разницы высот и создания дерева. На их основе была сделана функция баланса
```f#
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
```

После баланса были сделаны требуемые функции
вставки и удаления элемента, удаляемый элемент только один
```f#
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
```

Получения количества элементов вида x
```f#
let rec public tryGet node v =
    match node with
    | Node(_, _, v', l', _) when v <= v' ->
        let nowVert = if v = v' then 1 else 0 // Подсчитываем совпадения
        nowVert + tryGet l' v
    | Node(_, _, v', _, r') when v > v' -> tryGet r' v
    | _ -> 0
```

Оба вида свертки
```f#
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
```

Оба вида свертки
```f#
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
```

И функцию мапа Мар
```f#
let rec private map node f =
    match node with
    | Node(_, _, v, l, r) -> createVertex (f v) (map l f) (map r f)
    | Nil -> Nil
```

Для удобного доступа к функциям, создадим класс "мешок" и отдельный модуль, для работы с классом
```f#
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
            let hash1 = sumHash (otherBag.TreeSeq)
            let hash2 = sumHash (this.TreeSeq)
            hash1 = hash2
        else
            false

```

```f#
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
```

## Тестирование
Тестирование разделено на две части, первая юнит, где на основе с повторяющимися данными мы пытаемся проверить корректность данных.

В юнит тестировании я сначала проверил корректность и возможность доступа к структуре на своём наборе данных
```f#
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
```
Потом сделал генерацию массива из 1000 элементов, который пихал в следующие юнит тесты
```f#
let createArray size =
    let arr = Array.zeroCreate size

    for i in 0 .. size - 1 do
        if i % 5 = 1 && i > 0 then
            arr.[i] <- arr.[i - 1]
        else
            arr.[i] <- i

    arr
```
сами тесты
```f#
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
```

В тестировании свойств, генерировался рандомный массив, либо из строк, либо из чисел.

```f#
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

```
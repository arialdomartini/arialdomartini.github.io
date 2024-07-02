---
layout: post
title: "State Monad For The Rest Of Us - Part 2"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- F#
- Functional Programming
include_in_index: false
---


```fsharp
type Tree<'a> =
    | Leaf of 'a
    | Node of Tree<'a> * Tree<'a>

let rec numberOfLeaves = function
    | Leaf _ -> 1
    | Node(l, r) -> numberOfLeaves l + numberOfLeaves r

[<Fact>]
let ``counts the number of leaves in a tree`` () =
    let tree = Node(Leaf (), Node(Leaf (), Leaf ()))

    let leaves = numberOfLeaves tree

    test <@ leaves = 3 @>
```


```fsharp
let rec lengths = function
    | Leaf _ -> failwith "Not yet implemented"
    | Node(l, r) -> failwith "Not yet implemented"

[<Fact>]
let ``counts the number of leaves in a tree`` () =
    let tree = Node(Leaf (), Node(Leaf (), Leaf ()))

    let leaves = numberOfLeaves tree

    test <@ leaves = 3 @>

[<Fact>]
let ``counts the leaves' content length`` () =
    let treeOfWords = Node(Leaf "one", Node(Leaf "two", Leaf "three"))
    let treeOfNumbers = Node(Leaf 3, Node(Leaf 3, Leaf 5))

    let treeOfLengths = lengths treeOfWords

    test <@ treeOfLengths = treeOfNumbers @>
```


```fsharp
let rec lengths = function
    | Leaf v -> Leaf (String.length v)
    | Node(l, r) -> failwith "Not yet implemented"
```


```fsharp
let rec lengths = function
    | Leaf v -> Leaf (String.length v)
    | Node(l, r) -> Node (lengths l, lengths r)

```

```fsharp
[<Fact>]
let ``counts the leaves' content length, using map'`` () =
    let treeOfWords = Node(Leaf "one", Node(Leaf "two", Leaf "three"))
    let treeOfNumbers = Node(Leaf 3, Node(Leaf 3, Leaf 5))

    let treeOfLengths = map' treeOfWords String.length

    test <@ treeOfLengths = treeOfNumbers @>

[<Fact>]
let ``counts the leaves' content length, using map`` () =
    let treeOfWords = Node(Leaf "one", Node(Leaf "two", Leaf "three"))
    let treeOfNumbers = Node(Leaf 3, Node(Leaf 3, Leaf 5))

    let mapped = map String.length
    let treeOfLengths = mapped treeOfWords

    test <@ treeOfLengths = treeOfNumbers @>

let (^) = map

[<Fact>]
let ``counts the leaves' content length, lifting a function`` () =
    let treeOfWords = Node(Leaf "one", Node(Leaf "two", Leaf "three"))
    let treeOfNumbers = Node(Leaf 3, Node(Leaf 3, Leaf 5))

    let treeOfLengths = String.length ^ treeOfWords

    test <@ treeOfLengths = treeOfNumbers @>
```


# References

* [State Monad For The Rest Of Us - source code][source-code]

[source-code]: https://github.com/arialdomartini/state-monad-for-the-rest-of-us


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/30)

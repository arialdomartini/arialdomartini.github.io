---
layout: post
title: "State Monad For The Rest Of Us - Part 1"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- F#
- Functional Programming
most_read: true
---
<!--more-->

```fsharp
module StateMonadTest

open Xunit
open Swensen.Unquote

let countNumberOfLeaves tree = failwith "Not yet implemented"

[<Fact>]
let ``counts the number of leaves in a tree`` () =
    let tree = failwith "Not yet implemented"
    
    let leaves = numberOfLeaves tree
    
    test <@ leaves = 3 @>
```


```
     Node
     /  \
  Leaf  Leaf

     Node
     /  \
  Leaf  Node
        /  \
    Leaf   Leaf

   Leaf
```


```
     Node
     /  \
  Leaf  Node
        /  \
     Node   Leaf
     /  \
  Leaf  Node
        /  \
    Leaf   Leaf
```

```fsharp
type Tree = Leaf | ???
```

```fsharp
type Tree = Leaf | Node of ???
```


```fsharp
type Tree = Leaf | Node of (Leaf | Node of ...)
```


```fsharp
type Tree = Leaf | Node of Tree * Tree
```

Tree is a type, Leaf and Node are not.

```fsharp
let numberOfLeaves tree =
    match tree with
    | Leaf -> 1
    | Node (l, r) -> ??? 
```


```fsharp
let rec numberOfLeaves tree =
    match tree with
    | Leaf -> 1
    | Node (l, r) -> numberOfLeaves l + numberOfLeaves r
```


```fsharp

type Tree =
    | Leaf
    | Node of Tree * Tree

let rec numberOfLeaves tree =
    match tree with
    | Leaf -> 1
    | Node(l, r) -> numberOfLeaves l + numberOfLeaves r

[<Fact>]
let ``counts the number of leaves in a tree`` () =
    let tree = Node(Leaf, Node(Leaf, Leaf))

    let leaves = numberOfLeaves tree

    test <@ leaves = 3 @>
```

```fsharp
let rec numberOfLeaves = function
    | Leaf -> 1
    | Node(l, r) -> numberOfLeaves l + numberOfLeaves r
```


```fsharp
numberOfLeaves Leaf       = 1
numberOfLeaves Node(l, r) = numberOfLeaves l + numberOfLeaves r
```

# References

* [State Monad For The Rest Of Us - source code][source-code]
* [Arialdo Martini - Monads for the Rest of Us][monads-for-the-rest-of-us]

[source-code]: https://github.com/arialdomartini/state-monad-for-the-rest-of-us
[monads-for-the-rest-of-us]: https://arialdomartini.github.io/monads-for-the-rest-of-us

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/30)

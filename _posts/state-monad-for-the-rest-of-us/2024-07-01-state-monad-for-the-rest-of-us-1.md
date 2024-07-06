---
layout: post
title: "State Monad For The Rest Of Us - Chapter 1"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- F#
- Functional Programming
include_in_index: false
---
Source code: [github.com/arialdomartini/state-monad-for-the-rest-of-us][source-code].

## Binary Trees

Our journey starts with this problem: we want to count the number of
leaves of an arbitrary Binary Tree. To keep the problem as simple as
possible, we define a Binary Tree as that data structure having Nodes
and Leaves, and in which every Node has exactly two branches.

This is a Tree with 2 Leaves:

```
     Node
     /  \
  Leaf  Leaf
```

and here is a Tree with 3 Leaves:

```
     Node
     /  \
  Leaf  Node
        /  \
    Leaf   Leaf
```

It is legit to ask ourselves: is a single Leaf a Tree?

```
   Leaf
```

Let's agree that it is.  
Do null Trees &mdash; Trees without Nodes and Leaves &mdash; exist?  
Let's do ourselves a favor and agree they don't.

A Node always contains 2 Leaves. What does a Leaf contain? Nothing: it
just is.  
So, a Tree is either a Leaf or a Node containing 2 branches, each
containing another Tree structure. Remember this definition, because
we will translate it more or less literally to F#.

How deep can such Binary Trees be? Arbitrarily deep. For example, this
is a legit Tree:

```
     Node
     /  \
  Leaf  Node
        /  \
     Node   Leaf
     /  \
  Leaf  Node
        /  \
      Node  Leaf
      /  \
  Leaf   Node
         /  \
      Leaf  Leaf
```

Of course, our code should work with any Tree, no matter how deep.

## Counting Leaves
Good. We are diligent developers, so we start from a test. To keep it
simple, let's count the Leaves of a 3-leaves Tree:

```fsharp
module StateMonadTest

open Xunit
open Swensen.Unquote

let numberOfLeaves tree = failwith "Not yet implemented"

[<Fact>]
let ``counts the number of leaves in a tree`` () =
    let treeWith3Leaves = failwith "Not yet implemented"
    
    let leaves = numberOfLeaves treeWith3Leaves
    
    test <@ leaves = 3 @>
```


Let's fill the gaps. As often happens with Functional Programming, it
is convenient to start from modeling problems through their types.

## Sum and Product Types
How to model a Tree?
That's an easy task in F#: it's a matter of translating literally our
definition:

> A Tree is 
> either a Leaf 
> or a Node containing 2 branches
> each containing another Tree structure.

Step by step:

- `A Tree is` is simply translated to

```fsharp
type Tree =
```

- A Tree can `be a Leaf`:

```fsharp
type Tree = Leaf
```

- `or a Node`. The `or` part is translated with the `|` operator:

```fsharp
type Tree = Leaf | Node ???
```

- The Node `contains` `2` branches. The *contain* part is translated with
  `of`, the multiplicity is translated with `*`:

```fsharp
type Tree = Leaf | Node of (??? * ???)
```

- We know that each branch contains either a Leaf or another Node. We
  already have a type representing either a Leaf *or* a Node, dont't
  we? It's the `Tree` itself. So:

```fsharp
type Tree = Leaf | Node of (Tree * Tree)
```

That's it!  
As an aside, this expression makes use of the so called Algebraic Data
Types, which include Sum (or Discriminated Union) Types and Product Types.

| Name          | Symbol used | Equivalent technique in OOP |
|---------------|-------------|-----------------------------|
| Sum Types     | `|`         | Inheritance                 |
| Product Types | `*`         | Multiple fieds in classes   |


In C# this would approximately translate to:

```csharp
abstract record Tree;
record Leaf : Tree;
record Node(Tree Left, Tree Right) : Tree;
```

In both cases, we have a recursive type, which both languages happily support.

## Creating instances
Creating instances in F# is similar, but more concise, than in C# and
Java: you just have to omit the `new` keyword and the parenthesis.
Our 3-leaves tree:

```
     Node
     /  \
  Leaf  Node
        /  \
    Leaf   Leaf
```

can be created with:

```fsharp
let treeWith3Leaves = Node(Leaf, Node(Leaf, Leaf))
```

This completes the test implementation:

```fsharp
[<Fact>]
let ``counts the number of leaves in a tree`` () =
    let treeWith3Leaves = Node(Leaf, Node(Leaf, Leaf))

    let leaves = numberOfLeaves treeWith3Leaves

    test <@ leaves = 3 @>
```

Good. Now, let's see how to make it green.

## Type Constructors and Data Constructors
It is important to understand that in:

```fsharp
type Tree = Leaf | Node of (Tree * Tree)
```

`Tree` is a type, `Leaf` and `Node` are not: `Leaf` and `Node` are
ways for *creating* an instance of type `Tree`. This is akin to the
difference between a class and its constructors. In F#, we would call
`Tree` and `Leaf` as follows:

| Element                            | Name                     |
|------------------------------------|--------------------------|
| `Tree`                             | Discriminated Union Type |
| `Leaf` and `Node of (Tree * Tree)` | Union Case               |



Haskell uses different names, but the meaning is the same:


| Element                     | Name             |
|-----------------------------|------------------|
| `Tree`                      | Type Constructor |
| `Leaf` and `Node Tree Tree` | Data Constructor |


We will shortly see why `Tree` is a Type *Constructor*, not just a
Type: as soon as `Tree` will take a (type) parameter, we will learn to
see it more as a function than just a type name.

The takeaway here is: given an instance of `Tree`, F# can *pattern
match* on the Case which was used to create the instance. It's like
the instance *remembers* which constructor was used to create it. You
can read more about it in [Pattern Matching - Identifier Patterns][pattern-matching].

This give you all the ingredients to develop a function to calculate
the number of leaves.

## Implementation
Pattern matching is easy:

```fsharp
let numberOfLeaves tree =
    match tree with
    | Leaf -> ???
    | Node (l, r) -> ??? 
```

Read it as a glorified `if/then/else` and ask yourself:

* If `numberOfLeaves` receives a tree, and that tree is a single
  `Leaf`, how many leaves does that tree have?
  
It means that the function needs to calculate the number of leaves of
this tree:

```
   Leaf
```
Of course, it has just `1` leaf! So:

```fsharp
let numberOfLeaves tree =
    match tree with
    | Leaf -> 1
    | Node (l, r) -> ??? 
```

What about if `tree` is a `Node`? You only know that a `Node` contains
exactly `2` branches. Each branch could be a Leaf, like in:


```
     Node
     /  \
  Leaf  Leaf
```

or it could be another arbitrarily deep tree, like in:

```
     Node
     /  \
  Leaf  Node
        /  \
     Node   Leaf
     /  \
  Leaf  Node
        /  \
      Node  Leaf
      /  \
  Leaf   Node
         /  \
      Leaf  Leaf
```

In the general case, you can see the received tree as:

```
     Node
     /  \
  Tree  Tree
```

This node contains as many leaves as the leaves in the right branch
tree, *plus* the leaves of the left branch tree. Literally translating
this sentence to F# leads us to:

```fsharp
let rec numberOfLeaves tree =
    match tree with
    | Leaf -> 1
    | Node (l, r) -> numberOfLeaves l + numberOfLeaves r
```

Notice that we added the keyword `rec` to the function definition, to
make it clear that the function is recursive.

Does this work? You have a test. Run it.  
Of course it's green. As it often happens, if it compiles it works.
Get used to this, it will happen over and over.


## Putting all together
Let's review what you obtained:

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
    let treeWith3Leaves = Node(Leaf, Node(Leaf, Leaf))

    let leaves = numberOfLeaves treeWith3Leaves

    test <@ leaves = 3 @>
```

You can see how the:

```fsharp
    | Leaf -> 1
```

branch is the *base case* of recursion, while the:

```fsharp
    | Node(l, r) -> numberOfLeaves l + numberOfLeaves r
```

is the (double) recursive step.  
Also notice how the results of the 2 recursive calls are *composed*
with the binary `+` function.

## F# <3 concisiness (and Haskell loves it too)
In:

```fsharp
let rec numberOfLeaves tree =
    match tree with
    | Leaf -> 1
    | Node(l, r) -> numberOfLeaves l + numberOfLeaves r
```

the parameter `tree` is only used to pattern match. To stress this, F#
provides an alternative, more concise syntax, where

```fsharp
... tree =
    match tree with
```

is replaced by:

```fsharp
... function =
```

So, our function can be made shorter, as:

```fsharp
let rec numberOfLeaves = 
    function
    | Leaf -> 1
    | Node(l, r) -> numberOfLeaves l + numberOfLeaves r
```

A nice way to interpret this is to squeeze the eyes and imagine it's a
combination of 2 separate function definitions, one on `Leaf` and one
on `Node(l, r)`. Haskell would really let you define 2 separate functions:


```haskell
numberOfLeaves Leaf       = 1
numberOfLeaves Node(l, r) = numberOfLeaves l + numberOfLeaves r
```

If you manage to understand this last snippet, *bravo*!, give yourself
a pat on the back, take a little breather, and get prepared to the
next chapter, in which you will invent ([or
discover][propositions-as-types]) Functors.

Jump to [Chapter 2](state-monad-for-the-rest-of-us-2).

# References

* [State Monad For The Rest Of Us - source code][source-code]
* [Microsoft Learn - Pattern Matching - Identifier Patterns][pattern-matching]
* [Philip Wadler - Propositions as Types][propositions-as-types]

[source-code]: https://github.com/arialdomartini/state-monad-for-the-rest-of-us
[pattern-matching]: https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching#identifier-patterns
[propositions-as-types]: https://www.youtube.com/watch?v=IOiZatlZtGU

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/30)

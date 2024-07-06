---
layout: post
title: "State Monad For The Rest Of Us - Part 2"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- F#
- Functional Programming
include_in_index: false
---
Source code: [github.com/arialdomartini/state-monad-for-the-rest-of-us][source-code].

## Building same-shape trees
In the [previouls chapter](state-monad-for-the-rest-of-us-1) you
managed to write an algorithm able to traverse an arbitrary binary
tree and to count the number of its leaves.

Let's do something at first glance way more challenging: while
traversing the tree, the algorithm should generate a second tree with
the same shape, but with different, transformed components.  
For example, let's say that instead of having empty Leaves, each Leaf
holds a value of some type, such as a string:

```
           Node
          /    \
  Leaf "one"   Node
              /   \
     Leaf "two"   Leaf "three"
```


While traversing this tree, the algorithm should generate a similarly
shaped tree, whose Leaves contain the string length:


```
           Node
          /    \
     Leaf 3    Node
              /   \
         Leaf 3    Leaf 5
```

## Most likely, same pattern
You already know how to travese a tree. If you think how you counted
the leaves, the code for traversing a tree and doing something else
must have, more or less, the same structure. In pseudo-F#:

```fsharp
let rec traverseTree = 
    function
    | Leaf       -> doSomething
    | Node(l, r) -> recurse on l; recurse on r; combine the results
```

With an imperative language, without the constraint of immutability,
traversing a tree and changing the elements as they are visited is as
easy as pie. It is likely that, if you are not familiar with
Functional Programming, solving the same problem with the constraint
of immutability sounds a uphill battle.  
Let's overcome these fears.

## Test and type
First things first, we need a test:

```fsharp
let rec lengths = 
    function
    | Leaf _ -> failwith "Not yet implemented"
    | Node(l, r) -> failwith "Not yet implemented"

[<Fact>]
let ``calculate the leaves' content length`` () =
    let treeOfWords = Node(Leaf "one", Node(Leaf "two", Leaf "three"))
    let treeOfNumbers = Node(Leaf 3, Node(Leaf 3, Leaf 5))

    let treeOfLengths = lengths treeOfWords

    test <@ treeOfLengths = treeOfNumbers @>
```

You need to extend the Tree data structure so that its leaves can hold
either integers and string values. Generic Types come to mind:

```fsharp
type Tree<'a> =
    | Leaf of 'a
    | Node of Tree<'a> * Tree<'a>
```

In Java and C# both Types and Type Parameters are capitalized. Without
context, in C# it is not clear if:

```csharp
List<T>
```

is a list generic on the type parameter `T` or a list of `T`, where
`T` is a concrete type defined somewhere.  
F# is kind enough to stick with a more explicit naming convention:

| Element         | Syntax                        | Example |
|-----------------|-------------------------------|---------|
| Types           | Capitalized word              | `Tree`  |
| Type parameters | Lower letter, prefixed by `'` | `'a`    |

Haskell does the same, without the prefix.  
Good. Let's make the test happy.

### Leaf branch
Let's start from the Leaf case. 

```fsharp
let rec lengths = 
    function
    | Leaf v -> ???
```

If `lengths` receives a `Tree`, and this tree is a `Leaf` containing a
`string`, what should it return?  
The rushed developer would imprudently answer: "the string length!"

```fsharp
let rec lengths = 
    function
    | Leaf v -> String.length v
```

To understand why this is wrong, let us write the `lengths`'
signature. `lengths` takes a tree of strings and returns a tree of
integers:

```fsharp
// Tree<string> -> Tree<int>
let rec lengths = 
    function
    | Leaf v -> String.length v
```

(Yes, it's horrible that primitive types such as `string` and `int`
violate the naming convention and are not capitalized. So, from now on
I'm using the Haskell's more strict convention).

It is apparent that the `Leaf` case cannot return `String.length v`,
because `String.length v` is an `int`, not a `Tree<int>`.

Look again the sample reference:


```
           Node                               Node
          /    \                             /    \
  Leaf "one"   Node            -->      Leaf 3    Node
              /   \                              /   \
     Leaf "two"   Leaf "three"             Leaf 3    Leaf 5
```
  
`Leaf "one"` needs to be transformed into `Leaf 3`, not into `3`.  
It is more reasonable to implement the `Leaf` branch as:

```fsharp
// Tree String -> Tree Int
let rec lengths = 
    function
    | Leaf v -> Leaf (String.length v)
```

Notice that, just like for the leaves count problem, this is the base
case of the recursion.

### Node branch
What to do when `lengths` gets a Tree that is a `Node` of a left and a
right branch? This is more puzzling.

If the Leaf branch was the base case, following the same pattern you
can assume that in the Node branch there must be 2 recursive calls,
plus a way to combine the results:

```fsharp
let rec lengths = function
    | Leaf v -> Leaf (String.length v)
    | Node(l, r) -> (lengths l) ??? (lengths r)
```

The problem is: how to combine the 2 values? What to replace `???`
with?  
It helps again to focus on the signature: 

```haskell
Tree String -> Tree Int
```

and on the reference sample:

```
           Node                               Node
          /    \                             /    \
  Leaf "one"   Node            -->      Leaf 3    Node
              /   \                              /   \
     Leaf "two"   Leaf "three"             Leaf 3    Leaf 5
```

You get a Node in, you return a Node out. The right implementation
must be:

```fsharp
let rec lengths =
    function
    | Leaf v -> Leaf(String.length v)
    | Node(l, r) -> Node(lengths l, lengths r)
```

Is the test green? Yes it is!

## It's really the same pattern!
If the result reminds you of the leaves count case, it's because it
is almost the same:

```fsharp
let rec numberOfLeaves = 
    function
    | Leaf -> 1
    | Node(l, r) -> numberOfLeaves l + numberOfLeaves r

let rec lengths =
    function
    | Leaf v -> Leaf(String.length v)
    | Node(l, r) -> Node(lengths l, lengths r)
```

You can stress on the similarity replacing the call to `Node` with an
custom sum-like infix operator `^+`:

```fsharp
let (^+) l r = Node (l, r)
```

With it, you can build a Node with the very same syntax you used to
sum 2 integers:

```fsharp
    | Node(l, r) -> lengths l ^+ lengths r
```

You can also stress that the Leaf branches are the base cases of
recursion. You just need 2 helper functions. 

```fsharp
let baseCase _ = 1
let baseCase' v = Leaf(String.length v)
```

Putting all together, you get:

```fsharp
let baseCase _ = 1
let baseCase' v = Leaf(String.length v)
let (^+) l r = Node (l, r)

let rec numberOfLeaves =
    function
    | Leaf v     -> baseCase v
    | Node(l, r) -> numberOfLeaves l + numberOfLeaves r

let rec lengths =
    function
    | Leaf v     -> baseCase' v
    | Node(l, r) -> lengths l ^+ lengths r
```

Now the 2 algorithms are really similar:

* Both pattern match on the received tree.
* Both have the base case on the Leaf case.
* Both perform a double recursion on the Node branch.
* And both compose the result of the left and the right recursive
  calls using a combining function (`+` and `^+`).
* Finally, in both the actual action (counting / calculating the
  length) is performed in the Leaf branch only.
  
The fact that the shape of the two algorithms is the very same shoud
not surprise: after all, both functions have to traverse the tree
and, apparently, this shape captures the idea of *traversing a tree*.

The implementations are almost a copy/paste. And copy/pasting is the
root of all evil. It would be cool to abstract and isolate the
tree-traversing code from the performed action.

This is what you are going to do in the [next chapter](state-monad-for-the-rest-of-us-3).

# References

* [State Monad For The Rest Of Us - source code][source-code]

  
[source-code]: https://github.com/arialdomartini/state-monad-for-the-rest-of-us


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/30)

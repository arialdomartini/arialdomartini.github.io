---
layout: post
title: "State Monad For The Rest Of Us - Part 2"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- F#
- Functional Programming
include_in_index: false
---
# Building same-shape trees
In the [previouls chapter](state-monad-for-the-rest-of-us-2) you
managed to write an algorythm able to traverse an arbitrary binary
tree and to count the number of its leaves.

Let's do something at first glance way more challenging: while
traversing the tree, the algorythm should generate a second tree with
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


While traversing this tree, the algorythm should generate a similarly
shaped tree, whose Leaves contain the string length of word contained
in the corresponding leaf of the original tree (wow, that's a
mouthful):


```
           Node
          /    \
     Leaf 3    Node
              /   \
         Leaf 3    Leaf 5
```

# Most likely, same pattern

You already know how to travese a tree. If you think how you traversed
it to count the leaves, the code for traversing a tree and doing
something else must have, more or less, a similar structure. In
pseudo-F#:

```fsharp
let rec traverseTree = 
    function
    | Leaf       -> doSomething
    | Node(l, r) -> recurse on l, recurse on r
```

With an imperative language, without the constraint of immutability,
traversing a tree and changing the elements as they are visited is as
easy as pie.  
It is likely that if you are not familiar with Functional Programming,
solving the same problem with the constraint of immutability may sound
a uphill battle.

Let's overcome our fears.

# Test and type
First things first, let's write a comforting test:

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

We need to extend the Tree data structure so that its leaves can hold
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
F# has the good habit of sticking with a more explicit naming convention:


| Element         | Syntax                        | Example |
|-----------------|-------------------------------|---------|
| Types           | Capitalized word              | `Tree`  |
| Type parameters | Lower letter, prefixed by `'` | `'a`    |

Haskell does the same, without the prefix.  
Good. Let's make the test happy.

## Leaf branch
Let's start from the Leaf case. 


```fsharp
let rec lengths = 
    function
    | Leaf v -> ???
```

If `lengths` receives a `Tree`, and this tree is a `Leaf` containing a
`string`, what should it return?

The rushed developer would imprudently answer: "return the string
length!"


```fsharp
let rec lengths = 
    function
    | Leaf v -> String.length v
```

To understand why this is wrong, let's indulge a second writing the
`lengths`' signature. `lengths` takes a tree of strings and returns a
tree of integers:

```fsharp
// Tree<string> -> Tree<int>
let rec lengths = 
    function
    | Leaf v -> String.length v
```

(Yes, it's horrible that primitive types such as `string` and `int`
violate the naming convention and are not capitalized. So, from now on
I'm using the Haskell's more strict convention.)

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


  
`Leaf "one"` shall be transformed into `Leaf 3`, not into `3`.  
It is more reasonable to implement the `Leaf` branch as:

```fsharp
// Tree String -> Tree Int
let rec lengths = 
    function
    | Leaf v -> Leaf (String.length v)
```

Notice that, just like for the leaves count problem, this is the base case of the recursion.

## Node branch
What about when `lengths` gets a Tree that is a `Node` of a left and a
right branch? This is more puzzling.

If the Leaf branch was the base case, you can assume that in the Node
branch there must be 2 recursive calls:


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

You get a Node in, you return a Node.

```fsharp
let rec lengths =
    function
    | Leaf v -> Leaf(String.length v)
    | Node(l, r) -> Node(lengths l, lengths r)
```

Is the test green? Yes it is!

# It's really the same pattern!
If the result reminds you of the leaves count case, it is because it
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
custom infix operator akin to a sum, `^+`:

```fsharp
let (^+) l r = Node (l, r)
```

so that building a Node gets the same shape of summing 2 branches:

```fsharp
    | Node(l, r) -> lengths l ^+ lengths r
```

You can also stress that the Leaf branches are the base cases of
recursion. Putting all together, you get:

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

The 2 algorythms are really similar:

* Both pattern match on the received tree.
* Both have the base case on the Leaf case.
* Both perform a double recursion on the Node branch.
* And both compose the result on the left and the right recursive
  call, using a combining function (`+` and `^+`).
* Finally, in both the actual action (counting and calculating the length) is performed in the Leaf branch only.
  
The fact the shape of the two algorythms is the very same is not
surprising at all: after all, both functions have to traverse the
tree, and apparently this shape captures the idea of tree-traversing.

It would be cool to isolate the tree-traversing code from the
performed action.

This is what you are going to do in the next paragraph.

# Generic mapping
Let's consider back the original function:

```fsharp
// Tree String -> Tree Int
let rec lengths tree =
    match tree with
    | Leaf v -> Leaf(String.length v)
    | Node(l, r) -> Node(lengths l, lengths r)
```

Imagine a function that parses strings to integers:

```fsharp
// String -> Int
let parseWordToInt (word: string) : int =
    match word with
    | "one" -> 1
    | "two" -> 2
    | "three" -> 3
    | "four" -> 4
    ...
```

and one that counts the vowels in a string:

```fsharp
// String -> Int
let countVowels (input: string) : int =
    ...
```

Since both share with `String.length` the same signature `String -> Int`, you can easily use them in your algorythm:

```fsharp
// Tree String -> Tree Int
let rec lengths tree =
    match tree with
    | Leaf v -> Leaf(parseWordToInt v)
    | Node(l, r) -> Node(algo l, algo r)
```

and

```fsharp
// Tree String -> Tree Int
let rec lengths tree =
    match tree with
    | Leaf v -> Leaf(countVowels v)
    | Node(l, r) -> Node(algo l, algo r)
```

It makes sense to generalize the function and abstract the action
away, getting it as a parameter `f`. This would be akin to implementing the Strategy Pattern.

You can introduce `f` either before or after `tree`.

After:

```fsharp
// Tree String -> (String -> Int) -> Tree Int
let rec lengths tree f =
    match tree with
    | Leaf v -> Leaf(f v)
    | Node(l, r) -> Node(lengths l, lengths r)
```


Before:

```fsharp
// (String -> Int) -> Tree String -> Tree Int
let rec lengths f tree =
    match tree with
    | Leaf v -> Leaf(f v)
    | Node(l, r) -> Node(lengths l, lengths r)
```

Notice that the implementation is perfectly the same. While the placement of a parameter might seem a superficial detail, it actually reflects two fundamentally different mental models.  
You may not have realized it, but you just invented Functors, and the 2 functions above reflect 2 completely different metaphors: Functors as boxes vs Functors are morphisms between categories.  
This deserves some considerations, doesn't it?

# Opening Boxes or Lifting Functions
## Opening Boxes
Let's start from the version where the strategy `f` comes last:


```fsharp
// Tree String -> (String -> Int) -> Tree Int
let rec lengths tree f =
    match tree with
    | Leaf v -> Leaf(f v)
    | Node(l, r) -> Node(lengths l, lengths r)
```

Is the `lengths` signature still correct? Is `lengths` name still reflecting the operation being performed?  
Indeed, there is nothing in the implementation about *calculating
lengths*. And if you investigate on the types at hand, it will not
take long to realize that the function is now generic. If you ask the
F# compiler which type it infers, it will answer
`Tree a -> (a -> b) -> Tree b`:

![Inferred signature for lengths: Tree a -> (a -> b) -> Tree b](static/img/state-monad-for-the-rest-of-us/map-signature.png)

The mental model you can use to interpret this signature is the one of a box:

*`Tree a` is a box containing a value of type `a`. 
* You provide `lengths` with such a box, and a function from `a` to `b`, to be applied to the box content.
* You don't need to open the box: `lengths` will do this for you, giving you back a new box, containing a tranformed content of type `b`.

Here's a classic picture from [Aditya Bhargava's "Functors, Applicatives, And Monads In Pictures"][functors-in-pictures]:

![Functors as boxes](static/img/monads-for-the-rest-of-us/functors-as-boxes.png){: height="300px" }

It make sense to rename this function from `lengths` to `transform`. LINQ calls it `Select`. Indeed, here's the [signature of `Select`][select]:

```csharp
public static IEnumerable<TResult> Select<TSource, TResult>(
            this IEnumerable<TSource> source, Func<TSource, TResult> selector)
```

It matches the model `Box TSource -> (TSource -> TResult) -> Box TResult`.  
The Scala library Cats implements [the same signature][cats-map]:

```scala
abstract def map[A, B](fa: F[A])(f: (A) => B): F[B]
```

Although this is a very popular implementation for Functors, I am not particularly fond of it, and I think it's based on a poor metaphor.  
Let us see which Pandora's box we open just flipping the `f` and `tree` parameters.

## Lifting Functions
The signature `(a -> b) -> Tree a -> Tree b` is the one used by Haskell (see [`fmap` on Hoogle][fmap]):


```fsharp
// (a -> b) -> Tree a -> Tree b
let rec map f tree =
    match tree with
    | Leaf v -> Leaf(f v)
    | Node(l, r) -> Node(map l, map r)
```

The reason why this introduces a drammatically different interpretation has to do with currying and partial application. It may not be immediately apparent but, since functions associate on the right, the signature:

```haskell
(a -> b) -> Tree a -> Tree b
```

can be written as:


```haskell
(a -> b) -> (Tree a -> Tree b)
```

While:

```fsharp
// (a -> b) -> Tree a -> Tree b
let rec map f tree = ...
```

can be interpreted as a function of 2 parameters and returning a `Tree b`, the same function with the alternative, equivalent signature can be interpreted as a high-order function taking 1 parameter only, and returning a new function `Tree a -> Tree b`:

```fsharp
// (a -> b) -> (Tree a -> Tree b)
let rec map f = 
  fun tree -> ...
```

This is a general rule. Any F# function:

```fsharp
// a -> b -> c -> d -> e
let f a b c d e = ...
```

is completely equivalent to:

```fsharp
// a -> (b -> (c -> (d -> e)))
let f = 
  fun a -> 
    (fun b -> 
	  (fun c -> 
	    (fun d -> 
		  (fun e -> ...))))
```

This technique is called [currying][currying].  
In other words, all F# (and Haskell) functions take 1 parameter only. Multi-parameter functions are an illusion, syntactic sugar based on the fact that all functions are implicitely curried.

Read again the curried form of our function:

```fsharp
// (a -> b) -> (Tree a -> Tree b)
let rec map f = 
  fun tree -> 
    match tree with
    | Leaf v -> Leaf(f v)
    | Node(l, r) -> Node(map l, map r)
```

You give it a humble function `f: ('a -> 'b)`, which can only operate on `'a` values, and it transforms it into a on-steroids function `superF:  Tree<'a> -> Tree<'b>`.

To see this in practice, consider again the test method:

```fsharp
[<Fact>]
let ``calculate the leaves' content length, using map`` () =
    let treeOfLengths = map String.length treeOfWords

    test <@ treeOfLengths = treeOfNumbers @>
```

and focus on

```fsharp
map String.length treeOfWords
```

It's invoking `map` passing 2 arguments. But we know all functions are 1-parameter function. What happens if you pass only 1 argument? What's the meaning of:

```fsharp
map String.length
```

Let's see this in action:

```fsharp
let mapped = map String.length
let treeOfLengths = mapped treeOfWords

test <@ treeOfLengths = treeOfNumbers @>
```

If you ask F# to add type annotations on `mapped`, it would write:

```
let mapped: Tree<string> -> Tree<int> = map String.length
let treeOfLengths = mapped treeOfWords

test <@ treeOfLengths = treeOfNumbers @>
```

Read it like this: you feed `map` with `String.length`, a function `String -> Int`, which can only operate on strings, and it gives you back a super-`String.length`, which operates on trees of strings.  
We can make this even more apparent aliasing `map` with a custom operator `^`:

```fsharp
let (^) = map
```

Guess if the following compiles:

```fsharp
let treeOfLengths = String.length treeOfWords

test <@ treeOfLengths = treeOfNumbers @>
```

Of course it does not! `String.length` operates on strings, and
`treeOfWords` is a `Tree<string>`, not a `string`. Let's make it a
function on-steroids with our brand new `^` operator:

```fsharp
let treeOfLengths = String.length^ treeOfWords

test <@ treeOfLengths = treeOfNumbers @>
```

Wow. It works. Can you see how this is based on the metaphor of *lifting functions*? The idea is: you can think in terms of simple types, without worrying about trees; just write your ordinary functions operating on ordinary types. Then, lift your functions so they learn how to operate on trees.

Let me use again this image by Bartosz Milewski, from the chapter [Functors][bartosz-functors] of his book [Category Theory for Programmers][bartosz]:

![a functor as described in Category Theory](static/img/monads-for-the-rest-of-us/bartosz-functor.png){: height="300px" }

to which I added a missing arrow. Here's the metaphor:

* There are 2 worlds, `C` and `D`, the lower world of ordinary functions and values, and the upper world of Trees.
* In the lower world of ordinary functions there is a function `f` from the type `a` to the type `b`. In our case, `String.length`.
* In the upper world of Trees, there is an on-steroids function `F f` that does the same of `String.length`, but on Trees of string, instead of just strings.
* `map` / `^` is that function that lifts whatever function `f` operating on simple values to the on-steroids function operating on trees of simple values.

This is the notion of Functors: something able to move objects and functions from one world / category to another &mdash; possibly richer &mdash; one. This is not a series on Category Theory, so we will stop here on this topic. It should suffice to interpret the result as follows:

* Via the implementation of `map`, you capture the essence of traversing a tree and applying a function to its content.
* This allowed your design to completely separate the domain logic (`String.length`, `countVowels`, etc) from the tree-traversing code.
* The mental model you can use is that `map` lifts ordinary functions to tree-operating functions, without forcing you to re-implement the traversing logic over and over.

Cool. If you managed to read this far, take a break, reward yourself with a well deserved delicacy, and get ready to the next chapter. You are going to discover the limits of Functors.



* [State Monad For The Rest Of Us - source code][source-code]
* [github.com/dotnet/runtime - Signature of LINQ Select][select]
* [Signature of `map` in Cats][cats]
* [Signature of `fmap`][fmap]
* [Aditya Bhargava - Functors, Applicatives, And Monads In Pictures][functors-in-pictures]
* [Wikipedia - Currying][currying]
* [Bartosz Milewski - Category Theory for Programmers][bartosz]
  * [Bartosz Milewski - Functors][bartosz-functors]

  
[source-code]: https://github.com/arialdomartini/state-monad-for-the-rest-of-us
[select]: https://github.com/dotnet/runtime/blob/main/src/libraries/System.Linq/src/System/Linq/Select.cs#L13-L14
[functors-in-pictures]: https://www.adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html
[cats]: https://www.javadoc.io/static/org.typelevel/cats-docs_2.13/2.12.0/cats/Functor.html#map[A,B](fa:F[A])(f:A=%3EB):F[B]
[fmap]: https://hackage.haskell.org/package/base-4.20.0.1/docs/Prelude.html#v:fmap
[currying]: https://en.wikipedia.org/wiki/Currying
[bartosz-functors]: https://bartoszmilewski.com/2015/01/20/functors/
[bartosz]: https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/30)

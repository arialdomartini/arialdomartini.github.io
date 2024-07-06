---
layout: post
title: "State Monad For The Rest Of Us - Part 4"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- F#
- Functional Programming
include_in_index: false
---
Source code:
[github.com/arialdomartini/state-monad-for-the-rest-of-us][source-code].

## Finding the limits of Functors

In the [previouls chapter](state-monad-for-the-rest-of-us-3) you
distilled `map`, a function exhibiting 3 powerful features:

* It is able to transform the leaves content applying an *arbitrary
  transformation function*.
* It does that without mutating the original tree, generating instead
  a brand new tree.
* And, finally, it strictly preserves the shape of the original tree.


It sounds like a mighty function and in fact it is.  
One may mistake, though, the possibility to pass an *arbitrary
transformation function* with the ability to *perform any arbitrary
transformation*. The two are not the same and, unfortunately, Functors
have limits to the trasformations they can produce.

Let's put this statement to the test.

## Index a tree
During its execution, `map` traverses the tree. Very well: we want it
to keep a track of the order with which it visits the leaves. That
is, if it visits:

1. first `Leaf "one"`
2. then `Leaf "two"`
3. and finally `Leaf "three"`

it should transform the tree:

```
           Node
          /    \
  Leaf "one"   Node
              /   \
     Leaf "two"   Leaf "three"
```

into:

```
                Node
               /    \
   Leaf ("one", 1)   Node
                    /    \
      Leaf ("two", 2)    Leaf ("three", 3)
```

Let's call this operation *indexing a tree*.

Here is the requirement:

```fsharp
[<Fact>]
let ``indexes a tree`` () =
    let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))

    let indexed = map index tree

    test <@ indexed = Node(Leaf ("one", 1), Node(Leaf ("two", 2), Leaf ("three", 3))) @>
```

## Easy peasy, if you are impure!
If you come from an imperative language, you didn't have to think
twice to find the solution:

```fsharp
let counter = 1
let index v =
    let indexedLeaf = (v, counter)
    counter = counter + 1
    indexedLeaf
```

Now, F# is a grumpy functional zealot and will refuse to compile:

```fsharp
counter = counter + 1
```

"Variables must be immutable", it will bemoan.  
Not only does it demand you to explicitly declare `counter` as
mutable:

```fsharp
let mutable counter = 1
```

but it also insists that you to use a special syntax for mutating the
value:

```fsharp
counter <- counter + 1
```

To add insult to injury, Rider will underline all the 4 occurrences of
`counter` to warn you with alarming voice "Mayday, mayday! A mutable
variable!". They really go to great lenghts to make it hard to deviate
from purity, don't they?  
Anyway, once bow to F#'s will and you run the test, you can finally
confirm that your impure `index` function *does* work:


```fsharp
let mutable counter = 1
let impureIndex v =
    let indexedLeaf = (v, counter)
    counter <- counter + 1
    indexedLeaf

[<Fact>]
let ``indexes a tree`` () =
    let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))

    let indexed = map impureIndex tree

    test <@ indexed = Node(Leaf ("one", 1), Node(Leaf ("two", 2), Leaf ("three", 3))) @>
```

## Just impossible if you are not
Now, give yourself 15 mins for a coding challenge. Try to get to the
same green test, this time adding only one single extra constraint:

* Be pure.

This means:

* Do not mutate any variable.
* Write `index` so that it is referential transparent.

About the last comment, here's a trick you can use: whatever function
you write, if it is pure, given the same argument it must always return
the same value. So, it must be always safe to invoke it twice.  
So, write your test as follows:

```fsharp
[<Fact>]
let ``indexes a tree`` () =
    let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))

    map index tree |> ignore  // intentionally invoked twice
    let indexed = map index tree

    test <@ indexed = Node(Leaf ("one", 1), Node(Leaf ("two", 2), Leaf ("three", 3))) @>
```

Another option ris to define this function:
  
```fsharp
let invokedTwice f =
    fun v ->
        f v |> ignore
        f v
```

and then, instead of:

```fsharp
let indexed = map index tree
```

you can use:

```fsharp
let indexed = map (index |> invokedTwice) tree
```

It will not take much to convince yourself that indexing a tree using
a pure Functor is just not possible.

The problem is that, while traversing the tree, your algorithm must
retain some form of memory, some kind of state.

So, you can draw an important conclusion:

> Implementing stateful algorithms with pure functions
> is not possible.

Correct?  
Nothing further from the truth.

You will prove that statement wrong in the [very next chapter](state-monad-for-the-rest-of-us-5). Then you will build on top of that demonstration discovering Applicative Functors first, and finally the mighty State Monad.

Treat yourself with an icecream, and when you feel ready, jump to the [chapter 5](state-monad-for-the-rest-of-us-5).


[source-code]: https://github.com/arialdomartini/state-monad-for-the-rest-of-us


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/30)

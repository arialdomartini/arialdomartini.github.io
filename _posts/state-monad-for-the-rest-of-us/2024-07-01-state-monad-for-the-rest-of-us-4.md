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
If you did your homework, in the [chapter 3](state-monad-for-the-rest-of-us-3) 
you convinced yourself that `map`:

```fsharp
let rec map f =
    function
    | Leaf v -> Leaf(f v)
    | Node(l, r) -> Node(map f l, map f r)
```

is not as mighty as it needs to index a tree. Nevertheless, it is very
likely that, whatever function you will end up with, it will need to
pattern match on leaves and nodes just like `map` does. Let's follow
this gut feeling, and let's start from this scaffold:


```fsharp
let rec index =
    function
    | Leaf v -> failwith "Not yet implemented"
    | Node(l, r) -> failwith "Not yet implemented"

[<Fact>]
let ``indexes a tree`` () =
    let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))

    let indexed = index tree

    test <@ indexed = Node(Leaf ("one", 1), Node(Leaf ("two", 2), Leaf ("three", 3))) @>
```

# With mutation
Let's start from the leaf branch, again:

```fsharp
let rec index =
    function
    | Leaf v -> failwith "Not yet implemented"
    | Node(l, r) -> failwith "Not yet implemented"
```

It might help to compare this with the word-length-calculation algorithm that you
developed in [Chapter 2](state-monad-for-the-rest-of-us-2):

```fsharp
let rec lengths =
    function
    | Leaf v -> Leaf(String.length v)
    | Node(l, r) -> Node(lengths l, lengths r)
```

Let's reason about the signature:

```fsharp
lengths :: Tree String -> Tree (Int)
index   :: Tree String -> Tree (String * Int)
```

This should give you a hint. The leaf branch returns a leaf containing
a tuple with the original value *and* the current counter:

```fsharp
let rec index =
    function
    | Leaf v -> Leaf (v, count)
    ...
```

Cool. Now, you need to define `count`.

# The Moment of Truth
How to define `count`?

Now, reflect for a while, and ask yourself: where does `count` come
from? Where to define it?

You have 2 options.  
If there is a single takeaway you will ever remember from this series,
please, let it be this one. Big decisions often come from tiny, at
first glance harmless decisions. And I can tell you: this one is a
humongous decions. This is, indeed, the inflection point where your
code:

* either takes on imperative nature
* or is set to flourish as functional.

So, which options do you have? Try to find them out.

If you want to develop a functional attitude, I recommend you not to
cheat, and to jump to the next chapter only when you have found out 2
possible options. They don't need to be a complete solution. You just
have to answer the question:

* where does that `count` value come from?

Have a long hot tea, or a whiskey and a cigar if you prefer, and take
all the time you need. See you tomorrow in the [chapter 5](state-monad-for-the-rest-of-us-5). Bye!

# References
* [Arialdo Martini - Monads for the Rest of Us][monads-for-the-rest-of-us]

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/30)


[monads-for-the-rest-of-us]: https://arialdomartini.github.io/monads-for-the-rest-of-us

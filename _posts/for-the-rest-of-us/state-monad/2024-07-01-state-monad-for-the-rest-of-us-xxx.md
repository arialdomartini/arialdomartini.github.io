---
layout: post
title: "State Monad For The Rest Of Us - Part 7"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- F#
- Functional Programming
include_in_index: false
---
Source code:
[github.com/arialdomartini/state-monad-for-the-rest-of-us][source-code].

In [chapter 7](state-monad-for-the-rest-of-us-7) I mentioned that
there was a second perspective with which to get to the same result:

* Manipulate the signature, separating the state-management and domain
  logic on the type level.

Let us do this, because it will lead to some interesting refinements

## Working on the signature
Observing again the original signature:

```haskell
Tree a -> (Int -> (Tree (a, Int), Int))
```

it's not hard to see what happened. Ideally, you wanted a function:

```haskell
Tree a -> Tree (a, Int)
```

taking a tree of whatever content, and returning an indexed version of
it.  This is the original domain logic.  
Or, even better, ideally you wanted the indexing logic only:

```haskell
a -> (a, Int)
```

or a generic:

```haskell
a -> b
```

letting a Functor take care of the logic for traversing a tree. But
then you found out that Functors are not powerful enough to apply that
function with `map`.  

Instead of that, you ended up with:

| Pure logic                | What you got                              |
|---------------------------|-------------------------------------------|
| `Tree a -> Tree (a, Int)` | `Tree a -> (Int -> (Tree (a, Int), Int))` |


or, generalizing:


| Pure logic | What you got             |
|------------|--------------------------|
| `a -> b`   | `a -> (Int -> (b, Int))` |


Squint the eyes. This expression contains the domain logic and the
count-handling logic mixed together. Let me move the count-handling
logic to a separate line:

```haskell
Tree a ->           Tree (a, Int)
          (Int -> (               , Int))
```

or if you want to make to use the more generic version:


```haskell
a ->            b
     (Int -> (    , Int))
```

The:

```haskell
Int -> (b, Int)
```

captures exactly what you did in [chapter
6](state-monad-for-the-rest-of-us-6):

| What you did                                                     |       |               |
|------------------------------------------------------------------|-------|---------------|
| You return a function getting the previous `count` value         | `Int` |               |
| You return the domain logic result *plus* the next `count` value |       | `-> (b, Int)` |


If you want to give this idea a name, and capture it with a type, it
makes sense to defind it with:

```fsharp
type WithCount<'b> = WithCount of (int -> 'b * int)
```

Of course, this could be more specif to trees:

```fsharp
type WithCount<'b> = WithCount of (int -> Tree<'b> * int)
```

or even more specif to our indexing use-case:

```fsharp
type WithCount = WithCount of (int -> Tree<string, int> * int)
```

but why should it be? On the contrary, it could be generic also on the
type of the counter:

```fsharp
type WithCount<'b, 'c> = WithCount of ('c -> 'b * 'c)
```

where `'c` is the type of the state being chained, and `'b` is the
original domain logic result type.  
`c` is not a count anymore, it's a generic state. Let's call it
`s`. And `WithCounter` is a misleading name. We can easily call it
`State`:

```fsharp
type State<'b, 's> = State of ('s -> ('b * 's))
```

This generalized version of `WithCount` is, ladies and gentlemen, the
State Monad.


# References
* [State Monad For The Rest Of Us - source code][source-code]
  
[source-code]: https://github.com/arialdomartini/state-monad-for-the-rest-of-us


# Comments
[GitHub Discussions][discussions]


[discussions]: https://github.com/arialdomartini/arialdomartini.github.io/discussions/30


{% include fp-newsletter.html %}

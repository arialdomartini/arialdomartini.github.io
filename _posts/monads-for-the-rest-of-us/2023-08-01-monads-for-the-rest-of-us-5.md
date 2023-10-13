---
layout: post
title: "Monads for the rest of us, in C# - Part 5"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- C#
- Functional Programming
include_in_index: false
---
I hope you enjoyed your icecream.  
Without much ado, let's get our hands dirty with a different use case for monads. That could sound puzzling, at first, but I swear that it is simple: nondeterministic functions.

# Nondeterministic functions
In this context, *nondeterminism* does not mean *randomness* &mdash; by the way, another successful use case for monads.  
If a *deterministic* function represents a single path from an input to an outcome

```csharp
int IWillAlwaysCalculateTheDouble(int n) => n * 2;
```

a *nondeterministic* one represents an execution stemming into many paths, each yielding possibly different outputs. 

```csharp
int IAmUndecided(int n) => ... // maybe n*2, maybe n*3, maybe n*4
```

*Nondeterministic* is a misleading term: our nondeterministic functions are still pure and referential transparent: they consistently produce the same outputs for the same inputs. However, they are used to model uncertainty.  
Think to a function that given a specific position in a maze return the information "*from here you can either go forward, to the left or to the right: either paths are legit. I'm not going to take a decision on a specific path.*".

Unsurprisingly, nondeterministic functions are typically leveraged to explore combinatory problems, such as the evaluation of the possible moves in a chess match.  
And that's exactly the use case we will develop (although in an extremely simplified implementation).

## Modeling nondeterminism
Imagine a pure, deterministic function that given the position of a knight on a chess board, moves it to a different, legit position:

```csharp
moveKnight :: Position -> Position
```

A trivial way to model `moveKnight` as nondeterministic is to let it return multiple paths:

```csharp
moveKnight :: Position -> IEnumerable<Position>
```

That's absolutely correct, and you will find many tutorials implementing nondeterminism with Monads this way.  
I think it misses the opportunity to understand some small, yet important minutiae. Allow me then to proceed differently.

Following our initial idea to devote a dedicated monadic type to each specific source of side-effect / extra-behavior, let's imagine to have a `Nondeterministic` type:

```csharp
moveKnight :: Position -> Nondeterministic
```

Of course, a nondeterminism on integer outputs is different from a nondeterminism on `Positions`, so we'd better have a type parameter for our monad:


```csharp
moveKnight :: Position -> Nondeterministic<Position>
```

## The List monad is just a simplification
What does `Nondeterministic<Position>` mean?  
It models the fact that `moveKnight` is undecided which `Position` the algorithm should consider. I treats its undecision as a source of unpurity so it returns a monadic value. Doing so, the problem of considering all the possibilities is deferred. When the monad will be run, someone else will reason about it.  
All the `Nondeterministic<Position>` monad does is to provide the programmer with a way to compose and bind other nondeterministic functions, via the classsical standardized interface or a monad, without loosing information about the source of nondeterminism.

Now, what's inside the source code of `Nondeterministic<A>`? It's only natural to model the multiple possible values as a collection of `A`, `IEnumerable<A>`. But that's only one way to represent that.

A `decimal -> Nondeterministic<decimal>` representing the predicted price change of a stock is likely to return values around a Gaussian distribution. From this perspective, `IEnumerable<decimal>` would be a poor model, because it would correspond to a distrubution of equally probable prices.

In implementing `Nondeterministic<decimal>`, you could easily decide to store the mean and the variance, rather than a collection of possible prices. What is important, for your implementation, is that given 2 functions built around the notion of a Gaussian distribution, combining them match the mathematical rule you know must hold.

The take aways are:

* all the articles and tutorial stating that "*List is a monad for modeling non-deterministic computations*" are surely right, but they tell half the story. By no means this implies that Lists are the only way for modeling nondeterminism.

* If your `Nondeterministic<decimal>` must implement some logic about Gaussian distribution, this means that Monads aren't just a stratagem to mechanically combine type-uncompatible functions ; they are in fact a design tool for type-modeling the business domain.

If you are going to embrace Functional Programming, you are going to invent a lot of new monads.










# Implementing the List monad
We left with:

```csharp
moveKnight :: Position -> Nondeterministic<Position>
```

For the sake of simplicity: 

* We'll just use a pair of integers to represent positions
* We will not raise errors if the knight move outside the board
* We consider all the positions as equally probable and legit. So we can be prosaic and opt for a simple `IEnumerable`.

Considering how a knight moves:

![A chess board with a knight and an indication of the possible destination positions](static/img/monads-for-the-rest-of-us/Chessboard480.svg.webp)

this is a possible implementation for `move`:

```csharp
using Position = ValueTuple<int, int>;

Func<Position, NonDeterministic<Position>> move = currentPosition =>
{
    var (x, y) = currentPosition;
	
    return new NonDeterministic<Position>(
        (x + 1, y + 2),
        (x - 1, y + 2),
        (x + 1, y - 2),
        (x - 1, y - 2),
        (x + 2, y + 1),
        (x - 2, y + 1),
        (x + 2, y - 1),
        (x - 2, y - 1));
};
```





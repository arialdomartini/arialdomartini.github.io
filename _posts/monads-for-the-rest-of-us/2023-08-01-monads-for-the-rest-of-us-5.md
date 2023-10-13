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

A trivial way to model `moveKnight` as a nondeterministic function is to let it return multiple paths:

```csharp
moveKnight :: Position -> IEnumerable<Position>
```

That's absolutely correct, and you will find many tutorials implementing nondeterminism with Monads this way.  
Yet, I think this is a missed opportunity to understand some small, yet important minutiae. Allow me then to proceed differently.

Following our initial idea to devote a dedicated monadic type to each specific source of side-effect / extra-behavior, let's imagine to have a `Nondeterministic` type:

```csharp
moveKnight :: Position -> Nondeterministic
```

Of course, nondeterminism on integers is different from nondeterminism on `Positions`, so we'd better have a type parameter for our monad:

```csharp
moveKnight :: Position -> Nondeterministic<Position>
```

Allow me please to shorten hereinafter `Nondeterministic` to `Nond`:

```csharp
moveKnight :: Position -> Nond<Position>
```

## The List monad is just a simplification
What does `Nond<Position>` mean?  
It models the fact that `moveKnight` is undecided which `Position` the algorithm should consider. It treats this undecision as a source of unpurity so it returns a monadic value. Doing so, the problem of considering all the possibilities is deferred. When the monad will be run, someone else will reason about it.  
All the `Nond<Position>` monad does is to provide the programmer with a way to compose and bind other nondeterministic functions, via the classsical standardized interface or a monad, without loosing information about the source of nondeterminism.

Now, what's inside the source code of `Nond<A>`? It's only natural to model the multiple possible values as a collection of `A`, with `IEnumerable<A>`.  
But that's only one way to represent that.

A `decimal -> Nond<decimal>` representing the predicted price change of a stock is likely to return values around a Gaussian distribution. From this perspective, `IEnumerable<decimal>` would be a broken model, because it would correspond to a distrubution of equally probable prices, and this poorly model the reality.

In implementing `Nond<decimal>`, you could easily decide to store the mean and the variance, rather than a collection of possible prices. What is important, for your implementation, is that given 2 functions built around the notion of a Gaussian distribution, combining them match the mathematical rule you know must hold.

The take aways are:

* the articles and tutorials stating that "*List is a monad for modeling non-deterministic computations*" are surely right, but they tell half the story. By no means this implies that Lists are the only way for modeling nondeterminism.

* If your `Nond<decimal>` must implement some logic about Gaussian distribution, this means that Monads aren't just a stratagem to mechanically combine type-uncompatible functions ; they are in fact a *design tool* for type-modeling the business domain.

If you are going to embrace Functional Programming, you are going to invent a lot of new monads. You will like that.


# Implementing the List monad
We left with:

```csharp
moveKnight :: Position -> Nond<Position>
```

For the sake of simplicity: 

* We'll just use a pair of integers `(int, int)` to represent positions
* We will not raise errors if the knight move outside the board: `(100, 100)` will be just as fine as `(5, 5)`
* We consider all the positions as equally probable and legit. 

So we can be prosaic and opt for a simple `IEnumerable` of `(int, int)`.  
Considering how a knight moves:

![A chess board with a knight and an indication of the possible destination positions](static/img/monads-for-the-rest-of-us/Chessboard480.svg.webp)

this is a possible implementation for `move`:

```csharp
using Position = ValueTuple<int, int>;

Func<Position, Nond<Position>> move = currentPosition =>
{
    var (x, y) = currentPosition;
	
    return new Nond<Position>(new[]
    {
        (x + 1, y + 2),
        (x - 1, y + 2),
        (x + 1, y - 2),
        (x - 1, y - 2),
        (x + 2, y + 1),
        (x - 2, y + 1),
        (x + 2, y - 1),
        (x - 2, y - 1)
    });
};
```


## Using Bind
Let's see how to *use* `Bind` (yes: from now on, we are calling `Apply` as a Haskell programmer would).  
Starting from position `(5, 5)` we apply `move` over and over, passing the result of the one call to the next one with `Bind`:

```csharp
Position start = (5, 5);
Nond<Position> step1 = move(start);
Nond<Position> step2 = move.Bind(step1);
Nond<Position> step3 = move.Bind(step2);
```

If we used Linq (we will learn this soon!), we could write the code as:

```csharp
from s1 in move(start)
from s2 in move(s1)
from s3 in move(s2)
select s3;
```

To give you a sense of the flexibility, here's how to calculate the `1.073,741,824` possible destinations (`741` of which are unique) for a knight starting from `(5, 5)` after 10 steps:


```csharp
var startingPoint = new Nond<Position>((5,5));
var steps = 
    Enumerable.Range(1, 10)
        .Aggregate(startingPoint, (current, _) => move.Bind(current))
        .Run();

Assert.Equal(1_073_741_824, steps.Length());
Assert.Equal(741, steps.Distinct().Length());
```

Just like with the IO Monad, our client code *knows* from the type-system that there is a side effect, but *does nothing* in particular to deal with it other than using the monadic `Bind`.


## Implementing Bind and Run
We will implement the `Nondeterministic` monad in 5 simple steps:

* enough of the `Nondeterministic` type to store what is provided by the client
* a new function `Return`, as a way to lift values to the `Nondeterministic` realm
* `Run`, to unroll all the possible combinations
* `Bind`, to link functions
* `ComposeWith`, which we assume will be based on `Bind`

## Type skeleton
What's the minimum of `Nondeterministic` to make the compiler happy? Our function returns:

```csharp
Func<Position, Nond<Position>> move = currentPosition =>
{
    var (x, y) = currentPosition;
	
    return new Nond<Position>(new[]
    {
        (x + 1, y + 2),
        ...
        (x - 2, y - 1)
    });
};
```

For this, the following is enough:

```csharp
record Nond<T>(IEnumerable<T> Items)
```

## Return
Read again our original client code:

```csharp
Position start = (5, 5);

Nond<Position> step1 = move(start);
Nond<Position> step2 = move.Bind(step1);
Nond<Position> step3 = move.Bind(step2);
```

You might dislike that it's not completely consistent: the first time `move` is invoked with the native C# function application; then, `Bind` is used.  
We could be more consistent if only the starting position was nondeterministic too. Now, think about it: nothing prevents us from having a function that, given a single value, returns the same value in a nondeterministic context. Something like:

```csharp
Nond<Position> BuildMonad(Position p) =>
    new Nond<Position>(new []{ p });
```


This would allow you to write:

```csharp
Nond<Position> start = BuildMonad(5, 5);

Nond<Position> step1 = move.Bind(start);
Nond<Position> step2 = move.Bind(step1);
Nond<Position> step3 = move.Bind(step2);
```

Interpret this like the following:

* The knight starts from `(5, 5)`
* We want to explore all the possible combinations, so we want to operate in a nondeterministic context
* Then, we use `BuildMonad` to *lift* the deterministic position `(5, 5)` to a non deterministic context &mdash; *returning* us `new Nond<Position>(new []{ (5, 5) })`
* Bang! We are already in a world where all our functions will keep exploring the knight movement paths. We can proceede `Bind`ing functions after functions.

Here's a test for `BuildMonad`:

```csharp
Position position = (5, 5);
Nond<Position> monadicPosition = BuildMonad(position);

IEnumerable<Position> allCombinations = monadicPosition.Run();
        
Assert.Equal(1, allCombinations.Length());
Assert.Equal(new [] { position }, allCombinations);
```

To understand this little `BuildMonad`, compare it with the `identity` function:

```csharp
Nond<Position> BuildMonad(Position p) =>
    new Nond<Position>(new []{ p });

Position Identity(Position p) =>
    p;
```

* Give a position to `Identity`, and it will give it you back, unmodified. It's a pure, honest, deterministic function.
* Give a position to `BuildMonad` and it will give it you back, unmodified, but together with a monadic structure: it would put the position in a list and pass it to the `Nond<Position>` monad constructor. This makes `BuildMonad` a nondeterministic function.

That's a funny function, if you think about it! Its signature claims it is nondeterministic: it should be undecided which position to return; but in fact it is not, it returns only one. 

You can think of it as the simplest possible case for a nondeterministic function. It *acts* as nondeterministic, so it can be used together with its more complex sisters in a `Bind` chain. As a matter of facts, it is deterministic but its type signature lets it operate in an nondeterministic context.

Here's the takeaways:

* It is the monadic version of the identity function.
* It is traditionally called `return`
* By definition, `bind` and `return` are the minimal implementation of *any* monads.

In the case of the IO monad, it would be a function lifing a value in a side-effect context, and performing no side-effects at all:

```csharp
IO<T> return(T value) => 
    new IO(() => value);
```

In the case of the Error monad, which models a context where functions can generally fail returning errors, it would lift a value in a function just not failing at all.

`return` is the elevator to the monadic world.  
Yes, I agree with you: `return` is a horrible and misleading name. As mentioned in [Why does a monad use "return" or "unit" rather than "lift"?][return-name] it's called like this only for historical reasons.  
Not my fault, I swear.


## Run
Implementing Run should be easy. Given the skeleton we built:

```csharp
record Nond<T>(IEnumerable<T> Items)
```

*running* the monad should be a matter of returning to the caller all the possible combinations the monad accumulated:

```csharp
record Nond<T>(IEnumerable<T> Items)
{
    internal IEnumerable<T> Run() => Items;
}
```

Seriously?  
Yes, that's it.

## Bind
Ok! This is just a bit more challenging. Imagine you executed a nondeterministic function. By its nature, it returned a nondeterministic value:

```csharp
Nond<int> nondeterministicValue = previousFunction("foo");

Func<string, Nond<int> f => ....
```

You want to *apply* / *bind* this value to your function `f`, which wants a deterministic `string`, not a monadic value. Bind will have the following signature:

```csharp
Nond<int> bind(
    Func<string, Nond<int> f, 
	Nond<string> value) => ....
```

Just like the IO case, implementing it is a matter of executing `Run` in a deferred way:

```csharp
internal static Nondeterministic<B> Bind<A, B>(
    this Func<A, Nondeterministic<B>> f, 
    Nondeterministic<A> m)
{
    IEnumerable<A> values = m.Run();
    IEnumerable<Nondeterministic<B>> mappedNested = values.Select(f);
    IEnumerable<B> flat = mappedNested.SelectMany(m => m.Run());
    return new Nondeterministic<B>(flat.ToArray());
}

Nond<string> PreviousFunction() => new Nond<string>(new[]
{
    "A", 
    "B"
});

Func<string, Nond<string>> g = j =>
    new Nond<string>(new[]
    {
        $"{j}1", 
        $"{j}2"
    });

Nond<string> nondValue = PreviousFunction();

Nond<string> bind = g.Bind(nondValue);

IEnumerable<string> combinations = bind.Run();
Assert.Equal(new [] { "A1", "A2", "B1", "B2" }, combinations);
```


# References
* [Factory Method][factory-method]
* [Why does a monad use "return" or "unit" rather than "lift"?][return-name]

[factory-method]: https://en.wikipedia.org/wiki/Factory_method_pattern
[return-name]: https://softwareengineering.stackexchange.com/questions/231136/why-does-a-monad-use-return-or-unit-rather-than-lift

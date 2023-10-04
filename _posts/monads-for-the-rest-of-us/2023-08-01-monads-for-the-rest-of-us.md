---
layout: post
title: "Monads for the rest of us, in C#"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- C#
- Functional Programming
most_read: true
---
Virtually all the tutorials introduce Monads only after a lengthy discussion of functors, and often resorting to intrepid and debatable metaphors.  
I dare to take a different path and go straight to the point, following what Mike Vanier did in his seminal 8-post series [Yet Another Monad Tutorial][yet-another-tutorial].

I challenged myself to write a post that:

- prioritizes code over storytelling
- uses no metaphors (no boxes, no burritos)
- does not require Category Theory
- is concise
- jumps past some of the trivial introductory topics (e.g., defining what a pure function is)
- does not follow the classical Functors -> Monads narrative
- is tailored for C# developers
<!--more-->
**Disclaimer**: here and there I use a bit of Haskell-like notation; I capitalize type names, I keep method names in lowercase, and I translate function signatures like `Func<string, int, double>` as `f :: String -> Int -> Double`.  
Forgive me.

# The Goal
I don't want to waste your time repeating what you already know about the virtue of pure functions. It should suffice starting from 2 assuntions:

- We want to use pure functions as much as possible
- We *do* want some side effects (e.g., ultimately, we want our program to update the DB)

Only at a first glance are those 2 assuntions contradictory. In fact, they translate to:

- How can we *model* side effects without giving up on function purity?

One of the approaches to solving this challenge is of course Monads.

## Monadic functions, not monads
Monads are an elusive topic because asking "what's a monad?" isn't really the right way to approach it.  
It makes more sense to dig into what makes a function *monadic*. So, stay with me for a moment: put aside the idea of "monad" as a tangible "thing," and instead, zero in on "monadic" as an adjective. 

Let's find out what a monadic function is.  
We need to start from pure and impure functions, with a particular focus on their signature

# Computations, side effects and function type signatures
## Pure functions
The simplest and best-behaved notion of computation is a pure function, as per the mathematical definition: an assignment of elements of the input domain to elements of an output codomain.

As long as a function is pure, its signature exactly reflect the mapping it performs. 

In fact, ideally a pure function can be replaced with a dictionary whose keys are all the possible inputs, and whose values are the corresponding returned results.

```csharp
// double :: Int -> Int
int double(int i) => i * 2;
```

is fully described and replaced by the (very large) dictionary:

```csharp
int double(int i) => codomain[i];

Dictionary<int, int> codomain = new Dictionary<int, int>
{
    ...
    [-1] = -2
    [0] = 0,
    [1] = 2,
    [2] = 4,
    [3] = 6,
    ...
}
```

Notice how the Dictionary's type arguments match the function type ones.

```csharp
double :: Func<int, int>
codomain:: Dictionary<int, int>
```

## Extra computations
C# is not a pure-functional language and its functions, other than mapping an input to an output, can also:

* perform input/outputs to the filesystem
* raise exceptions
* depend on a global or a local shared state

This extra behavior is generally not reflected in the signature. In [Functional Programming in C#][buonanno-honest] Enrico Buonanno distinguishes between *honest* functions, the ones that honor their signatures, and and *dishonest* functions, the ones that, besides performing their mapping also do something else.

In the followig listing, `closure(1)` returns different values depending on the value of a shared state `b`:

```csharp
var b = string.Empty;

// `String -> Int`
int Closure(string a) => a.Length + b.Length;

Assert.Equal(3, closure("foo"));

b = "wat?";
Assert.Equal(7, closure("foo"));
```

The signature of `closure` declares that it only depends on an `string`. But this tells half of the story: in fact, there is a dependency from a second `string` which does not emerge at all from the signature.

A function that possibly raises an exception is clearly dishonest:

```csharp
decimal divide(decimal n, decimal d) => n / d;

Assert.Equal(3M, divide(9M, 3M));
Assert.Equal(4.5M, divide(9M, 2M));

Assert.Throws<DivideByZeroException>(() => divide(9M, 0M));
```

## Honest Functions
It would be interesting if `divide`'s signature could somehow indicate that it can return either a `decimal` *or* throw `DivideByZeroException`, with something like:

```csharp
(decimal | DivideByZeroException) divide(decimal n, decimal d) => n / d;
```

Of course, this does not even compile. Indeed, that's something C# does not support natively. Java, with its infamous Checked Exceptions, provides developers with a way to decorate a function signature to indicate that it could throw:

```java
static double divide(int numerator, int denominator) throws ArithmeticException {
    if (denominator == 0) {
        throw new ArithmeticException("Denominator cannot be zero");
    }
    return (double) numerator / denominator;
}
```

The `throws ArithmeticException` part of the signature is used by the compiler to make it sure the extra behavior is not ignored by the client of the function. In this, it works more or less like the static typing check works. This is good.

There are two main problems with Java's approach, though.  
First, that indication is not part of the function type: it's orthogonal to it. The signature is still `int -> int -> double`.  
Second, this only works for exceptions. That's a pity, because exceptions are not the only source of impurity.

The Monad approach tackle exactly those 2 issues.  
The direction it takes is more similar to the fictional C# code than the latter Java example: Monads are about providing a function with a type that gives both the developer and the compiler a clear indication of which *extra actions* it performs, in addition to the pure computation.  
Monads are also more generic, and not limited to exceptions. There are many kinds of *extra actions*, and the idea of Monads is to represent each of them with a specific type.

Monads are a way to model side effects without loosing the benefits of using pure functions.  
They are a way to generalize the notions of Function Application and Function Composition to kinds of computation which are different from pure functions.

## Extending the notion of functions
Think again to the function we saw before:

```csharp
int Closure(string a) => a.Length + b.Length;
```

Its signature is:

```haskell
string -> int
```

but beyond its pure calculation, it also accesses a shared state. Ideally, we would like to indicate its type as:

```
string --[reads an extra string]--> int
```

The function:

```csharp
// decimal -> decimal -> decimal
decimal divide(decimal n, decimal d) => n / d;
```

would be better represented with a signature

```haskell
decimal -> decimal --[might raise an exception]--> decimal
```

A function performing an IO side effect, such as:

```csharp
static int Main(string[] args)
{
    Console.WriteLine("Hello, World!");
	return 0;
}
```

instead of having the type

```haskell
[String] -> Int
```

would be better characterized as

```haskell
[String] --[also writes to Console]--> Int
```

## Dreaming of Monads
So far, we fantasized about a different notion of functions, replacing the arrow `->` with `--[does something else]-->`, getting to signatures such as:

```haskell
string --[reads an extra string]--> int
decimal -> decimal --[might raise an exception]--> decimal
[string] --[also writes to Console]--> int
```

Let`s do a little step forward, replacing the narrative English sentences with some hypothetical but legit type:


| Case                                                      | Example of type                         |
|-----------------------------------------------------------|-----------------------------------------|
| A function that depends (reads) an extra string parameter | string --[Reader<String>]--> int        |
| A function that might raise an exception                  | decimal -> decimal --[Error]--> decimal |
| A function that also writes to the Console                | [string] --[IO]--> int                  |

We could think to other types representing arbitrary extra behaviors for functions:

| Case                                                                  | Example of type                    |
|-----------------------------------------------------------------------|------------------------------------|
| A function that could fail to return a value                          | string --[Maybe]--> int            |
| A function returning non-deterministic values                         | string --[NonDeterministic]--> int |
| A function returning a value and also writing a double somewhere else | string --[Writer<double>] --> int  |
| A function which depends and updates a shared state                   | string --[State<MyState>]--> int   |


If such function kinds existed and there was a way to extend Function Application and Function Composition, we could use them as enhanced versions of regular C# functions.

Let's call these functions *monadic functions*.  
We have all the ingredients to make them a reality.

## Building Monads
A signature as 

```
string --[Maybe]--> int
```

is not legal C#. The idea is to stuff the *monadic part*, the type representing the extra computation, either on the input or on the output side:

```
Maybe<string> --> int
string --> Maybe<int>
```

It turns out, the canonical way to define monadic functions is with the second form.

In defining those types, and especially the consequent Function Application and Function Composition, we will make sure of 2 important traits:

* that the result will be a *pure function*, so we will get the benefits of both pure functions, and controlled side effects
* that the pure calculation and the extra-behavior are not mixed together, so we can tackle them separately.

Grab your keyboard. It's time to give birth to your first monad.


# References

[Writing "honest" functions - Enrico Buonanno - Functional Programming in C#][buonanno-honest]

[buonanno-honest]: https://livebook.manning.com/book/functional-programming-in-c-sharp/chapter-3/78

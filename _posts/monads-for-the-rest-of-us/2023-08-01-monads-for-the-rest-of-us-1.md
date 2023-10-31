---
layout: post
title: "Monads for the rest of us, in C#"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- C#
- Functional Programming
include_in_index: false
---
## In which you realize that Monads are not things

I don't want to waste your time repeating what you already know about the virtue of pure functions. It should suffice starting from 2 assumptions:

- We want to use pure functions as much as possible
- We *do* want some side effects (e.g., ultimately, we want our program to write on a DB)

Only at a first glance are those 2 assumptions contradictory. In fact, they translate to:

- How can we *model* side effects without giving up on function purity?

One of the approaches to solving this challenge is of course Monads.

## Monadic functions, not monads
Monads are an elusive topic, and asking "*what's a monad?*" isn't really the right way to approach it.  
It makes more sense to dig into what a *monadic function* is. So, stay with me for a moment: put aside the idea of "*monad*" as a tangible "thing," and instead, zero in on "*monadic*" as an adjective. 

To find out what a *monadic function* is, we need to start from *pure* and *impure* functions, with a particular focus on their signature.

# Computations, side effects and function type signatures
## Pure functions
The simplest and best-behaved notion of computation is a pure function, as per the mathematical definition: an assignment of elements of the input domain to elements of an output codomain.

As long as a function is pure, its signature exactly reflects the mapping it performs.  
Ideally a pure function can be replaced with a dictionary whose keys are all the possible inputs, and whose values are the corresponding returned results.

```csharp
// Double :: int -> int
int Double(int i) => i * 2;
```

is fully described and replaced by the (very large) dictionary:

```csharp
Dictionary<int, int> Codomain = new Dictionary<int, int>
{
    ...
    [-1] = -2,
    [0] = 0,
    [1] = 2,
    [2] = 4,
    [3] = 6,
    ...
}

Assert.Equal(Double(2),  Codomain[2]);
Assert.Equal(Double(3),  Codomain[3]);
Assert.Equal(Double(-1), Codomain[-1]);
```

Notice how the dictionary's type arguments match the function ones:

```csharp
Double :: Func<int, int>
Codomain :: Dictionary<int, int>
```

## Extra computations
C# is not a pure-functional language and its functions, other than mapping an input to an output, can also:

* perform input/outputs to the filesystem
* raise exceptions
* depend on a global or a local shared state

Think about how you usually deal with these alternative notions of computation in non-pure programming languages. Computations that do I/O? No problem! Just do that. How about reading and writing shared state? C# won't prevent you from using a global variable. What about raising exceptions? That's natively supported by the `throw` keyword.

The important thing to note is, in each case, we are no longer dealing with the traditional notion of function, because of this "*something else*" happening along with the usual simple mapping.  
Interestingly, althought there are multiple kinds of this *something else*, C# is not doing anything to capture and model that. We just don't care, until we need to troubleshoot that because of some unexpected behavior not captured by the compiler.

This would not happen if only this extra behavior would be somehow reflected in the signature. In [Functional Programming in C#][buonanno-honest] Enrico Buonanno distinguishes between *honest* functions, the ones that honor their signatures, and and *dishonest* functions, the ones that, besides performing their mapping also do something else.

In the followig listing, `Closure("foo")` returns different values depending on the value of a shared state `b`:

```csharp
var b = string.Empty;

// `string -> int`
int Closure(string a) => a.Length + b.Length;

Assert.Equal(3, Closure("foo"));

b = "wat?";
Assert.Equal(7, Closure("foo"));
```

The signature of `Closure` declares that it only depends on an `string`. But this tells half of the story: in fact, there is an implicit dependency from a second `string` which does not emerge at all from the signature.

A function that possibly raises an exception is clearly dishonest:

```csharp
decimal Divide(decimal n, decimal d) => n / d;

Assert.Equal(3M, Divide(9M, 3M));
Assert.Equal(4.5M, Divide(9M, 2M));

Assert.Throws<DivideByZeroException>(() => Divide(9M, 0M));
```

Here's a first approximation: Monads are about making dishonest functions honest.  
So, let's see what being honest means.

## Honest Functions
It would be interesting if `Divide`'s signature could somehow indicate that it can return either a `decimal` *or* throw `DivideByZeroException`, with something like:

```csharp
(decimal | DivideByZeroException) Divide(decimal n, decimal d) => n / d;
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

The `throws ArithmeticException` part of the signature is used by the compiler to make it sure that the extra behavior is not ignored by the function caller. In this, it works more or less like a static typing check. This is good.

There are 2 main problems with Java's approach, though.  

1. That indication is not part of the function type: it's orthogonal to it. The signature is still `int -> int -> double`.  
2. This only works for exceptions. That's a pity, because exceptions are not the only source of impurity.

The Monad approach tackle exactly those 2 issues.  
The direction it takes is more similar to the fictional C# code than the latter Java example.

Here's a second approximation: Monads are about providing a function with a type that gives both the developer and the compiler a clear indication of which *extra actions* it performs, in addition to the pure computation.  
Monads are also more generic, and not limited to exceptions. There are many kinds of *extra actions*, and the idea of Monads is to represent each of them with a specific type.

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
decimal Divide(decimal n, decimal d) => n / d;
```

would be better represented with a signature

```
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

instead of having the type:

```haskell
[string] -> int
```

would be better characterized as:

```
[string] --[also writes to Console]--> int
```

Here's a third approximation: Monads are a way to extend the notion of functions &mdash; and their signature &mdash; to model their possible extra behavior.

## Dreaming of Monads
So far, we fantasized about a different notion of functions, replacing the arrow `->` with `--[does something else]-->`, getting to signatures such as:

```
string --[reads an extra string]--> int
decimal -> decimal --[might raise an exception]--> decimal
string[] --[also writes to Console]--> int
```

Let`s do a little step forward, replacing the comments in the arrows with some hypothetical &mdash; but legit &mdash; types:


| Case                                                      | Example of type                           |
|-----------------------------------------------------------|-------------------------------------------|
| A function that depends (reads) an extra string parameter | `string --[Reader<String>]--> int`        |
| A function that might raise an exception                  | `decimal -> decimal --[Error]--> decimal` |
| A function that also writes to the Console                | `string[] --[IO]--> int`                  |

We could think to other types representing arbitrary extra behaviors for functions:

| Case                                                                  | Example of type                      |
|-----------------------------------------------------------------------|--------------------------------------|
| A function that could fail to return a value                          | `string --[Maybe]--> int`            |
| A function returning nondeterministic values                          | `string --[Nondeterministic]--> int` |
| A function returning a value and also writing a double somewhere else | `string --[Writer<double>] --> int`  |
| A function which depends and updates a shared state                   | `string --[State<MyState>]--> int`   |


If such function kinds existed and there was a way to extend Function Application and Function Composition, we could use them as enhanced versions of regular C# functions.

Let's call these functions *monadic functions*. We have all the ingredients to make them a reality.

## Building Monads
A signature as 

```
string --[Maybe]--> int
```

is not legal C#. The idea is to stuff the *monadic part*, the type representing the extra computation, either on the input or on the output side:

```
Maybe<string> -> int
string -> Maybe<int>
```

It turns out, the canonical way to define monadic functions is with the second form.  
So, the monadic functions we mentioned before could have types similar to:

| Case                                                                  | Example of type                   |
|-----------------------------------------------------------------------|-----------------------------------|
| A function that depends (reads) an extra string parameter             | `string -> Reader<String, int>`   |
| A function that might raise an exception                              | `decimal -> Error<decimal>`       |
| A function that also writes to the Console                            | `string[] -> IO<int>`             |
| A function that could fail to return a value                          | `string -> Maybe<int>`            |
| A function returning nondeterministic values                          | `string -> Nondeterministic<int>` |
| A function returning a value and also writing a double somewhere else | `string -> Writer<double, int>`   |
| A function which depends and updates a shared state                   | `string -> State<MyState, int`    |

Those are perfectly legit C# signatures!

What does `f :: A -> SomeMonad<B>` really mean?  
It means that `f` is a regular, pure function taking a value of type `A` and returning a monadic output value, that is a `B` value enriched by a special type `SomeMonad` modeling some specific extra behavior. 

We will soon find out that this minor change throws a monkey wrench into the works of traditional function application and composition.

In the next part we will implement some of those type. In defining them, we will make sure of ensuring 3 important traits:

1. that the result will be about *pure functions*, so we will get the benefits of both pure functions and controlled side effects
2. that the extra-behavior is not accidentally executed while the pure functions are being manipulated
3. that a version of Function Application and Function Composition exist for them, so we can use them in real scenarios.

Here's the last approximation for this first article: Monads are not an intuitive concept. Monadic Functions are. Monads are just how functional languages have conventionally chosen to represent the output of Monadic Functions.

Grab your keyboard. It's time to give birth to your first monad. Jump to [Chapter 2](monads-for-the-rest-of-us-2).

# References

* [Writing "honest" functions - Enrico Buonanno - Functional Programming in C#][buonanno-honest]
* [Mike Vanier - Yet Another Monad Tutorial][yet-another-tutorial]

[buonanno-honest]: https://livebook.manning.com/book/functional-programming-in-c-sharp/chapter-3/78
[yet-another-tutorial]: https://mvanier.livejournal.com/3917.html

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/26)

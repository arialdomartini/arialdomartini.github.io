---
layout: post
title: "Monads for the rest of us, in C#"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- C#
- Functional Programming
most_read: true
---

<!--more-->
Virtually all the tutorials introduce Monads only after a lengthy discussion of functors, and often resorting to bold and debatable metaphors.  
I dare to take a different path and go straight to the point, following what Mike Vanier did in his seminal 8-post series [Yet Another Monad Tutorial][yet-another-tutorial].

I challenged myself to write a post that:

- prioritizes code over storytelling
- uses no funny metaphors (no boxes, no burritos)
- does not require Category Theory
- tries to be as concise as possible
- jumps past some of the trivial introductory topics (e.g., defining what a pure function is)
- does not follow the classical Functors -> Monads narrative
- uses TDD
- is tailored for C# developers

# Table of contents


**Disclaimer**: here and there I'm using a bit of Haskell-like notation; I capitalize type names, I keep method names in lowercase, and I translate function signatures like `Func<string, int, double>` as `f :: String -> Int -> Double`. Forgive me.

# The Goal
I don't want to waste your time repeating what you already know about the virtue of pure functions. It should suffice starting from 2 assuntions:

- We want to use pure functions as much as possible
- We *do* want some side effects (e.g., ultimately, we want our program to update the DB)

Only at a first glance are those 2 assuntions contradictory. In fact, they translate to:

- How can we *model* side effects without giving up on function purity?

One of the approaches to solving this challenge is of course Monads.

# Function Application and Function Composition
The key of Monads is to use the type system to separate out side-effecting computations from pure computations, so that they do not interfere with each other.

Let's get started characterizing the main traits of pure functions we are interested in: Function Application and Function Composition. It turns out this is all you need to re-implement to get Monads.

## Function Application
Given a method `String -> Int`:

```csharp
int MyLength(string s)
{
    return s.Length;
}
```

we want to be able to apply it to a value of type `String` to get back an `Int`. Function Application is natively supported by C#:

```csharp
var length = MyLength("foo");
Assert.Equal(3, length);
```

From now on, let's prefer expressing functions as instances of `Func`. The above method can be rewritten as a function as:

```csharp
Func<string, int> myLength = s => s.Length;
```

Native C# Function Application works just the same:

```csharp
var length = myLength("foo");
Assert.Equal(3, length);
```

In the next pages we are going to define *pure functions modeling side effects* (also known as "*monadic functions*"), and we will need to figure out how to apply them to values. We will discover that this is not natively supported by C#, and that a specific implementation is needed.  
So, it is interesting to start re-implementing manually the native C# Function Application: it will be the basis for the future *Monadic* Function Application.  
Let's write a write a High Order Function (HOF) that taken a function `f :: String -> Int` and a `String` value `a` applies `f` to `a` returning an `Int` result:

```csharp
Func<string, int> f = s => s.Length;
string a = "foo";

int apply(Func<string, int> f, string a) => f(a);

var length = apply(f, a);

Assert.Equal(3, length);
```

It's easy to make it generic, so it works with any function "f :: a -> b" whatever the types `a` and `b` are:

```csharp
Func<string, int> f = s => s.Length;

string a = "foo";

B apply<A, B>(Func<A, B> f, A a) => f(a);
        
int length = apply(f, a);
Assert.Equal(3, length);
```

Take a few seconds to meditate on what we just wrote. Not surprisingly, we discovered that Function Application is implemented as `f(a)`.  
In Haskell, `apply` is written as `$` at its (simplified) implementation is:

```haskell
($) :: (a -> b) -> a -> b
($) f a =  f a
```

It may seem that `Apply` is a useless redundant function, but it's not:

* it can be extended, giving you the opportunity to do *something else* while applying a function toa value. For example, you can decorate the invocation surrounding it with some logging information: 

```csharp
B apply<A, B>(Func<A, B> f, A a)
{
    Log.Information("Got a value {A}", a);
    var b = f(a);
    Log.Information("Returning a value {B}", b);
    return b;
}
```

* it gives you the possibility to extend the very meaning of Function Application. You will soon see that with Monads you will need a special `apply` implementation that is able to apply functions to incompatible value types.


# References

[Mike Vanier][yet-another-tutorial]

[yet-another-tutorial]: https://mvanier.livejournal.com/3917.html

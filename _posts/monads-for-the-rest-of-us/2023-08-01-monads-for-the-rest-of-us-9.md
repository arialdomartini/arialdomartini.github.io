---
layout: post
title: "Monads for the rest of us, in C# - Part 9"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- C#
- Functional Programming
include_in_index: false
---
## In which you see how easy functors are, and you find inner peace
Implementing `map` for a specific functor is often easy if you reason about the type signature.  
Let's do that for `IO`, `Nond` and `Maybe`. You will see that it's an easy exercise.

# IO Functor
Given a function:

```haskell
f :: A -> B
```

the IO Functor's map implementation will elevate it to:

```haskell
f.Map() :: IO<A> -> IO<B>
```

The implementation is not that hard at all:

```csharp
Func<IO<A>, IO<B>> Map<A, B>(Func<A, B> f) =>
    ioa =>
    {
        A a = ioa.Run();
        var b = f(a);
        return new IO<B>(() => b);
    };
```

which inlined is:

```csharp
Func<IO<A>, IO<B>> Map<A, B>(Func<A, B> f) =>
    ioa => new IO<B>(() => f(ioa.Run()));
```

It's easy to implement it as an Extension Method. Here's a test using it:

```csharp
var io = new IO<string>(() =>
{
    File.WriteAllText("output.txt", "I'm a side effect");
    return "foo";
});

Func<string, int> length = s => s.Length;

// let's elevate length
var lengthM = length.Map();

var l = lengthM(io);
            
var result = l.Run();
Assert.Equal(3, result);
Assert.Equal("I'm a side effect", File.ReadAllText("output.txt"));
```

The IO Monad is one of those that lends itself well to being interpreted with the metaphor of the box. If define an extension method that takes the IO Monad as the first parameter:

```csharp
static IO<B> Map<A, B>(this IO<A> ioa, Func<A, B> f) =>
    f.Map()(ioa);
```

then its use becomes:

```csharp
IO<int> l = io.Map(length);
```
Read `io.Map(length)` as:

* map the function `length : string -> int`
* to the content of the box `io`, which contains a `string`
* preserving the side effects

This is equivalent of using LINQ as the following:

```csharp
IO<int> l = io.Select(length); 
```


# What's next?
There are some topics I didn't get around to covering.

* How to deal with multi-parameter functions, with currying and partial application.
* How we did not need to implement the `Nondeterministic` monad: it's already natively implemented by LINQ.
* How to use LINQ to bind any custom monadic functions in a fluent way
* Implementation of more monads, such as Either, State, Reader and Writer.

I promise I will follow up with a new series.

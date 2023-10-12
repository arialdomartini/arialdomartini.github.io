---
layout: post
title: "Monads for the rest of us, in C# - Part 2"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- C#
- Functional Programming
include_in_index: false
---
So, we learnt that monadic functions are a way to model side effects without loosing the benefits of pure functions.  
Let's start from the hardest problem: making an impure function &dash; with IO side effects&dash; pure.

Here's a function than, other than calculating the length of a string, also writes to a file:

```csharp
int LengthWithSideEffects(string s)
{
    File.WriteAllText("output.txt", "I'm a side effect!");
    return s.Length;
}

var length = LengthWithSideEffects("foo");
        
Assert.Equal(3, length);
Assert.Equal("I'm a side effect!", File.ReadAllText("output.txt"));
```

Its type, `string -> int`, does not reflect the fact it is performing an IO. Let's start from applying the idea of type modeling the IO with a type `string -> IO<int>`.

```csharp
IO<int> LengthWithSideEffects(string s)
{
    ...
}
```

With the signature, we are good to go.  
Time to turn our attention to the body. If we want to keep the function pure, it seems there are only 2 options available:

1. to keep the pure computation and the side effect completely separate, so the pure computation can be safely executed without side effects, and the side effect deferred
2. to defer the whole execution, so neither the pure computation nor the side effect are actually executed just yet


1 is a bit impractical, because side effects and pure computations are often interleaved.  
2 is basically about deferring a computation with a lambda, like in a promise.

So it makes a sense to proceed with:

```csharp
IO<int> LengthWithSideEffects(string s) =>
    new IO<int>(() => 
    {
         File.WriteAllText("output.txt", "I'm a side effect!");
         return s.Length;
    });
```

As for the type `IO`, let's implement the minimum necessary to make the compiler happy:

```csharp
record IO<B>(Func<B> f);
```

which is equivalent to the more verbose:

```csharp
class IO<B>
{
    private readonly Func<B> _f;

    internal IO(Func<B> f)
    {
        _f = f;
    }
}
```

Wrapping it up, that's the result:

```csharp
record IO<B>(Func<B> f);
    
IO<int> LengthWithSideEffects(string s) =>
    new IO<int>(() =>
    {
         File.WriteAllText("output.txt", "I'm a side effect!");
         return s.Length;
    });

IO<int> length = LengthWithSideEffects("foo");

Assert.Equal(3, length);
Assert.Equal("I'm a side effect!", File.ReadAllText("output.txt"));
```

## It does not work!
Something is not quite correct.  
The last `Assert` is doomed to fail. Of course: `LengthWithSideEffects()` is pure, but only because we have cheated; the side effect is not executed at all.  
Even worse: this code won't even compile. See the problem? The result we get from `LengthWithSideEffects()` is not an `int` anymore, so it cannot be compared with `3`.

Addding insult to injury, our freshely brewed `LengthWithSideEffects` does not compose.  
Before we turned it into a monadic function, we could easily execute it and pass the result to another type-compatible function:

```csharp
// string -> int
int LengthWithSideEffects(string s)
{
    Console.Write("I'm a side effect!");
    return s.Length;
}

// int -> double
double Double(int i) => i * 2;

// string -> int -> double
double doubleTheLength = Double(LengthWithSideEffects("foo"));
        
Assert.Equal(6, doubleTheLength);
```

Try to do the same with the monadic version and the compiler will refuse to proceed, complaining that:

```
Argument type `IO<int>` is not assignable to parameter type `int`
```

We got to the point where we need to rethink the way we apply and compose functions.  
We'd better take a quick detour on ordinary function application and composition: we will easily learn how to extend them to monadic functions.

Here's the gist of this second article: monadic functions, as a notion of computation richer than standard functions, come together with a way to generalize Function Application and Function Composition so that they work with not-directly-compatible types.

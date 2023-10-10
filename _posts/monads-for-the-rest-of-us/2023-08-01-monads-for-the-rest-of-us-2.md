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
Let's start from the hardest problem: making an impure function &mdash; with IO side effects&mdash; pure.

Here's a function than, other than calculating the length of a string, also writes to a file:

```csharp
int CalculateWithSideEffect(string s)
{
    File.WriteAllText("output.txt", "I'm a side effect!");
    return s.Length;
}

var length = CalculateWithSideEffect("foo");
        
Assert.Equal(3, length);
Assert.Equal("I'm a side effect!", File.ReadAllText("output.txt"));
```

Its type is `string -> int`, which does not reflect the fact it is performing an IO. Let's start from applying the idea of type modeling the IO side effect, making its type `string -> IO<int>`.

```csharp
IO<int> CalculateWithSideEffect(string s)
{
    ...
}
```

With the signature, we are good to go.  
Time to turn our attention to the body. If we want to keep the function pure, it seems there are only 2 options available:

1. to keep the pure computation and the side effect completely separate, so the pure computation can be safely executed without side effects
2. to defer the whole execution, so neither the pure computation nor the side effect are actually executed


1 is a bit impractical, because side effects and pure computations are often interconnected and interleaved.  
2 is basically about deferring a computation with a lambda, like in a promise.

So it makes a sense to proceed with:

```csharp
IO<int> CalculateWithSideEffect(string s) =>
    new IO<int>(() => {
         File.WriteAllText("output.txt", "I'm a side effect!");
         return s.Length;
    });
```

As for the type `IO`, let's implement the minimum necessary not to loose information:

```csharp
record IO<B>(Func<B> f);
```

which is equivalent to the more verbose

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

Wrapping it up, that's the result

```csharp
record IO<B>(Func<B> f);
    
IO<int> CalculateWithSideEffect(string s) =>
    new IO<int>(() => {
         File.WriteAllText("output.txt", "I'm a side effect!");
         return s.Length;
    });

IO<int> length = CalculateWithSideEffect("foo");

Assert.Equal(3, length);
Assert.Equal("I'm a side effect!", File.ReadAllText("output.txt"));
```

Something is not quite correct. Sure, `CalculateWithSideEffect()` is pure, but only because we are cheating. The side effect hasn't been executed at all. Indded, the last `Assert` fails.  
Even worse: this code won't even compile. See the problem? The result we get from `CalculateWithSideEffect()` is not an `int` anymore, so it cannot be compared with `3`.

Addding insult to injury, our freshely brewed `CalculateWithSideEffect` does not compose.  
Before we turned it into a monadic function, we could easily execute it and pass the result to another type-compatible function:

```csharp
string -> int
int CalculateWithSideEffect(string s)
{
    Console.Write("I'm a side effect!");
    return s.Length;
}

// int -> int
int Double(int i) => i * 2;

//                    string -> int -> int
int doubleTheLength = Double(CalculateWithSideEffect("foo"));
        
Assert.Equal(6, doubleTheLength);
```

Try to do the same with the monadic version and the compiler will refuse to proceed, complaining that:

```
Argument type `IO<int>` is not assignable to parameter type `int`
```

We got to the point where we need to rethink the way we apply and compose functions.  
We'd better take a quick detour on ordinary function application and composition: we will easily learn how to extend them to monadic functions.

Indeed, Monads are a way to generalize the notions of Function Application and Function Composition to kinds of computation which are richer than standard functions. And this is the key to understanding them.

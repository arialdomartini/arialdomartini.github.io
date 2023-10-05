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
Time to turn our attention to the body. Let's give us the following goals:

* to keep the function pure
* to keep the pure computation and the side effect separate

```csharp
IO<int> CalculateWithSideEffect(string s)
{
    Action sideEffect = () => File.WriteAllText("output.txt", "I'm a side effect!");
    int PureComputation(string s) => s.Length;
            
    ...
}
```

How to proceed? In the end, we need to return an instance of `IO`. We are free to define the type `IO` as we wish, so it makes a sense to proceed with:

```csharp
IO<int> CalculateWithSideEffect(string s)
{
    Action sideEffect = () => File.WriteAllText("output.txt", "I'm a side effect!");
    int PureComputation(string s) => s.Length;

    int pureComputationResult = PureComputation(s);
    return new IO<int>(pureComputationResult, sideEffect);
}
```

or more concisely:

```chsarp
IO<int> CalculateWithSideEffect(string s) =>
    new IO<int>(
        s.Length,
        () => File.WriteAllText("output.txt", "I'm a side effect!"));
```

As for the type `IO`, let's implement the minimum necessary not to loose information:

```csharp
record IO<T>(T value, Action action);
```

which is equivalent to the more verbose

```csharp
class IO<T>
{
    private readonly T _value;
    private readonly Action _action;

    internal IO(T value, Action action)
    {
        _value = value;
        _action = action;
    }
}
```

Wrapping it up, that's the result

```csharp
record IO<T>(T value, Action action);
    
IO<int> CalculateWithSideEffect(string s) =>
    new IO<int>(
        s.Length,
        () => File.WriteAllText("output.txt", "I'm a side effect!"));

IO<int> length = CalculateWithSideEffect("foo");

Assert.Equal(3, length);
Assert.Equal("I'm a side effect!", File.ReadAllText("output.txt"));
```

Something is not quite correct. Sure, `CalculateWithSideEffect()` is pure, but only because we are cheating. The side effect hasn't been executed at all. Indded, the last `Assert` fails.  
Even worse: this code won't even compile. See the problem? The result we get from `CalculateWithSideEffect()` is not an `int` anymore, so it cannot be compared with `3`.

Addding insult to injury, our freshely brewed `CalculateWithSideEffect` does not compose.  
Before we turned it into a monadic function, we could easily execute it and pass the result to another type-compatible function:

```csharp
[Fact]
void composition_of_pure_functions()
{
	// string -> int
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
}
```

Try to do the same with the monadic version and the compiler will refuse to proceed, complaining that:

```
Argument type `IO<int>` is not assignable to parameter type `int`
```

We got to the point where we need to rethink the way we apply and compose functions.  
We'd better take a quick detour on ordinary function application and composition: we will easily learn how to extend them to monadic functions.

Indeed, Monads are a way to generalize the notions of Function Application and Function Composition to kinds of computation which are different from pure functions. And this is the key to understanding them.

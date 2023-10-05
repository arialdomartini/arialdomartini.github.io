---
layout: post
title: "Monads for the rest of us, in C# - Part 2"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- C#
- Functional Programming
include_in_index: false
---
So, we learnt that monadic functions are a way to model side effects without loosing the benefits of being pure.  
In a sense, they are a way to generalize the notions of Function Application and Function Composition to kinds of computation which are different from pure functions.

Let's start from the hardest problem: making an impure function &mdash; with IO side effects&mdash; pure.  
This is a function than, other than calculating the length of a string, also writes to a file:

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

Its type is `string -> int`, which does not reflect the fact it is performing an IO. Let's start from applying the idea of modeling this into a type, making its type `string -> IO<int>`.

```csharp
IO<int> CalculateWithSideEffect(string s)
{
    ...
}
```

With the signature, we are good to go.  
Let's turn our attention to the body. Let's give us the following goals:

* to keep the function pure
* to keep pure computation and side effect separate

```csharp
IO<int> CalculateWithSideEffect(string s)
{
    Action sideEffect = () => File.WriteAllText("output.txt", "I'm a side effect!");
    int PureComputation(string s) => s.Length;
            
    ...
}
```


In the end, we need to return an instance of `IO`. And so far it seems we are free to define the type `IO` as we wish. It makes a sense to proceed with:

```csharp
IO<int> CalculateWithSideEffect(string s)
{
    Action sideEffect = () => File.WriteAllText("output.txt", "I'm a side effect!");
    int PureComputation(string s) => s.Length;

    var pureComputationResult = PureComputation(s);
    return new IO<int>(pureComputationResult, sideEffect);
}
```

or more concisely:

```chsarp
IO<int> CalculateWithSideEffect(string s)
{
    return new IO<int>(
        s.Length,
        () => File.WriteAllText("output.txt", "I'm a side effect!"));
}
```

As for `IO`, let's implement the minimum that makes sense not to loose the information we as passing it:

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

We ended up with

```csharp
record IO<T>(T value, Action action);
    
IO<int> CalculateWithSideEffect(string s)
{
    return new IO<int>(
        s.Length,
        () => File.WriteAllText("output.txt", "I'm a side effect!"));
}

IO<int> length = CalculateWithSideEffect("foo");

Assert.Equal(3, length);
Assert.Equal("I'm a side effect!", File.ReadAllText("output.txt"));
```

`CalculateWithSideEffect()` is pure. Of course: we are cheating! The side effect hasn't been executed at all. The last `Assert` would fail.  
Even worse: this code won't even compile. See the problem? The result we get from `CalculateWithSideEffect()` is not an `int` anymore, so it cannot be compared with `3`.


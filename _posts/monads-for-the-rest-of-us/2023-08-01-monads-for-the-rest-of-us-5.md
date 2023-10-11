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
We once stated that composing monadic functions can be built on top of `Apply` / `bind` / `SelectMany`. Time to put that in practice.

# Compose for the IO monad
Let's compose 2 monadic functions together:
```haskell
f :: A -> IO<B>
g :: B -> IO<C>

Compose(g, f) :: A -> IO<C>
```

The implementation of `Compose` can be based on `Apply`:

```csharp
Func<A, IO<C>> Compose<A, B, C>(Func<B, IO<C>> g, Func<A, IO<B>> f)
{
    return new Func<A, IO<C>>(a =>
    {
        IO<B> aa = f(a);
        IO<C> io = Apply(g, aa);
        return io;
    });
}

var composed = Compose<string, int, double>(DoubleWithSideEffect, LengthWithSideEffect);

IO<double> monadicResult = composed("foo");

var result = monadicResult.Run();

Assert.Equal(3*2, result);
Assert.Equal("I'm a side effect!I'm another side effect!", File.ReadAllText("output.txt"));
```



# What about the other monads?
What you have obtained with the IO monad can be easily done with other kinds of side-effect. Let's see some examples.



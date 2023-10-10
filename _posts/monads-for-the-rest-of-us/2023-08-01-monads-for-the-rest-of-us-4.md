---
layout: post
title: "Monads for the rest of us, in C# - Part 4"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- C#
- Functional Programming
include_in_index: false
---
Let's summarize our understanding of an IO monadic function:

* instead of executing the IO side effect, it returns a monadic value that models it. Basically, the IO side effect passed as a lambda, so its execution is deferred
* 2 IO monadic functions cannot be applied and composed using the ordinary `Apply` and `Compose`, because their types are not directly compatible: we need to write a monadic version of `Apply` and `Compose`
* writing `Apply` is enough: `Compose` can be defined in terms of `Apply`

# Running an IO monadic function
The very last ingredient we need is:

* nothing prevents us from defining a function that *executes* the IO monad. We are deferring the execution of the IO side effect, but eventually, at the end of the chain, we need to run it.

In a sense: an IO monadic function is a way to defer the execution of an IO side effect and to manipulate and combine functions as pure entities, running its unpure behavior only at the edge of the application.

Let's extend `IO<B>` with a method `Run`, which finally executes the side effect:
So, we left with:

```csharp
record IO<B>(B value, Action action)
{
    internal B Run()
    {
        action.Invoke();
        return value;
    }
}

IO<int> CalculateWithSideEffect(string s) =>
    new IO<int>(
        s.Length,
        () => File.WriteAllText("output.txt", "I'm a side effect!"));

// This is still a pure function
IO<int> monadicValue = CalculateWithSideEffect("foo");

// Indeed, no file has been created yet
Assert.False(File.Exists("output.txt"));

var result = monadicValue.Run();

Assert.Equal(3, result);
Assert.Equal("I'm a side effect!", File.ReadAllText("output.txt"));
```


# Functional Programming is all about side effects
Wait a minute: wasn't Functional Programming about *not* having side effects?  
Not at all! Stealing words from Eric Normand's [Grokking Simplicity][grokking-simplicity]:

> The definition says FP avoids side effects, but side effects are the
> very reason we run our software. What good is email software that
> doesn’t send emails? [...]
>
> Functional programmers know side effects are necessary yet prob-
> lematic, so we have a lot of tools for working with them. The defi-
> nition implies that we only use pure functions. On the contrary, we
> use impure functions a lot. We have a ton of functional techniques
> that make them easier to use.

# Abstracting side effects away
So, what's the benefit of an IO monadic function? Are we just deferring the execution of side effects?  
In the case of IO, yes: basically, that's the trick.

But there is another important aspect I haven't mentioned yet. You remember how we fantasized with the possibility of defining other kinds of impurity:

| Case                                                                  | Example of type                   |
|-----------------------------------------------------------------------|-----------------------------------|
| A function that depends (reads) an extra string parameter             | `string -> Reader<String, int>`   |
| A function that might raise an exception                              | `decimal -> Error<decimal>`       |
| A function that also writes to the Console                            | `[string] -> IO<int>`             |
| A function that could fail to return a value                          | `string -> Maybe<int>`            |
| A function returning non-deterministic values                         | `string -> NonDeterministic<int>` |
| A function returning a value and also writing a double somewhere else | `string -> Writer<double, int>`   |
| A function which depends and updates a shared state                   | `string -> State<MyState, int`    |


For each of those monadic types, `Reader`, `Writer`, `NonDeterministic` etc, we will need to define a specific implementation of `Apply`.  
Think about it like this: all those kinds of impurity will be abstracted away behind the very same interface; you will be able to manipulate all of them, as pure functions, using the very same `Apply` and `Compose`, regardless which specific impurity they deal with.  
This is in fact the key to segregate your pure domain logic from whatever source of impurity your application will need to deal with. It's about pushing impurity outside your code, while aknowledging it *does exist* and it must be handled with great gravity.

Look, this listing &mdash; based on [language-ext][language-ext] &mdash; performs some pure calculations with functions also performing synchronous IO side effects:

```csharp
Eff<int> Computation1() => ...
Eff<int> Computation2() => ...
Eff<int> Computation3() => ...

Eff<int> result =
    from value1 in Computation1()
    from value2 in Computation2(value1)
    from value3 in Computation3(value2, value1)
    select value2;
```

The following, instead, performs the same calculations, but each function might not return any result at all:

```csharp
Option<int> Computation1() => ...
Option<int> Computation2() => ...
Option<int> Computation3() => ...

Option<int> result =
    from value1 in Computation1()
    from value2 in Computation2(value1)
    from value3 in Computation3(value2, value1)
    select value2;
```

Here, each function might fail raising an instance of `Error`:

```csharp
Either<Error, int> Computation1() => ...
Either<Error, int> Computation2() => ...
Either<Error, int> Computation3() => ...

Either<Error, int> result =
    from value1 in Computation1()
    from value2 in Computation2(value1)
    from value3 in Computation3(value2, value1)
    select value2;
```

Here, each function is non-deterministic in nature, and it returns more than one value. The calculation will be performed on all the possible combinations:

```csharp
IEnumerable<int> Computation1() => ...
IEnumerable<int> Computation2() => ...
IEnumerable<int> Computation3() => ...

IEnumerable<int> result =
    from value1 in Computation1()
    from value2 in Computation2(value1)
    from value3 in Computation3(value2, value1)
    select value2;
```

You see the pattern? The specific *effect* does not affect the shape of your code; you can focus on the pure computations and let the type system model and deal with any extra side effect.  
That's the gist of monads.

# A working Apply for the IO monad
We are finally ready to write the monadic version of `Apply`.  
If `Apply` for ordinary functions has the signature:

```haskell
Apply :: (A -> B) -> A -> B
```

the IO monadic version would be


```haskell
Apply :: (A -> IO<B>) -> IO<A> -> IO<B>
```

Interpret is as:

* given a monadic function from `A` to `IO<B>`
* we don't have a value of type `A` to feed it with
* instead, we have a monadic value of type `IO<A>`, returned by a previously executed function
* we apply the monadic function `A -> IO<B>` to the monadic value `IO<A>`
* so we get the new monadic value `IO<B>`

Its implementation is actually trivial:

```csharp
IO<B> Apply<A, B>(Func<A, IO<B>> f, IO<A> a) => 
    new IO<B>(() =>
    {
        A aResult = a.Run();
        IO<B> bResult = f(aResult);
        return bResult.Run();
    });
```

It works as follows:

* `Apply` returns a new instance of `IO<B>`. The `IO` constructor takes a lambda: this means that the code we are passing to it is not going to be executed just yet. Any side effect will be deferred
* This allows us to run `IO<A> a`. As per its nature, this
  * will produce some side effects
  * will return back the value (of type `A`) of the pure computation
* A value of type `A` is compatible with the signature of `f`: it's easy to apply `f` to it, with the native C# function application
* `f` is a monadic function, so what we get back it an IO monad
* 


Here's a complete use case in which there are 2 monadic functions:

```haskell
LengthWithSideEffect :: string -> IO<int>
DoubleWithSideEffect :: int -> IO<double>
```

As stated by their signature, each function performs some IO side effects, other than performing a pure calculation.

If the functions were pure, we could directly combine them as follows:


```csharp
// Length :: string -> int
// Double :: int -> double

var doubleTheLength = Double(Length("foo"));

Assert(6, doubleTheLength);
```

or, with our custom `Apply`:

```csharp
var doubleTheLength = Apply(Double, Apply(Length, "foo"));

Assert(6, doubleTheLength);
```

With the monadic case, `LengthWithSideEffect` returns an `IO<int>`, which is not directly compatible with the type `DoubleWithSideEffect` wants.

The monadic version of `Apply` we just wrote takes care of *binding* the 2 monadic functions:

```csharp
IO<int> LengthWithSideEffect(string s) =>
    new IO<int>(
        () =>
        {
            File.WriteAllText("output.txt", "I'm a side effect!");
            return s.Length;
        });

IO<double> (int n) =>
    new IO<double>(
        () =>
        {
            File.AppendAllText("output.txt", "I'm another side effect!");
            return n * 2;
        });


IO<B> Apply<A, B>(Func<A, IO<B>> f, IO<A> a) => new(() =>
{
    A aResult = a.Run();
    IO<B> bResult = f(aResult);
    return bResult.Run();
});

IO<string> Return(string s) => new(() => s);

var apply = Apply(LengthWithSideEffect, Return("foo"));

IO<double> monadicResult = Apply(DoubleWithSideEffect, apply);

// Indeed, no file has been created yet
Assert.False(File.Exists("output.txt"));

var result = monadicResult.Run();

Assert.Equal(3*2, result);
Assert.Equal("I'm a side effect!I'm another side effect!", File.ReadAllText("output.txt"));
```

Have you noticed that I used the expression "*binding 2 monadic functions*"?  
The choice of the word *binding* was not random. Indeed, `Apply` in Haskell is implemented with the [`>>=` operator][haskell-bind], which reads exactly as *bind*.




# References
* [language-ext][language-ext]
* [Eric Normand - Grokking Simplicity][grokking-simplicity]
* [Bind in Haskell][haskell-bind]

[language-ext]: https://github.com/louthy/language-ext
[grokking-simplicity]: https://grokkingsimplicity.com/
[haskell-bind]: https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:-62--62--61-

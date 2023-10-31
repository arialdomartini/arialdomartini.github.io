---
layout: post
title: "Monads For The Rest Of Us - Part 4"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- C#
- Functional Programming
include_in_index: false
---
## In which you liberate C# from IO side effects

Let's summarize our understanding of an IO monadic function:

* Instead of executing the IO side effect, it returns a monadic value that models it. Basically, the IO side effect is passed as a lambda, so its execution is deferred.
* 2 IO monadic functions cannot be applied and composed using the ordinary `Apply` and `Compose` C# features, because their types are not directly compatible: we need to write a monadic version of `Apply` and `Compose`.
* Writing `Apply` is enough: `Compose` can be defined in terms of `Apply`.

# Running an IO monadic function
Here's the very last ingredient we need: nothing prevents us from defining a function that *executes* the IO monad. Sure: we are deferring the execution of the IO side effect; but, eventually, at the end of the chain, we need to run it, don't we?

Here's (yet) another intuition: IO monads are a way to delay the execution of side effects as long as we wish to manipulate the functions as pure entities, and to finally run their impure behavior when we are done, at the edge of the application.

Let's extend `IO<B>` with a method `Run`, which finally executes the side effect:


```csharp
record IO<B>(Func<B> f)
{
    internal B Run() => f();
}

IO<int> CalculateWithSideEffect(string s) =>
    new IO<int>(() => 
    {
        File.WriteAllText("output.txt", "I'm a side effect!");
        return s.Length;
    });

// This is still a pure function
IO<int> monadicValue = CalculateWithSideEffect("foo");

// Indeed, no file has been created yet
Assert.False(File.Exists("output.txt"));

// Finally, the IO monadic value is run
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
So, what's the benefit of monadic functions? Are we just deferring the execution of side effects?  
In the case of IO, yes: basically, that's the trick.

But there is another important aspect I haven't mentioned yet. You remember how we fantasized about the possibility of defining other kinds of impurity:

| Case                                                                  | Example of type                   |
|-----------------------------------------------------------------------|-----------------------------------|
| A function that depends (reads) an extra string parameter             | `string -> Reader<String, int>`   |
| A function that might raise an exception                              | `decimal -> Error<decimal>`       |
| A function that also writes to the Console                            | `[string] -> IO<int>`             |
| A function that could fail to return a value                          | `string -> Maybe<int>`            |
| A function returning non-deterministic values                         | `string -> Nondeterministic<int>` |
| A function returning a value and also writing a double somewhere else | `string -> Writer<double, int>`   |
| A function which depends and updates a shared state                   | `string -> State<MyState, int`    |


For each of those monadic types, `Reader`, `Writer`, `Nondeterministic` etc, we will end up defining a specific implementation of `Apply`.  
Think about it like this: all those kinds of impurity will be abstracted away behind the very same interface; you will be able to manipulate all of them, as pure functions, using the very same `Apply` and `Compose`, regardless which specific impurity they deal with.  
This is in fact the key to segregate your pure domain logic from whatever source of impurity your application needs to deal with. It's about pushing impurity outside your code, while aknowledging it *does exist* and it must be handled with great gravity.

Look, this listing. It is based on [language-ext][language-ext] and it uses LINQ expressions as the main costructs. It combines 3 functions whose type signature signal they are performing synchronous IO side effects:

```csharp
Eff<int> Computation1() => ...
Eff<int> Computation2() => ...
Eff<int> Computation3() => ...

Eff<int> result =
    from value1 in Computation1()
    from value2 in Computation2(value1)
    from value3 in Computation3(value2, value1)
    select value3;
```

In the following, instead, each function might not return any result at all:

```csharp
Option<int> Computation1() => ...
Option<int> Computation2() => ...
Option<int> Computation3() => ...

Option<int> result =
    from value1 in Computation1()
    from value2 in Computation2(value1)
    from value3 in Computation3(value2, value1)
    select value3;
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
    select value3;
```

Here, each function is non-deterministic and it returns more than one value. The computation will be performed on all the possible combinations:

```csharp
IEnumerable<int> Computation1() => ...
IEnumerable<int> Computation2() => ...
IEnumerable<int> Computation3() => ...

IEnumerable<int> result =
    from value1 in Computation1()
    from value2 in Computation2(value1)
    from value3 in Computation3(value2, value1)
    select value3;
```

You see the pattern? The specific *effect* does not affect the shape of your code; you can focus on the pure computation and let the type system model and deal with any extra side effect.  
That's the gist of monads.

# A working Apply for the IO monad
You managed to obtain a way to defer the IO side-effects (the constructor of the IO Monad) and a way to finally execute them, `Run`. Now you just need a way to manipulate the IO monadic functions as pure functions in between. Let's then write the monadic version of `Apply` / `Bind`.  
If `Apply` for ordinary functions has the signature:

```haskell
Apply :: (A -> B) -> A -> B
```

the IO monadic version would be


```haskell
Apply :: (A -> IO<B>) -> IO<A> -> IO<B>
```

Interpret is as:

* Given a monadic function from `A` to `IO<B>`
* we don't have a value of type `A` to feed it with
* instead, we have a monadic value of type `IO<A>`, most likely returned by a previously executed monadic function
* we apply the monadic function `A -> IO<B>` to the monadic value `IO<A>`
* so we get the new monadic value `IO<B>`

Notice: very importantly, the function is not returning a `B`, but still a `IO<B>`. Why is that?  
Because we are still refraining from executing the side effect until `Run` is intentionally invoked.  
For the same reason, although the monadic function type signature `A -> IO<B>` signals that it expects a simple `A` value, `Apply` feeds it with an `IO<A>`: this is because this value is the result of the previously executed monadic function.  
It's monads all the way down.

The implementation of `Apply` is only apparently intimidating:

```csharp
IO<B> Apply<A, B>(this Func<A, IO<B>> f, IO<A> a) => 
    new IO<B>(() =>
    {
        A aResult = a.Run();
        IO<B> bResult = f(aResult);
        return bResult.Run();
    });
```

It works as follows:

* `Apply` returns a new instance of `IO<B>`. The `IO` constructor takes a lambda: this means that the code we are passing to it is not going to be executed just yet. Any side effect will be deferred.
* This allows us to safely run `IO<A> a`. As per its nature, this
  * will produce some side effects
  * will return back the `A` value of the pure computation
* A value of type `A` is compatible with the signature of `f`: it's easy to apply `f` to it, with the native C# function application.
* `f` is a monadic function, so what we get back is an IO monad.
* This is run too, so its side effect is executed and the pure computation value is finally returned. 


Here's a complete use case in which there are 2 monadic functions:

```haskell
length :: string -> IO<int>
double :: int -> IO<double>
```

If the functions were pure, we could directly combine them as follows:


```csharp
// length :: string -> int
// double :: int -> double

var doubleTheLength = @double(length("foo"));

Assert(6, doubleTheLength);
```

or, with our custom `Apply`:

```csharp
var doubleTheLength = double.Apply(length.Apply("foo"));

Assert.Equal(6, doubleTheLength);
```

In the monadic case, the version of `Apply` we just wrote takes care of *binding* the 2 monadic functions:

```csharp
Func<string, IO<int>> length = s =>
    new IO<int>(() =>
    {
        File.WriteAllText("output.txt", "I'm a side effect!");
        return s.Length;
    });

Func<int, IO<double>> @double = n =>
    new IO<double>(() =>
    {
        File.AppendAllText("output.txt", "I'm another side effect!");
        return n * 2;
    });


static class FunctionExtensions
{
    internal static IO<B> Apply<A, B>(this Func<A, IO<B>> f, IO<A> a) => new(() =>
    {
        A aResult = a.Run();
        IO<B> bResult = f(aResult);
        return bResult.Run();
    });
}

IO<int> monadicLength = length("foo");
IO<double> monadicResult = @double.Apply(monadicLength);

// Indeed, no file has been created yet
Assert.False(File.Exists("output.txt"));

var result = monadicResult.Run();

Assert.Equal(3*2, result);
Assert.Equal("I'm a side effect!I'm another side effect!", File.ReadAllText("output.txt"));
```

`Apply` in Haskell is implemented with the [`>>=` operator][haskell-bind], which not surprisingly reads as *bind*.  
The code above in Haskell would be:

```haskell
(length "foo") >>= double 
```

or equivalently:

```haskell
do
  len <- length "foo"
  double len
```

You will soon find out that LINQ implements `>>=` calling it `SelectMany`, and that this is the secret ingredient that allows you to rewrite the code above as:

```csharp
Eff<int> monadicResult =
    from len in length("foo")
    from d in double(len)
    select d;
```

Don't try this just yet: you need to define a couple of Extension Methods for LINQ to learn how to deal with your custom monads. We will see this later.  
Instead, take a minute to ruminate on the code you obtained:

```csharp
IO<double> result = double.Apply(length("foo"));
```

It is similar to:

```csharp
double result = double(length("foo"));
```

and just identical to the one using the pure `Apply` we defined in [Part 3](monads-for-the-rest-of-us-3):

```csharp
double result = double.Apply(length("foo"));
```

with the only difference it returns `IO<double>` instead of `double`.

So:

* It deals with strictly pure functions only.
* The evidence that it peforms IO is explicit in the type signature, so the compiler can make it sure it is taken into consideration.
* It still allows you to combine and manipulate functions the way you were used to do.

You liberated your C# code from IO effects, abstracting them away, not swepting the problem under the rug.  
Not a bad result, indeed!

# Compose for the IO monad
A last effort to complete the journey. To compose 2 monadic functions together:

```haskell
f :: A -> IO<B>
g :: B -> IO<C>

Compose(f, g) :: A -> IO<C>
```

we can think of an implementation like the following:

```csharp
static class FunctionExtensions
{
    internal static Func<A, IO<C>> ComposedWith<A, B, C>(this Func<B, IO<C>> g, Func<A, IO<B>> f)
    {
        return a =>
        {
            IO<B> ioB = f(a);
            B b = ioB.Run();
            IO<C> c = g(b);
            return c;
        };
    }
}

var composed = @double.ComposedWith(length);

IO<double> monadicResult = composed("foo");
var result = monadicResult.Run();

Assert.Equal(3*2, result);
Assert.Equal("I'm a side effect!I'm another side effect!", File.ReadAllText("output.txt"));
```

It should not be too hard to grasp:

* First of all, notice from the signature and the `return a =>` that we are returning a new function.
* Inside the new function's body, you first apply `f(a)`. `f` is a monadic function, so you get back an IO monad.
* Run it, so you execute the side effects and you get back a `B` value.
* It's easy to pass the `B` value to `g`.
* `g` gets you back an `IO<C>`. That's fine: this is already compatible with the expected returned type.


As we said, `ComposedWith` can easily be implemented using `Apply`:

```csharp
IO<B> Apply<A, B>(this Func<A, IO<B>> f, IO<A> a)
{
    A run = a.Run();
    IO<B> apply = f(run);
    return apply;
}


Func<A, IO<C>> ComposedWith<A, B, C>(this Func<B, IO<C>> f, Func<A, IO<B>> g)
{
    return a =>
    {
        IO<B> ioB = g(a);
        IO<C> ioC = f.Apply(ioB);
        return ioC;
    };
}
```

This is a bit more challenging to understand. Follow the types, that should help.   
When you are done, inline all the variables and you will get to:

```csharp
IO<B> Apply<A, B>(this Func<A, IO<B>> f, IO<A> a) 
    => new(() => f(a.Run()).Run());

Func<A, IO<C>> ComposedWith<A, B, C>(this Func<B, IO<C>> f, Func<A, IO<B>> g) =>
    a => f.Apply(g(a));
```

That's a typical outcome in the Functional Programming world: pages and pages of deep contemplation and deconstruction of a topic, only to end up with a single-line code formula.

# You made it!
That was an IO monad. There are of course a bunch of details we passed over &mdash; the monad laws, the `return` operation, the relation between monads, functors and applicatives, and the like &mdash; but I hope you found the topic less intimidating than you expected.

Good job! Ready for the next round?  
The next topics are:

* What about the different kinds of inpurity?
* Besides monads, what are Functors?

Observing other Monads will unlock a multitude of possibilities for expressiveness, and will hopefully introduce you to a new programming style. You are warned, though: I'll only take you to the rabbit hole's entrance, it's very, very deep &mdash; but filled with rewards.  
Learning Functors, on the other hand, will be relaxing and reassuring. They are very easy, have a wide range of applications and, best of all, you probably already know most of the topic intuitivelly.  
Finally, Functors will allow you to visually grasp some concepts that might currently appear a bit vague.

But before proceeding, go and have an ice-cream: you deserved it!

Geto to [Chapter 5](monads-for-the-rest-of-us-5).

# References
* [language-ext][language-ext]
* [Eric Normand - Grokking Simplicity][grokking-simplicity]
* [Bind in Haskell][haskell-bind]

[language-ext]: https://github.com/louthy/language-ext
[grokking-simplicity]: https://grokkingsimplicity.com/
[haskell-bind]: https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:-62--62--61-

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/26)

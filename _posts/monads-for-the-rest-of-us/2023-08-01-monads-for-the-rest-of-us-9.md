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
Implementing `map` for a specific functor is often easy if you reason about the type signature. Try writing it and the rest should fall into place.  
Let's do that for `IO`, `Nond` and `Maybe`. You will see that it's an easy exercise.

# Maybe, as a Functor
Tutorials often start with the maybe functor because it's, objectively, the simplest one. I will do the same.

Given a function:

```haskell
f :: a -> b
```

it should return

```haskell
map(f) :: Maybe<A> -> Maybe<B>
```

Try yourself to complete the implementation:

```csharp
Func<Maybe<A>, Maybe<B>> Map<A, B>(this Func<A, B> f) => ...
```

It must return a function `Maybe<A> -> Maybe<B>`. Therefore:

```csharp
Func<Maybe<A>, Maybe<B>> Map<A, B>(this Func<A, B> f) =>
        (Maybe<A> maybeA) => ...
```

What to do with `maybeA`? Well, we could easily pattern match on it and take 2 different paths based on whether it contains a value or not:

```csharp
Func<Maybe<A>, Maybe<B>> Map<A, B>(this Func<A, B> f) =>
    (Maybe<A> maybeA) => maybeA switch
    {
        Just<A> a => ...
        Nothing<A> => ...
    };
```

If there is no value, it makes sense to propagate the absence of a value, returning a `Nothing<B>`

```csharp
Func<Maybe<A>, Maybe<B>> Map<A, B>(this Func<A, B> f) =>
    (Maybe<A> maybeA) => maybeA switch
    {
        Just<A> a => ...
        Nothing<A> => new Nothing<B>()
    };
```

If there is a value, we can apply `f` to it to get a `B` value. Since the function is supposed to return a `Maybe<B>`, we elevate the `B` value as a `Just<B>`:

```csharp
Func<Maybe<A>, Maybe<B>> Map<A, B>(this Func<A, B> f) =>
    (Maybe<A> maybeA) => maybeA switch
    {
        Just<A> a => new Just<B>(f(a.Value)),
        Nothing<A> => new Nothing<B>()
    };
```

We could have used the implementation of `Run`:

```csharp
Func<Maybe<A>, Maybe<B>> Map<A, B>(this Func<A, B> f) =>
    maybeA =>
        maybeA.Run<Maybe<B>>(
            just: a => new Just<B>(f(a)),
            nothing: () => new Nothing<B>());
```

And that's it.  
It works as follows:

```csharp
// given a value that may or may not contain a string
Maybe<string> maybeAString = new Just<string>("foo");

// and a function to calculate the length of a string
Func<string, int> length = s => s.Length;

// Map elevates length to work on Maybe values
Func<Maybe<string>,Maybe<int>> lengthF = length.Map();

// So we can calculate the length of a Maybe<string>.
// if the value does not exist, we will get a Nothing<int>
var maybeLength = lengthF(maybeAString);

Assert.IsType<Just<int>>(maybeLength);
Assert.Equal(3, ((Just<int>) maybeLength).Value);
```

The Maybe Monad is one of those that lends itself very well to being interpreted with the metaphor of the box. You can imagine it as a box either empty (in the case of `Nothing<A>`) or containing a value (in the case of `Just<A>`).  
You can make this apparent defining an extension method on it:

```csharp
Maybe<B> Map<A, B>(this Maybe<A> maybeA, Func<A, B> f) =>
    f.Map()(maybeA);
```

which lets you use `Maybe` as follows:

```csharp
Maybe<string> maybeAString = new Just<string>("foo");

Func<string, int> length = s => s.Length;


var maybeLength = maybeAString.Map(length);

Assert.IsType<Just<int>>(maybeLength);
Assert.Equal(3, ((Just<int>) maybeLength).Value);
```

I guess you can see how:

```csharp
maybeAString.Map(length)
```

resembles the LINQ's expression:

```
maybeAString.Select(length)
```

In fact, LINQ's `Select` *is* the implementation of `Map` for the functor `IEnumerable`.


# Nond, as a Functor
For `Nond`, let's start from a test:

```csharp
var nondeterministicString = new Nond<string>(new[]
    { "foo", "bar", "barbaz" });

Func<string, int> length = s => s.Length;

Func<Nond<string>, Nond<int>> lengthM = length.Map();

var results = lengthM(nondeterministicString).Run();

Assert.Equal(new []{3, 3, 6}, results);
```

Try to implement `Map`. You should get something like:

```csharp
Func<Nond<A>, Nond<B>> Map<A, B>(this Func<A, B> f) => nondA =>
{
    var values = nondA.Run();
    IEnumerable<B> enumerable = values.Select(f);
    return new Nond<B>(enumerable);
};
```

which is simply equivalent to:

```csharp
Func<Nond<A>, Nond<B>> Map<A, B>(this Func<A, B> f) => nondA =>
    new Nond<B>(nondA.Items.Select(f));
```

Actually, it is easy to convince oneself that the `Nond` class is redundant, and that `IEnumerable` can be directly used as the nondeterministic Monad and Function, thanks to the native LINQ support.

LINQ's `Select` is the most glaring example of the boring box metaphor. It's very easy to see `IEnumerable` as a box containing an arbitrary number of values, and `Select` as a way to apply a function to them from outside the container.


# IO Functor
And finally, let's distill `Map` for `IO`. The approach is the same: start from the signature, and try to follow how it leads you.  
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

I'm not particularly fond of the box metaphor, but if you really want to see IO trough that lens, sqeeze your eyes and imagine it as a box containing the value it will eventually generate when run (together with side-effects).  
It helps to to define an extension method that takes the IO Monad as the first parameter:

```csharp
static IO<B> Map<A, B>(this IO<A> ioa, Func<A, B> f) =>
    f.Map()(ioa);
```

Then, you use it as follows:

```csharp
IO<int> l = io.Map(length);
```

Read `io.Map(length)` as:

* map the function `length : string -> int`
* to the content of the box `io`, which contains / will eventually produce a `string`
* preserving the side effects

This is equivalent of using LINQ as the following:

```csharp
IO<int> l = io.Select(length); 
```


# That's it
Wow, you did it!  
Those 9 chapters were quite a mouthful, right? Now that you've dined on monads, reward yourself with a slice of meringue pie!  
You've really earned it!

# What's next?
There are some topics I didn't get around to covering.

* How to deal with multi-parameter functions, with currying and partial application.
* How to use LINQ to bind any custom monadic functions in a fluent way
* Implementation of more monads, such as `Either`, `State`, `Reader` and `Writer`.

I promise I will follow up with a new series.  
Cheers!

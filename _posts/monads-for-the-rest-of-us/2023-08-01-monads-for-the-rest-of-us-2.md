---
layout: post
title: "Monads for the rest of us, in C#"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- C#
- Functional Programming
include_in_index: false
---
# Function Application and Function Composition
The key of Monads is to use the type system to separate out side-effecting computations from pure computations, so that they do not interfere with each other.

Let's get started characterizing the main notions about pure functions useful for our case: Function Application and Function Composition. It turns out these are all you need to re-implement to get Monads.

## Function Application
Consider the method:

```csharp
int MyLength(string s)
{
    return s.Length;
}
```

We can easily rewrite it as a function as:

```csharp
Func<string, int> myLength = s => s.Length;
```

This makes it a bit clearer that its type signature is:

```haskell
myLength :: String -> Int
```

We want to apply `myLength` to a value of type `String` to get back an `Int`.  
In C# that's a trivial exercise, as Function Application is natively supported by the language:

```csharp
var length = MyLength("foo");
Assert.Equal(3, length);
```

Why are we talking about such a basic operation such as function application?  
In the next pages we are going to define *pure functions with a signature modelling the side effects they perform* (also known as "*monadic functions*"), and we will need to figure out how to apply them to values. We will discover that function application with those fancy functions is not natively supported by C#, and that a specific implementation is needed.

So, it could be interesting to see if we are able to manually re-implement the native C# Function Application, so we can possibly extend it. Indeed, our implementation will be the basis for the future *Monadic* Function Application.  
Let's then write a High Order Function (HOF) that taken a function `f :: String -> Int` and a `String` value `a` applies `f` to `a` returning an `Int` result:

```csharp
Func<string, int> myLength = s => s.Length;
string s = "foo";

int Apply(Func<string, int> f, string a) => f(a);

var length = apply(myLength, s);

Assert.Equal(3, length);
```

It's easy to make it generic, so it works with any function `f :: a -> b` whatever the types `a` and `b` are:

```csharp
// apply :: (A -> B) -> A -> B
B Apply<A, B>(Func<A, B> f, A a) => f(a);
```

Take a few seconds to meditate on what we just wrote. Not surprisingly, we have discovered that Function Application is implemented as `f(a)`.  
In Haskell, `apply` is written as `$` at its (simplified) [implementation][haskell-apply-implementation] is:

```haskell
($) :: (a -> b) -> a -> b
($) f a =  f a
```

### What we got
It may seem that `apply` is a useless redundant function, but it's not:

* it can be extended, giving you the opportunity to do *something else* while applying a function to a value. For example, you can decorate the invocation surrounding it with some logging calls: 

```csharp
B Apply<A, B>(Func<A, B> f, A a)
{
    Log.Information("Got a value {A}", a);
    var b = f(a);
    Log.Information("Returning a value {B}", b);
    return b;
}
```

* it gives you the possibility to extend the very meaning of Function Application. You will soon see that with Monads you will need a special `apply` implementation that is able to apply functions to incompatible value types.


### Function Application of multi-parameter functions

The version of `apply` we got only works with 1-parameter functions. The following code does not even compile:

```csharp
int F(string s, string z) => s.Length + z.Length;

apply(F, "foo", "bar");
```

It turns out that it is always possible to reduce multi-parameter functions to single-parameter ones, with a technique called *currying*. We will see this later.

## Function Composition
The second fundamental notion we are interested to re-implement is Function Composition.  
Consider the following:

```csharp
Func<string, int> length = s => s.Length;
Func<int, decimal> halfOf = n => (decimal)n / 2;

decimal halfTheLength = halfOf(length("foo"));
        
Assert.Equal(1.5M, halfTheLength);
```

With `halfOf(length("foo"))` we first apply `length()` to `"foo"`, we get `3` as result and then we apply `halfOf()` to it.  
This is equivalent to directly writing a custom `halfOfLength()` function that performs the same 2 computations *combined* in a single step:

```csharp
Func<string, decimal> lengthThenHalfOf = s =>
{
    var l = s.Length;
    var halfOfIt = (decimal)l / 2;
    return halfOfIt;
};

var halfTheLength = lengthThenHalfOf("foo");

Assert.Equal(1.5M, halfTheLength);
```

Have a look to the signatures:

```charp
length           :: string -> int
halfOf           :: int    -> decimal
lengthThenHalfOf :: string -> decimal
```

Let's write a function that, given a `string -> int` function such as `length` and a `int -> decimal` such as `halfOf` *composes* the two in a `string -> decimal` function:

```csharp
Func<string, int> length = s => s.Length;
Func<int, decimal> halfOf = n => (decimal)n / 2;

Func<string, decimal> compose(Func<string, int> length, Func<int, decimal> halfOf) => s => halfOf(length(s));
        
var halfOfLength = compose(length, halfOf);
var halfTheLength = halfOfLength("foo");

Assert.Equal(1.5M, halfTheLength);
```

Of course, this works with any `f :: string -> int` and `g :: int -> decimal` functions, so we can safely rename `halfOf` and `length` to something more generic:

```csharp
Func<string, decimal> Compose(Func<string, int> f, Func<int, decimal> g) => a => g(f(a));
```

Talking about being generic, we can in fact make this function generic on its types, so that given two functions `f :: a -> b` and `g :: b -> c` it composes them into a `gComposedWithf :: a -> c` composite function:

```csharp
// compose :: (a -> b) -> (b -> c) -> (a -> c)
Func<A, C> Compose<A, B, C>(Func<A, B> f, Func<B, C> g) => a => g(f(a));
```

In Haskell, `Compose` is written as `.` at [its implementation][haskell-composition-implementation] is:

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)
```

It's essentially the same, besides the parameters being swapped in their positions.

### What we got
As it happened with `Apply`, with the formula `Compose(f, g) => a => f(g(a))` we have just reinvented the weel.  
And yet, our little `Compose` implementation is not for nothing:

* It is slightly more more powerful than the native C# feature.  
C# does not exacly implement function composition. `f(g(a))` composes 2 functions and then also *applies* the resulting function to a value. Our `Compose()` function is more humble and interesting: it is a High Order Function that composes 2 generic, single-parameter functions, returning back a new function, *without* applying it.

* As for `Apply()`, the manually implemented `Compose()` gives us the opportunity to do *something else* in addition to composing functions.

* Finally, as for `Apply()`, `Compose()` gives us the chance to redefine the very meaning of Function Composition.  
For example, we could work it out to compose functions with not exactly compatible signatures.  
And this turns out to be exactly the key for implementing and undestanding Monads.

# References

* [Mike Vanier][yet-another-tutorial]
* [Implementation of ($) in Haskell][haskell-apply-implementation]
* [Implementation of (.) in Haskell][haskell-composition-implementation]

[yet-another-tutorial]: https://mvanier.livejournal.com/3917.html
[haskell-composition-implementation]: https://hackage.haskell.org/package/base-4.18.1.0/docs/src/GHC.Base.html#.
[haskell-apply-implementation]: https://hackage.haskell.org/package/base-4.18.1.0/docs/src/GHC.Base.html#%24

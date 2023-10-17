---
layout: post
title: "Monads for the rest of us, in C#"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- C#
- Functional Programming
include_in_index: false
---
**In which you reimplement &mdash; and finally understand! &mdash; C#'s function application**

# Function Application and Function Composition
We learnt that Monads revolve around using the type system to separate out side-effecting computations from pure computations, so that they do not interfere with each other.  
We also found out that we need to apply and compose monadic functions, and that this is not directly supported by C#.

The goal of this 3rd installment is to manually re-implement the native C# function application and function composition, so that we learn how to extend them to work with monadic functions.

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
myLength :: string -> int
```

We want to apply `myLength` to a value of type `string` to get back an `int`.  
In C# that's a trivial exercise, as Function Application is natively supported by the language:

```csharp
var length = mylength("foo");

Assert.Equal(3, length);
```
Manually re-implementing the native C# Function Application might sound as a silly exercise, but it will be useful to learn how we can possibly extend it. Indeed, our implementation will be the basis for the future *Monadic* Function Application, which is not natively supported by C#.  
Let's then write a High Order Function (HOF) that taken a function `f :: string -> int` and a `string` value `a` applies `f` to `a` returning an `int` result:

```csharp
Func<string, int> myLength = s => s.Length;
string s = "foo";

int Apply(Func<string, int> f, string a) => f(a);

var length = Apply(myLength, s);

Assert.Equal(3, length);
```

The syntax can be improved defining `Apply` as an extension method:

```csharp
static class FunctionExtensions
{
    internal static int Apply(this Func<string, int> f, string s) => f(s);
}

Func<string, int> myLength = s => s.Length;

var length = myLength.Apply("foo");

Assert.Equal(3, length);
```

So, instead of:

```csharp
myLength("foo");
```

we ended up with


```csharp
myLength.Apply("foo");
```

Just slighly more verbose. By the way: C# implements this style of calling a function with the method `Invoke`:

```csharp
myLength.Invoke("foo");
```

We really didn't invent anything new.  
It's easy to make `Apply` generic, so it works with any function `f :: A -> B` whatever the types:

```csharp
// Apply :: (A -> B) -> A -> B
B Apply<A, B>(this Func<A, B> f, A a) => f(a);
```

Take a few seconds to meditate on what we just wrote. Not surprisingly, we have discovered that Function Application is implemented as `f(a)`.  
In Haskell, `Apply` is written as `$` at its (slightly simplified) [implementation][haskell-apply-implementation] is:

```haskell
($) :: (a -> b) -> a -> b
($) f a =  f a
```

### What we got
The implementation of `Apply` might seem of no use, but it's not:

* It can be extended, giving you the opportunity to do *something else* while applying a function to a value. For example, you can decorate the invocation surrounding it with some logging calls:


```csharp
B Apply<A, B>(Func<A, B> f, A a)
{
    Log.Information("Got a value {A}", a);
    var b = f(a);
    Log.Information("Returning a value {B}", b);
    return b;
}
```

The *something else* we are interested to do might be related to the extra-computation characterizing monadic functions, and this could be interesting.

* It gives you the possibility to extend the very meaning of Function Application. You will soon see that with monadic function you will need an `Apply` implementation able to work with type-incompatible functions.

Do you start to see a pattern? Monads are all about separating some *effects* in a type, and then handling them during function application and function composition.  
Keep going: we are almost there.

### Function Application of multi-parameter functions

The version of `Apply` we got only works with 1-parameter functions. The following code does not even compile:

```csharp
Func<string, string, int> f = (s, z) => s.Length + z.Length;

f.Apply("foo", "bar");
```

That's sounds like a discouraging constraint.  
It turns out, though, that it is always possible to reduce multi-parameter functions to single-parameter ones, with a technique called *currying*.  
Basically, it's a way to automatically convert the previous function to:

```csharp
Func<string, Func<string, int>> f = s => z => s.Length + z.Length;

f.Apply("foo")("bar");
```

Don't despair, we will see this later.

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

Function Composition is about generating `lengthThenHalfOf` automatically, as a combination of `length` and `halfOf`, without writing its implementation by hand. Even better, it's about generating a combination of *any* 2 functions, no matter their implementation and type signature, as long as the output of the one is type-compatible with the input of the other .  
So, let's write a function that, given any `string -> int` function such as `length` and a `int -> decimal` such as `halfOf` *composes* the two in a `string -> decimal` function:

```csharp
Func<string, int> length = s => s.Length;
Func<int, decimal> halfOf = n => (decimal)n / 2;

// (int -> decimal) -> (string -> int) -> (string -> decimal)
Func<string, decimal> Compose(Func<int, decimal> f, Func<string, int> g) => s => f(g(s));
        
Func<string, decimal> halfOfLength = Compose(halfOf, length);

Assert.Equal(1.5M, halfOfLength("foo"));
```

It's easy to make this function generic on its types, so that given two functions `f :: A -> B` and `g :: B -> C` it composes them into `gComposedWithf :: A -> C`:

```csharp
// Compose :: (B -> C) -> (A -> B) -> (A -> C)
Func<A, C> Compose<A, B, C>(Func<B, C> g, Func<A, B> f) => a => g(f(a));
```

Again, using an extension method slightly improves the syntax:

```csharp
static class FunctionExtensions
{
    internal static Func<A, C> ComposedWith<A, B, C>(this Func<B, C> g, Func<A, B> f) => a => g(f(a));
}

Func<string, decimal> halfOfLength = halfOf.ComposedWith(length);

Assert.Equal(1.5M, halfOfLength.Apply("foo"));
```

In Haskell, `Compose` is written as `.` at [its implementation][haskell-composition-implementation] is:

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)
```

It's essentially the same that we found.

### What we got
As for `Apply`, with the formula `Compose(f, g) => a => f(g(a))` we have just reinvented the weel.  
And yet, our little `Compose` implementation is not for nothing:

* It is slightly more more powerful than the native C# feature.  
C# does not exacly implement function composition. `f(g(a))` composes 2 functions and then also *applies* the resulting function to a value. Our `Compose` function is more humble and interesting: it is a High Order Function that composes 2 generic, single-parameter functions, returning back a new function, *without* applying it.

* As for `Apply`, the manually implemented `Compose` gives us the opportunity to do *something else* in addition to composing functions. And you know that the *something else* is what constitute the monadic part.

* Finally, as for `Apply`, `Compose` gives us the chance to redefine the very meaning of Function Composition.  
For example, we could find a way to compose functions with not exactly compatible signatures.  
And this turns out to be exactly the key for implementing and undestanding Monads.


# Apply as the main building block of function composition
Don't think to `Apply` merely as the way to pass an argument to a function. Go beyond that and consider how it is the fundamental way to *link* functions together: you use `Apply` to pass the result of the application of a previous function to the next one. Basically, it *binds* type-compatible functions in a chain. No surprises that, in the context of monadic functions, `Apply` is called `bind`.

Consider the following: 

```haskell
length :: string -> int
double :: int -> double
```

You want to apply `double` to the result of `length`.  
In C#:

```csharp
Func<string, int> length = s => s.Length;
Func<int, double> double = i => i * 2;

string a = "foo";

double doubleTheLength = double.Apply(length.Apply("foo"));

Assert.Equal(6, doubleTheLength);
```

The Haskell notation here is much clearer:

```haskell
doubleTheLength = double $ length $ "foo"
```

which is pretty much the same of the native C# function application:

```csharp
double doubleTheLength = double(length("foo"));
```

The result we get is the same we could get from a a single function composing `length` and `double`:

```haskell
length :: string -> int
double :: int -> double

chain :: string -> double
```

with :

```csharp
chain = double.ComposedWith(length)
```

This equivalence is not a coincidence. `Apply` is such a basic building block that `Compose` can be easily defined in terms of it:

```csharp
Func<A, C> Compose<A, B, C>(Func<B, C> f, Func<A, B> g) => 
    a => 
        f.Apply(g.Apply(a));
```

The gist of this is:

- if `Apply` binds type-compatible functions in a chain
- but we extended the notion of pure-computations with functions returning extended types
- so that the ordinary `Apply` does not work anymore,
- the key to Monads must be about extending `Apply` to work with those type-incompatible functions. 

Indeed, you can find several tutorials stating that monads are those classes that implement `bind` (as we know, a synomym of `Apply`). Actually, they also need a method for lifting ordinary values to their monadic equivalent, but that's a detail we will cover later.

Anyway: once you have `Apply`, you can easily get `Compose` too, and nothing can hold you back.  
We are ready to come back to the IO monadic function and make it finally work.

Proceed to [Chapter 4](monads-for-the-rest-of-us-4).

# References

* [Function Application in Haskell][haskell-apply-implementation]
* [Function Composition in Haskell][haskell-composition-implementation]

[haskell-apply-implementation]: https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:-36-
[haskell-composition-implementation]: https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:.

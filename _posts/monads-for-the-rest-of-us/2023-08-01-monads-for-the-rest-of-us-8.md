---
layout: post
title: "Monads for the rest of us, in C# - Part 6"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- C#
- Functional Programming
include_in_index: false
---
**in which you achieve true enlightenment seeing that Functors are not boxes**
You just discovered that `Bind` takes crippled functions, with a leg still clinging to the old world, and fix them elevating them so they are fully immersed in the realm of monads:

![a monadic function f from A to Monad B](static/img/nond-for-the-rest-of-us/monadic-functions-before-bind.png){: height="300px" }

What about even more stubborn functions that have both input and output in the non-monadic world?

![an ordinary function from A to B](static/img/nond-for-the-rest-of-us/functors-before-map.png){: height="300px" }

How can we fix them elevating to the monadic world?

We never thought that this was a problem. We always said that pure, ordinary functions are the ideal ones to deal with, the building blocks of Functional Programming.  
And that's true: by no means the desire to elevate them to the monadic realm implies they are problematic.

The function that takes:

```haskell
f :: A -> B
```

and lifts it to:

```haskell
f :: Monad<A> -> Monad<B>
```

is called `Map`:

![an ordinary function mapped to be from Monad A to Monad B](static/img/nond-for-the-rest-of-us/functors-after-map.png){: height="300px" }

`Map` is a notion related to Functors.

Let me tell you what a Functor is, using a bit of Category Theory.  
I promised that this series did not require any knowledge of Category Theory, and I won't go back on my word. I swear that it will be super easy.

# Functors
I love this image by Bartosz Milewski, from the chapter [Functors][bartosz-functors] of his book [Category Theory for Programmers][bartosz]:

![a functor as described in Category Theory](static/img/monads-for-the-rest-of-us/bartosz-functor.png){: height="300px" }

I slightly modified it adding a missing arrow.  
Interpret it as follows:

* There are 2 worlds, `C` and `D`. In our case, the world of ordinary functions and values, and the world of monadic functions.
* In the world of ordinary functions there is a function `f` from the type `a` to the type `b`
* In the world of the monadic functions, there is another function called `F f`, from the type `F a` to the type `F b`. In our previous diagrams we used the C# style, and we called them `Monad<A>` and `Monad<B>`. This does not alter the essence


A Functor is a mapping between these 2 worlds.  
In [Category Theory for Computing Science][barr-wells], Michael Barr and Charles Wells write:

"A functor `F : C -> D` is a pair of functions [...]" 

* mapping the types of `C` to the types of `D`
* and the functions of `C` to the functions of `D`

while preserving some rules, that I'm omitting for simplicity.

Let me stress again: a functor is *a pair of functions*. A functor is not a thing: it's two.

Of course, this is the mathematical definition. Projected to the programming language world, you need some way to implement this idea. No surprises that you can find a wide variety of implementations, from  classes implementing a `Functor` interface, like in [language-ext][language-ext]:

```csharp
[Typeclass("F*")]
public interface Functor<FA, FB, A, B> : Typeclass
{
    /// <summary>
    /// Projection from one value to another
    /// </summary>
    /// <param name="ma">Functor value to map from </param>
    /// <param name="f">Projection function</param>
    /// <returns>Mapped functor</returns>
    [Pure]
    FB Map(FA ma, Func<A, B> f);
}
```

or a type classe called `Functor`, like in Haskell:

```haskell
class Functor f where
    fmap        :: (a -> b) -> f a -> f b
```

(which captures both the mapping of functions, with `fmap` and the mapping of values, with `f`)

or traits, like in [Scala Cats][scala-cats-functor]:

```scala
trait Functor[F[_]] extends Invariant[F] { self =>
  def map[A, B](fa: F[A])(f: A => B): F[B]

  override def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] = map(fa)(f)
  final def fmap[A, B](fa: F[A])(f: A => B): F[B] = map(fa)(f)
  def widen[A, B >: A](fa: F[A]): F[B] = fa.asInstanceOf[F[B]]
  def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)
  [...]
}
```

The implementation is not essential, but it is interesting to notice how it often influences the way functors are described and defined. Here's a collection of examples from real random articles:

* "a functor is simply something that can be mapped over"
* "a Functor is some kind of box that can hold generic data and map a function"
* "a functor is a type that can map itself to another version of itself using a mapping function"
* "A functor is a container of values"

The most popular way to visualize functors is using boxes, as in [Functors, Applicatives, And Monads In Pictures][functors-as-boxes]:

![Functors as boxes](static/img/monads-for-the-rest-of-us/functors-as-boxes.png){: height="300px" }

This is all good. But I recommend: don't forget that these are only approximations and visualization stratagems.  
The box metaphor holds true in many scenarios, but easily falls short in others, leading to situations that can be quite perplexing.

Before implementing functors for our IO, Undetemined and Maybe cases, let's see with some code those 2 different perspectives on Functors.

## Functor as a mapper
Take a simple function `string -> int`:

```csharp
Func<string, int> length = s => s.Length;
```


![length function from string to int](static/img/monads-for-the-rest-of-us/functors-length.png){: height="300px" }


We want to map it work with `IEnumerables`, and make its signature

```haskell
IEnumerable<string> -> IEnumerable<int>
```

That is, awe want to elevate it to the world inhabitated by types modeled by the `IEnumerable` monad:

![length function from string to int](static/img/monads-for-the-rest-of-us/functors-functorial-length.png){: height="300px" }

Do you see how this type-signature transformation perfectly match the goal of `Map`?

Let's implement it:

```csharp
internal static class FunctorExtensions
{
    internal static Func<IEnumerable<A>, IEnumerable<B>> Map<A, B>(this Func<A, B> f)
    {
        IEnumerable<B> ApplyFunctionToAllItems(IEnumerable<A> aa)
        {
            foreach (var a in aa)
            {
                yield return f(a);
            }
        }

        return values => ApplyFunctionToAllItems(values);
    }        
}

string s = "foo";

Func<IEnumerable<string>, IEnumerable<int>> functorialLength = length.Map();


IEnumerable<string> values = new[] { "foo", "barbaz", "" };
IEnumerable<int> lengths = functorialLength(values);

Assert.Equal(new[] { 3, 6, 0 }, lengths);
```

# Functor as a box
Bear with me: we are going to perform a series of refactoring moves. At the end we will get to an implementation of a Functor seen as a box. The reason why I want to get there via refactoring is to stress that the 2 points of view are alternative, 2 faces of the same coin.

With Rider / ReSharper, move your cursor on `yield` and apply  "*To collection return*":

```csharp
Func<IEnumerable<A>, IEnumerable<B>> Map<A, B>(this Func<A, B> f)
{
    IEnumerable<B> ApplyFunctionToAllItems(IEnumerable<A> aa)
    {
        var items = new List<B>();
        foreach (var a in aa)
        {
            items.Add(f(a));
        }

        return items;
    }

    return ApplyFunctionToAllItems;
}
```

On `foreach`, "*Convert into LINQ-expression*":

```csharp
Func<IEnumerable<A>, IEnumerable<B>> Map<A, B>(Func<A, B> f)
{
    IEnumerable<B> ApplyFunctionToAllItems(IEnumerable<A> aa)
    {
        return aa.Select(a => f(a)).ToList();
    }

    return ApplyFunctionToAllItems;
}
```

Feel free to remove the `ToList()` call:


```csharp
Func<IEnumerable<A>, IEnumerable<B>> Map<A, B>(Func<A, B> f)
{
    IEnumerable<B> ApplyFunctionToAllItems(IEnumerable<A> aa)
    {
        return aa.Select(a => f(a));
    }

    return ApplyFunctionToAllItems;
}
```


On `ApplyFunctionToAllItems` apply "*To Expression Body*":

```csharp
Func<IEnumerable<A>, IEnumerable<B>> Map<A, B>(Func<A, B> f)
{
    IEnumerable<B> ApplyFunctionToAllItems(IEnumerable<A> aa) => aa.Select(a => f(a));

    return ApplyFunctionToAllItems;
}
```

Again on `ApplyFunctionToAllItems`, apply "*Inline method*":

```csharp
Func<IEnumerable<A>, IEnumerable<B>> Map<A, B>(Func<A, B> f)
{
    return aa => aa.Select(a => f(a));
}
```

On `f` "*Replace with method group":

```csharp
Func<IEnumerable<A>, IEnumerable<B>> Map<A, B>(Func<A, B> f)
{
    return aa => aa.Select(f);
}
```

On `return`, apply "*To Expression Body*":

```csharp
Func<IEnumerable<A>, IEnumerable<B>> Map<A, B>(Func<A, B> f) => aa => 
    aa.Select(f);
```

That's interesting! We just discovered that `Map` is natively implemented by Linq's `Select`.  
To see this clearly, in the calling site:

```csharp
var lengths = length.Map()(values);
```

inline `Map()`:

```csharp
var lengths = ((Func<IEnumerable<string>, IEnumerable<int>>)(aa => aa.Select(length)))(values);
```

Go on the `=>` and apply `To Local Function`:

```csharp
IEnumerable<int> Func(IEnumerable<string> aa) => aa.Select(length);

var lengths = ((Func<IEnumerable<string>, IEnumerable<int>>)(Func))(values);
```

You can simplify the last line removing a bit of extra type annotations:

```csharp
var lengths = Func(values);
```

Inline `Func` and, voilà:

```csharp
var lengths = values.Select(length);
```

You can interpret `Select` as the function that maps `length` to the content of `values`, where values, an `IEnumerable<string>` is seen as a container of `string` values.



# Resources
* [Bartosz Milewski - Category Theory for Programmers][bartosz]
  * [Bartosz Milewski - Functors][bartosz-functors]
* [Michael Barr, Charles Wells - Category Theory for Computing Science][barr-wells]
* [Functor as defined in language-ext][language-ext]
* [Functor as defined in Scala Cats][scala-cats-functor]
* [Functors, Applicatives, And Monads In Pictures][functors-as-boxes]

[bartosz-functors]: https://bartoszmilewski.com/2015/01/20/functors/
[bartosz]: https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/
[barr-wells]: https://www.math.mcgill.ca/barr/papers/ctcs.pdf
[language-ext]: https://github.com/louthy/language-ext/blob/main/LanguageExt.Core/Type%20Classes/Functor/Functor.cs
[scala-cats-functor]: https://github.com/typelevel/cats/blob/main/core/src/main/scala/cats/Functor.scala#L31-L228
[functors-as-boxes]: https://www.adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html

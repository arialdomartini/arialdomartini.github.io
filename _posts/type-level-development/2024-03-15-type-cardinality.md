---
layout: post
title: "Type Cardinality"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- Type Theory
- Functional Programming
- Haskell
- C#
---
I refuse to see the tension between theory and practice.  
I love the sentence:

> In theory there is no difference between theory and practice.  
> In practice there is.

It is very, very funny, I love to cite it when I can. But I cannot
help but think it is false &mdash; I will write about this, some time.

Let's talk about mathematical notions. I hear way too many times


> Yes, hey are universal and profound. But *practically* useless.

I hate this.

With this post I challenge myself to debunk this notion. I chose a
very abstract notion &mdash; the concept of *cardinality
of types* &mdash; and challanged myself to find pragmatic uses of it
in the everyday programmer's life.

Can there be a more useless and theoretical notion that *cardinality*?

<!--more--> 
# Types as Set
Types can be modelled as sets. This enables us to use to our advantage
many of the results of Set Theory.

In Set Theory, sets are associated to a Cardinality, a measure of
set's size. While the cardinality of a finite set is just the number
of its elements, it is also possible to extend this notion to infinite
sets and types by relying on the notion of comparison of sets.

Equally, we can talk about types cardinality. Here are the simplest
examples:

## Empty types
Any type not defining a data constructor cannot be build, so it is
inhabitated. Consequently, its cardinality is `0`. The canonical
example is:

```haskell
data Void
```

It is not possible to build an instance of an empty set. Curiously, it
is perfectly legit to define functions from Void, but it is not
possible to invoke them.

```haskell
data Void
f :: Void -> Int
f _ = 42
```

The only way to invoke `f` would be to provide it with an instance of
`Void`: but there is no way to build it.  
Being pedantic, one could observe that every type in Haskell contains
a shared inhabitant, `Bottom`, which represents the non-terminating
and the undefined values. Therefore, it *is* possible to invoke a
`Void -> a` function, as demostrated below:

```haskell
a = undefined -- an undefined value
f a
> 42

id x = id x -- a never ending function
f (id "Hey")
> 42
```

It is also possible to define function to `Void`, but evaluating them
causes an error. So, strictly speaking, we should say that `Void` does
not contain any value representing a *terminating computation*.

### In C#
If you wonder how to define such a type in C#, and you follow the idea
that it must be impossible to create an instance, it makes sense to
use a record with a private constructor:

```csharp
record Void
{
    private Void()
    {
    }
}
```

or an abstract record:

```csharp
abstract record Void { }
```

C# does not define the bottom value, but it is still possible to use
`null` as the ubiquitous value inhabitating most of the types:

```csharp
[Fact]
void cannot_invoke()
{
    Func<Void, int> f = _ => 42;
    
    var result = f(null);
    
    Assert.Equal(42, result);
}
```

In order to remove `null` from the game, one could think to define
`Void` using a `record struct`:

```csharp
record struct Void
{
    private Void()
    {
    }
}
```

but this fails to compile: the compiler would complain that the
constructor must be public (yes: internal would not be enough). We are
no more lucky trying with an abstract struct:

```csharp
abstract record struct Void { }
```

This won't compile, because record structs cannot be abstract.

That said, you may wonder what an inhabitated type can be useful for.
The fact that `Void` represents something that can never
occur can be used as a type-safe way for exhaustively handling "can't
happen" cases. The Curry-Howard isomorphism gives us an interpretation
of `Void` as the false proposition. 

Void types can also be used as [phantom types][phantom-type] to
implement some interesting type-level tricks &mdash; such as defining
[Fixed-length vectors][fixed-length vectors], vectors whole length is
encoded it their type, so that it is possible to define functions that
can be applied only to non-empty vectors and whose correctness is
defined at compile time.  
Or for checking, at compile time, that [pattern matching is exhaustive][exhaustive-pattern-matching].


## Singletons
Types having `1` single inhabitant have cardinality `1`. They can be
built using a data constructor taking no parameters:

```haskell
data Singleton = Singleton
```

The canonical example is Unit:

```haskell
data () = ()
```

Types with a single inhabitant are referred to as Singleton, a word
that differs significantly from its usage in Object-Oriented
Programming's Singleton Pattern.

In Category Theory singletons as the terminal objects: a terminal
object is the object that every other object has a unique morphism to.

### In C#
In C# a singleton type can be *approximated* with a fieldless record:

```csharp
record struct Unit;
```

I wrote "approximated" because if you instance `Unit` multiple times,
the values you will obtain are equal &mdash; so for all intent and
purposes they can be consider the same value; yet, according to the
reference equality, they are still different intances:


```csharp
[Fact]
void not_a_real_singleton_type()
{
    var unit = new Unit();
    var notSameUnit = new Unit();
    
    Assert.Equal(unit, notSameUnit);
	
    Assert.NotSame(unit, notSameUnit);
}
```

## Types with exactly `n`  inhabitants
Types such as `Bool` have cardinality `2`:

```haskell
data Bool = True | False
```

Defining a type with cardinality `n` is a matter of providing exactly
`n` data constructors. For example, for `4`:

```haskell
data CardinalPoint = North | East | South | East
```
### In C#
C#'s `bool` also has cardinality `2`. Playing dirty on could thing of
using `bool?` to get cardinality `3`. Is it possible to define types
with cardinality `n`?  
Besides few cases (like `bool`), in languages like C# we are used to
types that have a very large amount of inhabitants (such as `int`) or
even an infinite number (such as `string`). Haskell obtains the
desired result through Discriminated Union Types. C# does not support
them directly. But it is not hard to convince ourselves that
inheritance is the key to implement them, although not as concisely as
other languages like F# do:

```haskell
record abstract CardinalPoint;

record North : CardinalPoint;
record East : CardinalPoint;
record South : CardinalPoint;
record West : CardinalPoint;
```

<!-- You can read in [Discriminated Union Types in C#][union-types-csharp] -->
<!-- how to use and pattern match them. -->


# Isomorphisms
Here's a very interesting fact: all types having the same
cardinality are equivalent: it is perfectly safe to replace a type of
a certain cardinality with an arbitrary other one, having the same
cardinality, without any risk of loosing information.

Stricly speaking, they are *isomorphic*. This means that there exist
pairs of functions mapping one to another and back, such that their
composition is the identity function. In other words: it is always
possible to move back and forth values from one type to the other
without loosing information. For example given:

```haskell
data Bool = True | False
data Actor = Laurel | Hardy
```

it is possible to define:

```haskell
toBool :: Actor -> Bool
toBool True = Laurel
toBool False = Hardy

toActor :: Bool -> Actors
toActor Laurel = True
toActor Hardy = False
```

such that:

```haskell
(toBool . toActor) :: Actor -> Actor
toBool . toActor = id

(toActor . toBool) :: Bool -> Bool
toActor . toBool = id
```

Notice that I mentioned *pairs* of functions. In fact, for types of
cardinality `n`, there exist `n!` isomorphisms.

When this happens, we write: 

$$Bool \cong Actor$$

How can this possibly be useful? Let me use a specific example, which
gives me the chance to introduce an often negletted code smell and a
resolution that goes in direction of type-modelling: Boolean Blindness.

This deserves a post on its on. I promise I'm publishing it soon.

(If you are even lightly curious to read the next post, I reached my
goal: theory is a way to improve practice).

# References
* [Sandy Maguire - Thinking With Types][thinking-with-types]
* [Phantom Type - Haskell Wiki][phantom-type]
* [Fixed-length vectors in Haskell, Part 1: Using GADTs][fixed-length-vectors]
* [How do I check that a switch block is exhaustive in
  TypeScript?][exhaustive-pattern-matching]
  
[thinking-with-types]: https://leanpub.com/thinking-with-types
[phantom-type]: https://wiki.haskell.org/Phantom_type
[fixed-length-vectors]: https://mail.haskell.org/pipermail/haskell/2005-May/015815.html
[exhaustive-pattern-matching]: https://stackoverflow.com/questions/39419170/how-do-i-check-that-a-switch-block-is-exhaustive-in-typescript

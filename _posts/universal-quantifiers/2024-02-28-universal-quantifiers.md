---
layout: post
title: "Universal Quantifiers and Existential Types For The Rest Of Us"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- Haskell
- Type Theory
- Functional Programming
---
## Intro - delete me
My personal goal: to be able to answer the following questions

* What is an `Existential Type` and what is it useful for?

* Why does this produce a compilation error?

```haskell

```

* Why are the following completely different?

```haskell
foo :: (forall a. a -> a) -> (Char, Bool)
bar :: forall a. ((a -> a) -> (Char, Bool))
```

* Why are all those declaration the same?

```haskell
zip :: forall a. ( forall b. ( [a] -> [b] -> [(a, b)] ))
zip :: forall a. forall b. [a] -> [b] -> [(a, b)]
zip :: forall a b. [a] -> [b] -> [(a, b)]
zip :: [a] -> [b] -> [(a, b)]
```

* How to interpret this:

```haskell
runST :: forall a. (forall s. ST s a) -> a
```

This post series is the collection of my study notes. I hope that by
the end of it you will be also able to wrap your head around these
topics.

<!--more-->

## Disclaimer: Haskell as a reference for C#
Although this series touches on some (for me) advanced Haskell topics, these are not articles on Haskell. I'm not an expert neither in Haskell nor in Type Theory, and I would not dare to pass myself as one. Indeed, I've got nothing to teach about them  
Instead, I study Haskell as a state-of-the-art language that can teach
me a lot on how to write better code in other languages.

The goal of this study, besides satisfying my natural curiosity on
Haskell and Type Theory, is to develop a better intuition on
interfaces, generics and functions in C#, to learn how to apply
functional programming whenever it makes sense, and to explore the
possibilities of Type Level Development in C#, TypeScript and F#.

It's for me the beginning of a long journey rather than a destination.

# Generic types are nothing but level-type functions
Let's begin with some basic definitions to establish a shared vocabulary.  

We will see how ad-hoc polymorphism in Haskell is implemented with
type classes and type families, and how parametric polymorphism is
implemented through type variables.  

We will witness a lot of applications of the `forall` keyword: it is
used for expressing Universal Quantification and Existential
Quantification, to implement polymorphism.

If this is already confusing, don't despair. It was for me too.  
Let's start from the beginning, with the question: "What are ad-hoc
and parametric polymorphism"?

## Polymorphism
Polymorphism is the use of a single symbol to represent multiple
different types. Think of this as an extension of data structure and
functions that can only operate on a single, concrete type &mdash; and
which, by contrast, are called *monomorphic*  
In the conventional (and depressing) example:

```csharp
interface Shape
{
    double Area();
}

class Circle : Shape { ... }
class Rectangle : Shape { ... }

Shape circle = new Circle();
```

the `circle` instance's declared type `Shape` is no longer identical
to its run-time type `Circle`.

A function such as:

```csharp
Func<Shape, double> calculateArea = shape => shape.Area();
```

is said to be polymorphic, because it can operate on objects of
different types, through their common interface `Shape`.

### Ad-hoc polymorphism
The cases above are examples of the so called `ad-hoc polymorphism`. 

"ad hoc" is a Latin expression, meaning "for a specific purpose",
"customized".

The naming makes sense: indeed, each concrete type (`Circle`,
`Rectangle`) has the chance to offer an *ad-hoc*, specialized
implementation of the virtual method (`Area()`).

### Parametric polymorphism
There is another type of polymorphism, whose purpose is to offer a
*generic*, single implementation of some functionality, which is
capable of operating uniformally on different types.

Think to:

```csharp
int calculateLength<T>(IEnumerable<T> list) => ...
```


It provides an implementation that uniformally works for multiple
types (`IEnumerable<string>`, `IEnumerable<double>`) and even for
types that don't exist yet.

The conventional, trivial example, is identity:

```csharp
T identity<T>(T t) => t;
```

which operates on all possible types, providing the very same
implementation.


It is called `parametric polymorphism` because parametric polymorphic
expressions involve the use of *type parameters*: instead of
mentioning specific concrete types, *type parameters* are placeholders
for types that will be specified when the function or the data
structure is used.


Parametric polymorphism allows a function or a data type to be written
*generically*, so that it can handle values uniformly without
depending on their type.

I'm insisting with the term *generic* because classically, in OOP
languages, parametric polymorphism is implemented through generics.

Haskell offers a different means, which we will see in a minute. Allow
me to comment on the consequence of parametric polymorphism first.

#### Generic signatures, specific implementations
Parametric polymorphism is interesting because it helps driving a
correct code modeling and implementation. In fact, the more generic a
signature is, the more specific the implementation must be.

Think again to the identity function. It is so polymorphic that given
the signature:

```haskell
f :: a -> a 
```

besides the less-than useful cases of an infinite loop or raising an
exception, there cannot be any other implementation than:

```csharp
T identity<T>(T t) => t;
```

As puzzling as it may seem, it's not hard to be convinced.

We use parametric polymorphism because we want to restrict the number
of the possible implementations. And because we want the type system
to be able to check its correctness, at compile time.


### Type parameters
How does this have to do with the bold claim "Generic types are
nothing but level-type functions"?

Let's quit C# for Haskell and let's see how Haskell represents &mdash; and
how it implements &mdash; parametric polymorphism.

## Polymorphis For The Rest of Us
In these pages, all our reasonings will be around type and function
signatures, to the point that we can safely ignore the actual
implementation code.  
So, talking about polymorphism, it makes sense to choose the simplest
polymorphic function ever, the identity function:

```haskell
id :: a -> a
id x = x
```

`a` here stands for any type. This is the equivalent of C#'s:

```csharp
A Id<A>(A x) = x
```

Unfortunately, C# is not powerful enough to let us express the same with:

```csharp
Func<T, T> id = (T x) => x;
```

In both C# and Haskell the names `A` and `a` are absolutely
arbitrary. They might be interpreted as a placeholder for an
unidentified type.

But there is a better interpretation. Under the hood, `id` takes 2,
not 1, parameters. Indeed, it is like `id` was defined, again using a
fictional syntax, as:

```haskell
id @a (x :: @a) = x
```

Notice the `@` symbol: I am using here to signify that `@a` is a type
parameter. Notice also how the original parameter `x` is of type `@a`.

Similarly, in a fictional C#, it is like `Id` was defined as:

```csharp
A Id(Type A, A x) = x
```

In both the cases, the first parameter is a type; the second parameter
is a value, whose type matches the first parameter.


### Term level and type level parameters
The last fictional snippets assume a different notion of functions. We
are used to functions taking value parameters of some type. The
functions we used above also take parameters which are not values of
some type, but types themselves.

From now on, let's call the ordinary parameters we are familiar with
as *term level parameters*, and the new notion of `@` parameters as
*type level parameters*.

I claimed that the following was a fictional syntax: 

```haskell
id :: a -> a
id @a (x :: @a) = x
```

In reality, Haskell does really see polymorphic functions in a similar
way.  
Here's an experiment you can try, which follows the brilliant
Gabriella Gonzales' article [Polymorphism For
Dummies][polymorphism-for-dummies].

Define `id` (hiding the standard Prelude implementation):

```haskell
-- file: id.hs

module Id where

import Prelude hiding (id)

id :: a -> a
id x = x
```

Then ask `ghc` to show the low-level core implmentation it
generates, with

```bash
ghc -ddump-simpl id.hs
```

You will get the output:

```bash
GHCi, version 9.8.1: https://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling Main             ( id.hs, interpreted )

==================== Tidy Core ====================
Result size of Tidy Core
  = {terms: 18, types: 10, coercions: 0, joins: 0/0}

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
id :: forall a. a -> a
[GblId, Arity=1, Unf=OtherCon []]
id = \ (@a_aBS) (x_aBB :: a_aBS) -> break<0>(x_aBB) x_aBB
```

Focus on the last line:

```haskell
id = \ (@a_aBS) (x_aBB :: a_aBS) -> break<0>(x_aBB) x_aBB
```

If you squeeze your eyes you should be able to reason about it,
already. It might help renaming the type and the term parameters, so
that turns into:

```haskell
id = \ (@a) (x :: a) -> break<0>(x) x
```

You have surely noticed how similar it is to our fictional:

```haskell
id @a (x :: @a) = x
```

So, it seems that those type parameters, although not apparent and
explicit in the source code, are actually a fact of Haskell.

Where does this come from?

### System F
Is you played with Functional Programming you have surely stumbled
upon the notion of Lambda Calculus and you have probably learnt that
Haskell is based on it.

You have also surely heard of the Hindley-Millner type system, on which
Haskell is also based on.

They both have to do with System F.  
System F formalizes parametric polymorphism in programming
languages. It introduces parts of lambda calculus at the type level.

Since it can be demonstrated that type inference for System F is
impossible, many languages use a restriction of System F known,
surprise surprise, "Hindley–Milner". Over time, the restrictions of
Hindley-Millner have become apparent, so many language extensions have
been introduced in Haskell to allow more expressive constructs.

System F is a mathematical discipline, so it is not necessarily
approachable for us developer. Never the less, it might be interesting
to see how parametric polymorphism is formalized in System F, because
it makes apparent what we just speculated about type parameters.

#### A taste of System F
The identity function is formalized in [System F][system-f] as the
*judgement*:

$$\vdash \Lambda \alpha.\lambda x^\alpha.x : \forall \alpha.\alpha \rightarrow \alpha$$

The first time I saw this, I felt discouraged.  
I promise that you already know what it means, only you possibly don't
know this specific syntax.  Read it as follows:

| Symbol                        | Meaning                                                                            |
|-------------------------------|------------------------------------------------------------------------------------|
| $$\vdash$$                    | In System F, the following is provable                                             |
| $$\Lambda \alpha.$$           | The function takes a type parameter $$\alpha$$                                     |
| $$ \lambda x^\alpha. $$       | and a term-level parameter $$x$$ of type $$\alpha$$                                |
| $$x$$                         | in the expression $$x$$                                                            |
| $$:$$                         | is such a way that                                                                 |
| $$\forall \alpha .$$          | for any value of type parameter $$\alpha$$                                         |
| $$\alpha \rightarrow \alpha$$ | the function takes a value of type $$\alpha$$ and returns a value of the same type |

As you see, in System F, a lower-case $$\lambda$$ is used to introduce
a term-level parameter, while an upper-case $$\Lambda$$ denotes a type
variable. The superscripted $$\alpha$$ in  $$x^\alpha$$ means that the
bound of $$x$$ is of type $$\alpha$$.

### A first intuition
We can build a first intuition on parametric polymorphism: a
parametric polymorphic function can be thought as an ordinary
function, taking extra type-level parameters. Those extra parameters
indicate the types the function is cabaple of operating on, providing
an uniform behavior.

I like to see the `<` and `>` symbols in C# and Java generic
expressions such as `Id<T>` and `IEnumerable<T>` as a special form of
the `()` used for applying the ordinary function to term-level
parameters.

### Some examples

```haskell
```

fmapz
  = \ (@(f_aCE :: * -> *))
      (@a_aCF)
      (@b_aCG)
      ($dFunctor_aCH :: Functor f_aCE)
      (f1_aBP :: a_aCF -> b_aCG)
      (x_aBQ :: f_aCE a_aCF) ->
      break<0>(x_aBQ,f1_aBP)
      fmap @f_aCE $dFunctor_aCH @a_aCF @b_aCG f1_aBP x_aBQ
z


### Are those real function parameters?
If it is true that functions can take type parameters, 2 questions
arise:

* Can we actually provide type-level parameters, when invoking
  functions?<br/>(Spoiler: yes!)
* If so, can we partially apply a function providing only some of the
  type-level parameters?<br/>(Spoiler: amazingly, yes! yes! yes!)

Before discovering how, let's take a little detour. Let's build a bit
more solid foundations to out intuition, by reviewing (some of) the
content of the seminal paper [On understanding types, data
abstraction, and polymorphism][on-understanding-types] by [Luca
Cardelli][luca-cardelli].

# References

* [Gabriella Gonzales][gabriella-gonzales]  - [Polymorphism For Dummies][polymorphism-for-dummies]

* [Relationship between Haskell's 'forall' and '=>'](https://stackoverflow.com/questions/33199180/relationship-between-haskells-forall-and)
* https://downloads.haskell.org/~ghc/6.12.2/docs/html/users_guide/other-type-extensions.html#explicit-foralls

* [Luca Cardelli][luca-cardelli], Peter Wegner -  [On understanding types, data abstraction, and polymorphism][on-understanding-types]

* https://wasp-lang.dev/blog/2021/09/01/haskell-forall-tutorial

* [What does the `forall` keyword in Haskell/GHC do?][what-does-forall-do]
  
* [System F][system-f] 

[what-does-forall-do]: https://stackoverflow.com/questions/3071136/what-does-the-forall-keyword-in-haskell-ghc-do

[gabriella-gonzales]: https://www.blogger.com/profile/01917800488530923694

[polymorphism-for-dummies]: https://www.haskellforall.com/2015/10/polymorphism-for-dummies.html

[luca-cardelli]: http://lucacardelli.name/
[on-understanding-types]: http://lucacardelli.name/Papers/OnUnderstanding.A4.pdf


[system-f]: https://en.wikipedia.org/wiki/System_F

---
layout: post
title: "Seeking Genericity Brings Specificity"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- Type Theory
- Functional Programming
- Haskell
- C#
---
<!--more--> 
There is an interesting tension between the desires we harbor as
programmers and the techniques we adopt to fullfill them. On the one
hand, we wish that our programs run in any possible circumstance, so
we try to be as permissive as possible; on the other hand, on the
quest to ensure the correctness of our code, we apply a series of
techniques whose ultimate goal is the opposite contrary: we use type
checks &mdash; which have the effect that large classes of expressions
legal in the untyped world become illegal, we kill without mercy
implementations if a unit test says so, we value rigor and strictness
over laxity and indulgence.

We seek both genericity and specificity at the same time.

Is this contrast a contraddition? (A clue: no)
<!--more-->
This post aims to motivate investment in direction of type-level
development by providing some arguments to embrace the type system and
to adopt a more generic programming style, for the benefit of
correctness.

I will use a bunch of Haskell examples, but I am addressing C#/Java/OOP readers.

# The more generic you are, the more specific you end up being
The tension I mentioned is intrinsic in many costructs we use while
building software. I've always found very captivating the counter
intuitive fact that the more generic a function is &mdash; that is,
the larger the set of values it accepts &mdash; the less the function
can do with the received argument. The canonical example is the
identity function:

```haskell
id :: forall a. a -> a
```

Identity lives a dilemma: despite being so permissive accepting any
value of any type, it actually has no possibility at all to operate on
them. It just cannot infer which operations can be applied with those
values. So, it cannot do anything.  
As surprising as it may seem, it's not hard to convince yourself that
the only 3 possible implementations a (pure) function with such
signature can have are:

* returning the value, unmodified: `id x = x`
* crashing the whole universe: `id _ = undefined`
* refusing to even return, entering a never ending loop: `id x = id x`

On the other side of the spectrum, when we operate on numbers, often
we impulsively and instinctively use the `Int` type. I dare you to
answer: how many times you use `Int` while in reality you would admit
"Well, I just needed *positive* ints..."  
We generally tend to be permissive because we want our program to be
generic.

But this is an illusion.

# Succ
Think of a function for calculating the successor of a number:

```haskell
succ :: Int -> Int
succ x = x + 1
```

Haskell's type inference leans toward genericity, so if you omit the
signature and you just write:

```haskell
succ x = x + 1
```

and you ask the compiler to fill the gap re-inserting the omitted
signature, it will diligently infer:

```haskell
succ :: Num a => a -> a
```

That's interesting! It made the signature generic, adding a type
parameter `a`. According to the type system, your implementation also
works with `Float`, `Double`, `Complex` and `Rational` and with all
the future types implementing `Num`. That's genericity for free!

This is good, of course! What you might have overlooked, because is
less obvious, is that doing so the type system also narrowed down the possible implementations function can have.  
While the (wrong) implementation:

```haskell
succ :: Int -> Int
succ x = x `div` 2
```

was happily accepted before, trying to use it with the new signature
miserably fails:

```haskell
succ :: Num a => a -> a
succ x = x `div` 2

error:
    * Could not deduce (Integral a) arising from a use of `div'
      from the context: Num a
        bound by the type signature for:
                   succ :: forall a. Num a => a -> a
        at <interactive>:18:1-24
      Possible fix:
        add (Integral a) to the context of
          the type signature for:
            succ :: forall a. Num a => a -> a
    * In the expression: x `div` 2
      In an equation for `succ': succ x = x `div` 2
```

And this is even more interesting! The more generic signature forced
the function implementation to be more specific. In a sense,
genericity helped the `succ` function being its true self, removing
(some of) the possible wrong implementations from the game.

Streching a bit its original meaning, this somehow embodies the
[Postel's Law][robustness-principle], also known as Robustness
Principle:

> Be conservative in what you do, be liberal in what you accept from others

If you pursue the correctness of your code, aim to be as much generic
as you can.

Type-Level Development extends this idea taking it to the limit. As
Sandy Maguire observes in [Thinking With Types][thinking-with-types]:

> Type-level programming is an uncommon calling. While most
> programmers are concerned with getting more of their code to
> compile, we type-level programmers are trying our best to prevent
> code from compiling

# To put the principle into practice
Is there anything simple one can do, without applying fancy type-level
techniques?  
Yes. The trick is: even before writing the implementation of a
function, stop and ask yourself *what* you are trying to get, not
*how* to implement it. Then design your function signature around the
types allowing that *what*. Stay generic, capture the general intent
rather than the specific case.

You surely noticed this is exactly the same mental approach applied in
TDD: even before writing the code implementation, you first put down
into code a test &mdash; the *what* part of the requirement; only then
you express the same requirement in terms of the *how*, with the
actual implementation.  
I like to see TDD as is an extension of static typing: it amplifies
the compilation phase filtering out some of the wrong implementation
that would be otherwise accepted by the compiler, just like your type
system does. As you can read in [Property Testing For The Rest of
Us](/property-testing), also being generic in tests, instead of
relying on specific examples, helps implementing correct code with
great specificity.

Tests aside, seeking genericity is often a matter of designing the
proper types and interfaces.

## It's all about using (small, smaller) interfaces
The OOP literature offers very compelling reasons to program to
interfaces, not implementations: low coupling / high cohesion,
testability, Inversion of Control, the [Law of Demeter][demeter], and
many other old friends.

I claim that using interfaces is about the very same topic of this
post: seeking genericity to obtain specificity. In fact, interfaces
are contracts that don't know anything about the implementation. As a
consequnce, they restrict the possible operations their clients can
invoke.

Here's a trivial C# case. You have a concrete class with many methods
and also implementing an interface:

```csharp
interface ISpecificUse
{
    string OnlyThis(int i);
}

sealed class CanDoALot : ISpecificUse
{
    ISpecificUse.OnlyThis(int i) => ...
	
	void AnotherOperation() => ...
	void AndAnother() => ...
	void YetAnother() => ...
}
```

A client receiving the concrete class is *very specific* in what it
gets, and *very relaxed* in what it can do:

```csharp
class PermissiveClient(CanDoALot service)
{
    void Run()
	{
        // can do a lot with this.service
        service.AnotherOperation();t
        service.AndAnother();
        service.YetAnother();
	}
}
```

# Is this a contraddition?
I tell you a secret. As per the [Betteridge's law of headlines][betteridge] "any headline that ends in a question mark can be
answered by the word no". And this question is no exception.

You may think that by providing a *specific* concrete type you are
being more specific, and that instead providing an interface you are
more generic. But it's absolutely the opposite.

I can think of 2 ways to demostrate this: pragmatically and through
the Set Theory.

**Pragmatically**, by finding a counter example. Let me show that
trying to be *limitlessly* loose would not bring us *limitless*
genericity. Here's the most loose implementation you can have in C#:

```csharp
class WayTooPermissiveClient(object service)
{
    void Run()
	{
        // you can do very little with this.service
	}
}
```

Do you see the problem? Receiving an `object`, all you can do is to
invoke `Equals`, `GetType`, `ToString` and `GetHashCode`. Depressing,
and far from being *limitessly generic*.

**Through Set Theory**. A type is a set, a collection of elements.
When we say that the constructor of our client receives a `CanDoALot`
instance, we actually mean "our client can legitimately use any inhabitant of the set `CanDoALot`, provided with all the
methods and functions the type system allows for it".

If instead of an instance of the specific `CanDoALot` you pass an
instance of a larger type, up the inheritance graph, the set of
methods and functions the type system makes available is smaller, not
larger. This sounds counter intuitive, at first, but it's not hard to grasp.  
A good explanation is provided by Gabriel Vergnaud in [<Type>-Level
Typescript][type-level-ts]. Consider the following TypeScript example:

```typescript
type A = { a: string; c: boolean };
type KeyOfA = keyof A; // => 'a' | 'c'

type B = { b: number; c: boolean };
type KeyOfB = keyof B; // => 'b' | 'c'

type C = A | B;
type KeyOfC = keyof C; // => ('a' | 'c') & ('b' | 'c') <=> 'c'
```

Elements of set `A` have fields `a` and `c`.  
Elements of set `B` have fields `b` and `c`.  
`C` is both `A` and `B`, just like a common parent of `A` and `B`, up
the hierarchy graph. `C`, the *union* of `A` and `B`, it is the set
containing *all* the elements of `A` and *all* the elements of *B*.  
Intuitively: if you pick a random element from this union set, you
might get either an `A` or a `B`. If you expected to find a field
`a`, there is no certainty the element at hand is instead a `B`. The
type systems knows this, and would break the compilation if you try to
access `a`. So, although `C` contains elements of `A`, they have lost
their field `a`. The very same happens for `B` elements and their
field `b`.   
All you can say is that, whatever the element you pick from `A union
B`, it surely contain the common field `c`.

The general rule is:

> the union of two [types] contains the intersection of their keys.

Once again: the more general the type you choose, the more specific
the usage must be.

# Be specific by using interfaces
Let's get back to our C# example. Let's pass `CanDoALot` in terms of
its interface:

```csharp
interface ISpecificUse
{
    string OnlyThis(int i);
}

sealed class CanDoALot : ISpecificUse
{
    ISpecificUse.OnlyThis(int i) => ...
	
	void AnotherOperation() => ...
	void AndAnother() => ...
	void YetAnother() => ...
}

class MoreGenericClient(ISpecificUse service)
{
    void Run()
	{
        var s = this.service.OnlyThis(42);
		...
	}
}
```

By programming against an interface we *increased* the number of
instances the client can work with. In the meanwhile, we dramatically
restricted what the client can do to the few operations defined in the
`ISpecificUse` contract.

This is such A Good Thing&trade; that a functional programmer would
probably go even beyond, being more generic just injecting the single
operation instead of the whole istance:


```csharp
class MoreGenericClient
{
    void Run(Func<int, string> operation)
	{
        var s = operation(42);
		...
	}
}
```

(Most likely a functional programmer fond of strong typing would
define a delegate to give that operation a name).

If this reminds you of the [Interface Segregation
Principle][interface-segregation-principle] you stand correct: it
turns out that all the good principles are tightly interconnected and
one good principle tends to manifest when talking about another good one.

# I'm already using interfaces. What can I do more?
Let's take this idea to the limit, playing again with the successor function.

While designing the signature of `succ` one could endeavor to question
whether `succ` should only work with numbers. Afterall, isn't `d` the
successor of `e`? Does not `Tuesday` come after `Monday`, and `14PM`
after `15PM`? Why should we want to limit `succ` to numbers?

An attempt to be generic with C# could bring us to using
`IOrderedEnumerable<TElement>`, which inherits from
`IEnumerable<TElement>`:

```csharp
T Succ<T>(IOrderedEnumerable<T> elements)
{
    var enumerator = elements.GetEnumerator();
    enumerator.MoveNext();
    return enumerator.Current;
}
```

This is honestly a terrible implementation. Its ugliness is partly due
to the fact `IEnumerator` exposes `MoveNext()`, which returns `bool`
instead of the next element.

Implementation aside, I just wanted an excuse to ask you: is it
really necessary that a type can be ordered for it to have a
successor? Can `Succ` have an even more generic signature &mdash; and
therefore a possibly more correct implementation?

One can think that given a type that defines an order it is always
possible to find the successor of an element (the smallest of the
larger elements).  
If this is the case, in C# the `IComparable<T>` interface would be enough.

But this is unfortunately wrong. The fact you can order things does
not imply that given an element there is a successor.  
Think to the set of cities ordered by their population, or a queue of
tasks each having a priority. Both sets have possible duplicates. 

More generally, if `-->` means *being smaller than`, an ordered set
can be something like:

```
                    .-> el4
                   /
 el1 --> el2 -->el3 --> el5
```


What's the successor of `el3`?

This is an example of what mathematicians call [Strict Total
Order][strict-total-order]: it does not ensure that an element has one
and only one successor. A set guaranteeing this trait would be
called [Well-order][well-order].  
It turns out that having a successor implies being ordered, while the
opposite is not true. So, if C# designers really wanted to promote
*programming against interfaces not implementations*, they could have
offered us an interface for types that can be ordered, and a different
interface &mdash; inheriting from the former &mdash; for types having
a successor:

```csharp
interface ISortable<in T>
{
    bool IsSmallerThan(T t);
}

interface IHasSuccessor<out T> : ISortable<T>
{
    T Successor { get; }
}
```

That would be very generic. And this is how I would suggest designing
the code.  
Not only would this guide a correct implementation. It would also
promote code reuse, giving some very generic implementations the
change to emerge.

The experience with Haskell reveals that this approach leads to
breaking problems down into fundamental and profoundly meaningful
components, many of which possess such universal applicability that
they frequently mirror mathematical concepts, just like what happened
with our `Succ` case.  
No wonder that the most generic libraries provided by languages, such
as LINQ, make us of interfaces and notions so bound to maths.

## The surprising extremism of Haskell
Now you are surely wondering why I went off on tangents so much. It's
because I really wanted to show you how generic the signature of
`succ` can be.

Surprisingly (or maybe not anymore, at this point), the signature of
`succ` is Haskell has (almost) nothing to do with numbers:

```haskell
succ :: Enum a => a -> a
```

Read it as: "if you want to get the successor of something, I don't
care what the concrete type is as long as there is a way to define a
sequential order".

Haskell is extreme this notion of genericity. See how it defines
things that can be ordered &mdash; but not necessarily having a
successor:

```haskell
class Eq a => Ord a where
```

Notice the `Eq`. Read it as: "if you want to define how to order
things, you surely need to figure out when two instance are are equal.
Therefore, implement `Eq`".

This is a completely different use of inheritance:

* first, an attempt to distill the very basic building blocks of the
  *what* one wants to obtain from the code
* then, a mechanism for composing the building blocks, to build more
  complex behaviors without loosing in generality.
  
This is a general approach, not specific to Haskell, and perfectly
applicable to any modern language.

# Haskell teaches us a lesson
There is an easy way to be inspired and to see how far we can go with
this approach: write a function in the Haskell REPL, omitting the
signature, then ask the type system which signature it inferred. I
find it fascinating that often the result gives the impression that
the compiler magically captured the intentions.  
Here are a couple of examples.

## Reviewing Pull Requests
Imagine a stupid function returning an error message if someones
wants to review their own pull requests:

```haskell
qualityGate reviewer committer = 
    if reviewer == committer 
    then "Hey! Ask someone else to review your code" 
    else "OK"
```

It's hard to imagine how a compiler could infer what types that project
uses for modelling committers and reviewers. But the real question is:
does it need this information?  
No, it does not. After all, all that this function needs to do with
those parameters is to check if they are the same instance. The type
system is smart enough to infer:

```haskell
qualityGate :: Eq a => a -> a -> String
```
So, being equatable is enough. Makes sense.  
Your language's type inference might not be that smart. But you are.
No reasons to be less generic than this.

## Calculate the cart total
Here's a function that, given a list of products and a getter for the
product price, returns the grand total:

```haskell
calculateTotal products getPrice = sum (fmap getPrice products)
```
If you ask Haskell what the signature is, you might be surprised how
generic it is, not even mentioning lists:

```haskell
calculateTotal :: (Foldable t, Num a1, Functor t) => t a2 -> (a2 -> a1) -> a1
```

## esreveR
The fact that there is a tight link between generic signatures and
specific implementations implies that, in many cases, given a
signature it is possible to infer the implementation, or to get
close to it.  
There is a renowned search engine, [Hoogle][hoogle], which can do
this: provide it with a signature, even a generic one, and it will
find the function implementing it.  
Such a search engine exists because it covers a need: programming
through generic interfaces makes code reuse way, way more intensive. 

# Conclusion
I like the point of view of Luca Cardelli and Peter Wegner in their
seminal [On Understanding Types, Data Abstraction, and Polymorphism][on-undertanding-types]]:

> A type may be viewed as a set of clothes (or a suit of armor) that
> protects an underlying untyped representation from arbitrary or
> unintended use. It provides a protective covering that hides the
> underlying representation and constrains the way objects may
> interact with other objects. In an untyped system untyped objects
> are naked in that the underlying representation is exposed for all
> to see. Violating the type system involves removing the protective
> set of clothing and operating directly on the naked representati


My take on this is: the type system can be your best friend in your
quest to correctness. A very convenient, cheap and safe first step
is to start resisting from the temptation of relying on concrete types
and using large interfaces.  
Instead, if you want to be specific, generalize. If you believe in
Interface Segregation, segregate more. Use more, not less, generic
types and generic functions.

Have fun. Happy coding.

# References
* [Robustness Principle - Wikipedia][robustness-principle]
* [Sandy Maguire - Thinking With Types][thinking-with-types]
* [Interface Segregation Principle -
  Wikipedia][interface-segregation-principle]
* [IEnumerator - learn.microsoft.com][ienumerator]
* [IOrderedEnumerable - learn.microsoft.com][iorderedenumerable]
* [Enum - Hackage][enum]
* [Strict Total Order - Wikipedia][strict-total-order]
* [Well Order - Wikipedia][well-order]
* [On Understanding Types, Data Abstraction, and Polymorphism Luca Cardelli, Peter Wegner][on-undertanding-types]
* [Hoogle][hoogle]
* [Betteridge's law of headlines][betteridge]
* [TypeLevel Typescript][type-level-ts]
  * [Intersections of objects and unions of keys][intersection-union]
[Property Testing For The Rest of Us](/property-testing)
* [Mnemonics: Law of Demeter - Arialdo Martini][demeter]


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/28)


[robustness-principle]: https://en.wikipedia.org/wiki/Robustness_principle
[thinking-with-types]: https://leanpub.com/thinking-with-types
[interface-segregation-principle]: https://en.wikipedia.org/wiki/Interface_segregation_principle
[ienumerator]: https://learn.microsoft.com/en-us/dotnet/api/system.collections.ienumerator.movenext?view=net-8.0#system-collections-ienumerator-movenext
[iorderedenumerable]: https://learn.microsoft.com/en-us/dotnet/api/system.linq.iorderedenumerable-1?view=net-8.0
[enum]: https://hackage.haskell.org/package/base-4.19.1.0/docs/src/GHC.Enum.html#Enum
[strict-total-order]: https://en.wikipedia.org/wiki/Total_order
[well-order]: https://en.wikipedia.org/wiki/Well-order
[on-undertanding-types]: http://lucacardelli.name/Papers/OnUnderstanding.A4.pdf
[hoogle]: https://hoogle.haskell.org/
[betteridge]: https://en.wikipedia.org/wiki/Betteridge's_law_of_headlines
[type-level-ts]: https://type-level-typescript.com/objects-and-records
[intersection-union]: https://type-level-typescript.com/objects-and-records#intersections-of-objects-and-unions-of-keys
[demeter]: https://github.com/arialdomartini/mnemonics/blob/law-of-demeter/README.md

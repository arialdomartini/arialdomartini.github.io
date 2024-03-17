---
layout: post
title: "Boolean Blindness"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- Type Theory
- Functional Programming
- C#
---
Or: booleans can be very ambiguous without context, as in:

> A Prolog programmer is delivering a baby.
>
> The mother asks, "Is it a boy or a girl?"
>
> and the Prolog programmer says: "Yes". 

<!--more-->
## Booleans carry no information beyond their value
Consider this C# function:

```csharp
interface IFiltersOut
{
    T[] FilterOut<T>(Func<T, bool> predicate, T[] collection);
}
```

or, equivalently, in F#:

```fsharp
type filterOut<'a> = ('a -> bool) -> ('a array)
```

It gets the elements of an array and, depending on the result of
`predicate`, it keeps or it drops them.

Does a `true` in `predicate` mean to drop it or to keep it?  
Good question!

## Loss of information causes ambiguity
This ambiguity is the manifestation of Boolean Blindness, a term that
I think was coined by Professor [Dan Licata][dan-licata].  
Boolean Blindness is a smell occurring whenever a function operating
on a boolean value forgets &mdash; or *erases* &mdash; the information
about *what* that boolean was meant to represent.


Let's go straight to resolution. Here is a less ambiguous implementation:

```csharp
enum Keep
{
    Keep,
    Drop
}
    
interface IFiltersOut
{
    T[] FilterOut<T>(Func<T, Keep> predicate, T[] collection);
}
```

All the ambiguity is gone. F# would allow a stronger typed implementation:

```fsharp
type Keep = Keep | Drop

type filterOut<'a> = ('a -> Keep) -> ('a array)
```

It's just unfortunate that C# does not support discriminated union
types natively. We could compensate this lack with a slightly more verbose:

```csharp
abstract record KeepOrDrop;
record Keep : KeepOrDrop;
record Drop : KeepOrDrop;

interface IFiltersOut
{
    T[] FilterOut<T>(Func<T, KeepOrDrop> predicate, T[] collection);
}
```

`Keep | Drop` conveys *the same information* of a boolean, *plus* a
bit of domain context. 

## Other examples
The filter function is the canonical example when talking about
Boolean Blindless. Here are some other poorly designed signatures:

```csharp
void SellAlcohol(
    User user, 
    Func<User, bool> checkAge);

bool ActivateFeature(
    Account account, 
    Func<FileInfo, bool> readSettings);

decimal CalculateTotal(
    Order order, 
    bool discountStatus);
```

Which meaning those `bool` values have is absolutely arbitrary, at the
complete discretion of the implementor, and totally opaque to its
clients. There is no *good* or *bad* choice. It is just an ambiguity,
often clarified with comments, conventions or conventional variable
names. Or left there to our damage.

Here are the same signatures, with an attempt to clarify the ambiguity
at the type level:


```csharp
void SellAlcohol(
    User user, 
    Func<User, IsAdult> checkAge);

bool ActivateFeature(
    Account account, 
    Func<FileInfo, FeatureIsActive> readSettings);

decimal CalculateTotal(
    Order order, 
    DiscountsEnabled discountStatus);
```

## Naming variables
One could think that Boolean Blindness can be equally (and more
easily) addressed assigning meaningful names to variables. Indeed,
properly naming things helps. It's not coincidental that Boolean
Blindness belongs to the same family of [Uncommunicative
Name][uncommunicative-name] and [Magic Number][magic-number], which
are about naming things.

Yet, resolving Boolean Blindness at type level ensures an enforcement
by the compiler which in general is preferrable than relying solely on
discipline. My take on this is that with statically and strongly typed
languages, the more we leverage the compiler, the greater the benefit
we obtain in the long run.

## Isomorphisms
As we saw in [Type Cardinality](/type-cardinality), types having the
same number of inhabitants are equivalent: there always exist
isomorphisms mapping one type onto the other.  
This means that, ideally, we should be able to use `Keep | Drop`
wherever we used to have `bool`.

This is in general true, with a little caveat.  Most of the languages
treat boolean in a special way, reserving `if/then/else` keywords to
it. `bool` is priviledged, if compare with any custom type you want to
introduce. That's an unfair perk.  
But not everything is lost. Pattern matching is the the way to go. You
can always build a mapping to `bool` with:

```csharp
var keepIt = predicate(element) switch
{
    Drop => false,
    Keep => true
};
```

or use pattern matching directly in your expressions:

```csharp
class FiltersOut : IFiltersOut
{
    T[] IFiltersOut.FilterOut<T>(Func<T, KeepOrDrop> predicate, T[] collection) =>
        collection
            .Where(element => predicate(element) is Keep)
            .ToArray();
}
```

This approach is more scalable, more flexible and more expressive,
than just using booleans: it is always possible to add additional
cases and have the compiler supervising the usage and its consistency
everywhere, for free.

# Domain-Modeling
My personal take-away is: wherever I find the opportunity, I try to
give an emerging domain notion the dignity of a type, so that I can
treat it as a *thing*. I often do the same with booleans.

In my experience, the domain experts:

* speak about *adults* and *underages*, not `true`
and `false` in the context of *checking age*;
* they refer to *enabled feature* not `false` in the context of
reading a configuration file;
* they talk about active, non-active and not yet confirmed accounts,
not `bool?`.

Therefore, it's just a matter of acklowleging this and writing down:

```fsharp
type Age = Adult | Underage
type Feature = Enabled | Disabled
type AccountState = Active | NonActive | ToBeConfirmed
```

Having a type-level domain language always pays off. DDD *aficionados*
would call this *Ubiquitous Language*. I call it *Type-Driven
Domain-Modelling*. Each to their own.

Should I ever manage to design my personal language, I would surely
*not* include any `if` statement. Instead, I would make my best to
have a pattern matching machinery as convenient as `if`. But don't
hold your breath: I cannot even write a proper parser yet.

# Mathematical treatment
If you are intollerant to hair-splitting posts, you can safely stop
here. Instead, if you want to be inspired by mathematical and
phylosophical arguments, go read [Boolean Blindness][robert-harper] by
Robert Harper.  
Amongst the interesting arguments he brings there is the following: booleans are oftern confused with logical propositions, but this is an
error. Propositions express assertions, make claims; booleans do not.

An interring argument Robert Harper brings is: the innocent `equals`
function:

```
equals :: t -> t -> Bool
equals a b = a == b
```

opens a Pandora box with very profound (philosophical) consequences.  
Here's the catch: are you ready to bet that if 2 things are marked as
*not equal* by that function are in fact *not equal*?

The problem arises from the following observation: provided that 2
*functions* (propositions) are equal, in many programming languages it
is way tougher to prove that, compared to how easy it is to prove that
two *values* are equal. Take the following:

```csharp
bool Or1(bool a, bool b) => a || b;
bool Or2(bool a, bool b) => b || a;
```

`equals` fails to prove they are equal, but in fact they are. The same
would happen for logic statements such as:

```
Proposition 1: "All humans are mortal."
Proposition 2: "If something is human, then it is mortal."
```

As Rober Harper notices:

> For a proposition, p, to be true means that it has a proof;
> there is a communicable, rational argument for why p is the case.
> For a proposition, p, to be false means that it has a refutation;
> there is a counterexample that shows why p is not the case.
>
> The language here is delicate.  The judgement that a proposition,
> p, is true is not the same as saying that p is equal to true, 
> nor is the judgement that p is false the same as saying that p
>is equal to false!  In fact, it makes no sense to even ask the
> question, is p equal to true, for the former is a proposition
> (assertion), whereas the latter is a Boolean (data value); 
> they are of different type.

But this is, really, philosophy.  
If you want to keep your feet on the ground, please: next time you
happen to be in front of a yes/no, true/false, here/there, this/that
situation, try to model it with a type.

Feel free to insult me via email if this does not work: I will not
complain.


# References
* [Dan Licata][dan-licata]
* [Luzkan - Boolean Blindness][luzkan]
* [Luzkan - Uncommunicative Name][uncommunicative-name]
* [Luzkan - Magic Number][magic-number]
* [Robert Harper - Boolean Blindness][robert-harper]

[luzkan]: https://luzkan.github.io/smells/boolean-blindness
[uncommunicative-name]: https://luzkan.github.io/smells/uncommunicative-name
[magic-number]: https://luzkan.github.io/smells/magic-number
[robert-harper]: https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/
[dan-licata]: https://dlicata.wescreates.wesleyan.edu/

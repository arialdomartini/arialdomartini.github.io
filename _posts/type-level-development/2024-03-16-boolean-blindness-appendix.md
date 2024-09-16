---
layout: post
title: "Boolean Blindness - Math appendix"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
include_in_index: false
tags:
- Type Theory
- Functional Programming
- C#
---
This is the appending to the article [Boolean
Blindness](/boolean-blindness), based on [homonymous article][robert-harper] by Robert Harper.

# Mathematical treatment
Amongst the interesting arguments he brings there is the following: booleans are ofter confused with logical propositions, but this is an
error. Propositions express assertions, they make claims; booleans do
not, they just are. The former are computed, the latter are not.

An intriguing argument Robert Harper brings is: the innocent `equals`
function:

```
equals :: t -> t -> Bool
equals a b = a == b
```

opens a Pandora box with very profound (philosophical) consequences.  
Here's the catch: are you ready to bet that if 2 things are marked as
*not equal* by that function are in fact *not equal*?

The problem arises from the following observation. Compared to
equality of values, equality of functions is way more
complicated. Provided that 2 *functions* (2 propositions) are equal,
in many programming languages it is almost impossible to prove that
they are equal. Take the following:

```csharp
bool Or1(bool a, bool b) => a || b;
bool Or2(bool a, bool b) => b || a;
```

`equals` fails to prove they express the same. The same would happen
for logic statements such as:

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

But this is, in fact, (beautiful) philosophy.

If you want to keep your feet on the ground, please: next time you
happen to be in front of a yes/no, true/false, here/there, this/that
situation, try to model it with a type.

Feel free to [insult me](/about) if this does not work: I will not
complain.

Happy programming!

# References
* [Arialdo Martini - Boolean Blindness](/boolean-blindness)
* [Dan Licata][dan-licata]
* [Luzkan - Boolean Blindness][luzkan]
* [Luzkan - Uncommunicative Name][uncommunicative-name]
* [Luzkan - Magic Number][magic-number]
* [Robert Harper - Boolean Blindness][robert-harper]
* [Sydney Padua - Mr. Boole Comes To Tea][mr-boole]
* [Sydney Padua - The Thrilling Adventures of Lovelace and Babbage: The (Mostly) True Story of the First Computer][lovelace-and-babbage]

[luzkan]: https://luzkan.github.io/smells/boolean-blindness
[uncommunicative-name]: https://luzkan.github.io/smells/uncommunicative-name
[magic-number]: https://luzkan.github.io/smells/magic-number
[robert-harper]: https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/
[dan-licata]: https://dlicata.wescreates.wesleyan.edu/
[mr-boole]: https://sydneypadua.com/2dgoggles/uncategorized/happy-200th-birthday-george-boole/
[lovelace-and-babbage]: https://sydneypadua.com/2dgoggles/the-book/

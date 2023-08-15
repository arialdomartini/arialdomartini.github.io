---
layout: post
title: "Property-based Testing For The Rest Of Us - 2"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- bash
- zsh
---
# Properties

A Property is an observation on a piece of code that we expect to hold true regardless of the inputs. This generic -- but not very practicle definition -- describes well the 1st level of the domain expert requirements.

If you want to translate a property to code, instead of writing individual unit tests consisting of certain arbitrary, possibly unmotivated input-output pairs, you will be forced to describe the functionality in a more abstract and general way. You will try to capture the invariants of the program.<br/>
Then the library will use carefully forged random values, covering the most convenient subset of the input space, trying to falsify your statements and to prove you wrong.

Proponents of formal methods sometimes stress the notion of specification above that of implementation. However it is the inconsistencies between these two independent descriptions of the desired behavior that reveal the truth. We discover incomplete understanding in the specs and bugs in the implementation. Programming does not flow in a single direction from specifications to implementation but evolves by cross-checking and updating the two. Property-based testing quickens this evolution.
(from [Design and Use of QuickCheck][design-and-use-of-quickcheck])

Makes sense? I'm sure, though, you are hungry of code, now. All right, let's go.



## Observing Test Case Distribution
It is important to be aware of the distribution of test cases: if test data is not well distributed then conclusions drawn from the test results may be invalid.
Test Cases can be counted and classified.



## The Size of Test Data
Test data generators have an implicit size parameter; the library begins by generating small test cases, and gradually increases the size as testing progresses

## Arbitrary
The library defines instances of Generators for the most common types. They are called Arbitrary: see them as the defaul generators of a give type `a`.


# Notes


I personally never though this approach could provide a false sense of security. 

Properties are universally quantified over their parameters, via the use of Test Data Generators.
Properties must have monomorphic types.


# References
* [QuickCheck][quickcheck]: the original (a bit outdated) manual of the Haskell library
* [Haskell Hedgehog][haskell-hedgehog]
* [The Design and Use of QuickCheck][design-and-use-of-quickcheck]
* [xUnit Theory: Working With InlineData, MemberData, ClassData][xunit-theory]
Videos:

* [The lazy programmer's guide to writing thousands of tests - Scott Wlaschin][lazy-programmer]


# Comments

[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/xxx)

[quickcheck]: https://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html
[haskell-hedgehog]: https://github.com/hedgehogqa/haskell-hedgehog
[design-and-use-of-quickcheck]: https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html

[lazy-programmer]: https://www.youtube.com/watch?v=IYzDFHx6QPY
[xunit-theory]: https://hamidmosalla.com/2017/02/25/xunit-theory-working-with-inlinedata-memberdata-classdata/ 

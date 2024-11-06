---
layout: post
title: "xUnit tips its hat to Property-Based Testing"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- functional programming
- tdd
- pbt
---
Ever wondered why parametric tests in xUnit are called "*theories*"? And
why tests are "*facts*" instead of, well, just "*tests*"?

Investigating on this topic helped me realize that I have always used
xUnit theories incorrectly. And that xUnit loves the idea of Property-Based
Testing..

<!--more-->

I've been tought to use xUnit theories as a mere trick to repeat a
test with different inputs, without incurring in duplication:

```csharp
[Theory]
[InlineData(2, true)]
[InlineData(4, true)]
[InlineData(3, false)]
[InlineData(7, false)]
void check_if_odd_or_even(int number, bool expectedResult)
{
    var actualResult = IsEven(number);
    
    Assert.Equal(expectedResult, actualResult);
}
```

Like in this dummy example, I've always been used  have an
`expectedResult` parameter.

## Theory's purpose

I then realized that this is not the style used in the [xUnit
manual][manual]. The chapter [Write your first theory][write-theory]
never mentions any argument like my `expectedResult`. Instead, it uses
this style:

```csharp
[Theory]
[InlineData(3)]
[InlineData(5)]
[InlineData(6)]
public void MyFirstTheory(int value)
{
    Assert.True(IsOdd(value));
}

bool IsOdd(int value)
{
    return value % 2 == 1;
}
```

It claims that a *property* ("to be odd") holds for a set of inputs.
Indeed, it was introduced by this sentence:

> You may have wondered why your first unit tests use an attribute
> named [Fact] rather than one with a more traditional name like Test.
> xUnit.net includes support for two different major types of unit
> tests: facts and theories. When describing the difference between
> facts and theories, we like to say:
>
> - Facts are tests which are always true. They test invariant
>   conditions.
>
> - Theories are tests which are only true for a particular set of
>   data.

Under this light, *Theories* remind me of *Properties* in
[Property-Based Testing][property].  
Ideally, xUnit designers could have decided to let theories run
against a predicate, and let them assume success if the predicate is
true, failure otherwise:

```csharp
[Theory]
[InlineData(3)]
[InlineData(5)]
[InlineData(6)]
public void MyFirstTheory(int value) =>
    IsOdd(value));
```

Add a way to generate inputs and you would have Property-Based
Testing.

## Further details
Am I going too far?  
I thought so. Then I stubled upon a comment to the [xUnit Issue
#2822 - Why the word "\[Theory\]" as opposed to something like
"\[MultiFact\]"?][issue-2822]:


[Ruben Bartelink](https://github.com/bartelink) observed in his [comment](https://github.com/xunit/xunit/discussions/2822#discussioncomment-7593309):

> It's just naming; The names are accentuating the role it plays vs a
> lower level description of what it technically does/is. [...] you
> can argue it puts authors in the right frame of mind. Or you can
> argue that it's cryptic. But xUnit is definitely designed with lots
> of though put into nuanced design aspects with a view to nudging one
> towards writing good tests, and shaping good systems.


So, this is indeed very intentional and about promoting a specific
testing style. Going ahead (bold is mine):


> In xUnit  
> A Fact is an individual relevant assertion. If you have 3 of them,
> they may or may not mean anything in aggregate.  
> **A Theory is a set of facts that together prove an overall
> concept**  
> **In Property Based Testing, a Property is called that because
> that's its role - It's not a
> `TestWithMultipleInvocationsBasedOnArbitraryData`. We are specifting
> a property of the system under test**.

So, I'm not alone observing a similarity with PBT.

[Brad Wilson](https://github.com/bradwilson), xUnit's caretaker,
[commented](https://github.com/xunit/xunit/discussions/2822#discussioncomment-7602733)
quoting the page [Why Did we Build xUnit 1.0?][why-xunit]:

> The definition of how to run a test method can be extended. There
> are two example of this: the first, in xunitext.dll, is the [Theory]
> attribute which allows data-driven tests; the second, in the
> samples, is the [RepeatTest] attribute which runs a test method
> multiple times in a row. For more information on data theories, see
>  https://homes.cs.washington.edu/~mernst/pubs/testing-theories-tr002-abstract.html.

Down the rabbit hole, here is an excerpt of the paper's abstract (bold
is mine):

> Traditional test suites verify a few well-picked scenarios or
> example inputs. [...] We propose **theory-based testing as
> an adjunct to example-based testing**.  
> **A theory generalizes a (possibly infinite) set of example-based
> tests. A theory is an assertion that should be true for any data,
> and it can be exercised by human-chosen data or by automatic data
> generation.**  

This Theory-Base Testing is really akin to Property-Based Testing.
Which, honestly, I found puzzling, because [QuickCheck][quickcheck]
predates that paper by 9 years.


## My conclusion
xUnit is not a Property-Based Testing library, and it's unlikely to
evolve into one anytime soon.  
In [Property-Based Testing for The Rest of Us][property] I argue that
the value of PBT lies not primarily in the random generation of
inputs; instead, it comes from how it encourages developers to gain a
deeper and reasoned undertanding of requirements, compared to
example-based testing.

My take away is: it is worth to use theories the way they have been
originally conceived and designed. I bet that the mere attempt not to
use any `expectedResult` argument is very likely to bring to more
profound and effective tests.

## References

* [Why Did we Build xUnit 1.0?][why-xunit]
* [xUnit Issue #2822 - Why the word "\[Theory\]" as opposed to
  something like "\[MultiFact\]"?][issue-2822]
* [xUnit manual - Write your first theory][write-theory]
* [Theories in practice: Easy-to-write specifications that catch
  bugs][theories-in-practice]
* [QuickCheck][quickcheck]
* [Property-Based Testing for The Rest of Us][property]
  
[issue-2822]: https://github.com/xunit/xunit/discussions/2822
[why-xunit]: https://xunit.net/docs/why-did-we-build-xunit-1.0
[manual]: https://xunit.net/docs/getting-started/v3/cmdline
[write-theory]: https://xunit.net/docs/getting-started/v3/cmdline#write-your-first-theory
[theories-in-practice]: https://homes.cs.washington.edu/~mernst/pubs/testing-theories-tr002-abstract.html
[quickcheck]: https://www.cse.chalmers.se/~rjmh/QuickCheck/
[property]: https://arialdomartini.github.io/property-testing


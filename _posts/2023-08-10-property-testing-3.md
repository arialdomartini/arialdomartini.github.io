---
layout: post
title: "Property-based Testing For The Rest Of Us - It's properties all the way down"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
include_in_index: false
tags:
- tdd
- functional programming
- property-based testing
---
## Index
1. [Utterly opinionated introduction to Property Testing](property-testing)
2. [Shut up and code!](property-testing-2)
3. It's properties all the way down
4. [Property-driven Development](property-testing-4)

# It's properties all the way down

- [A matter of naming](#a-matter-of-naming)
- [Properties](#properties)
    - [Essential is better than Collateral](#essential-is-better-than-collateral)
- [References](property-testing-references)
<!-- markdown-toc end -->


# A matter of naming
I always suspected that the low adoption of TDD might be partly due to its poor naming. Some developers who never practiced TDD find it counterintuitive that they are supposed to write a test for an implementation even before that implementation exists. How can you blame them? It really sounds crazy.

If only tests were presented as *requirements expressed with code*, the same skeptic developers would probably find TDD completely natural: of course you produce the code only after the requirements! Of course it would be absurd to write requirements as an afterthought.<br/>
If TDD was called *Requirement-Driven Design*, maybe there would be less resistance to the approach.

Along these lines, I think it would be fair to call TDD "*Example-Driven Development*", and PBD "*Requirement-Driven Development*".

This leads us to the definition of "Property", which I intentionally pushed back as much as possible. Before, I wished you to build an intuition of what PBT was really about.

# Properties
A Property is an observation on a piece of code that we expect to hold true regardless of the inputs.

To a certain extent, an assertion in TDD is a property that holds for a specific input. Take this case:

```csharp
record Product(Guid Id, string Name, Category Category, decimal Price);

[Fact]
void books_can_be_shipped_internationally()
{
    var product = new Product(
        Id: Guid.NewGuid(),
        Name: "The Little Schemer", 
        Category: Books, 
        Price: 16.50M);
    
   
    var canBeShipped = Ship(product, Countries.France);
	
    Assert.True(canBeShipped);
}
```

The property here is

```
The book 'The Little Schemer' can be shipped to France
```

or if you like

```
∃ Product "The Little Schemer" ∈ Books | it can be shipped`
```

In PBT the property is extended to

```
All the books can be shipped to France
```

or equivalently

```
∀ Product ∈ Books | it can be shipped
```

which translates to:

```csharp
[Property]
Property books_can_be_shipped_to_France()
{
    Gen<Product> books = 
        Arb.Generate<Product>()
        .Where(p => p.Category == Books);

    bool canBeShippedToFrance(Product product) =>
        Ship(product, Countries.France) == true;

    return Prop.ForAll(books, canBeShippedToFrance);
}
```


Playing with mathematical terms, one could say that 

* TDD resorts to *Existential Quantified Properties*: "*it exists (`∃`) an example for which a property holds*"
* PBT uses *Universally Quantified Properties*: "*for all the values (`∀`) this property holds*".<br/>No surprises that many PBT libraries define a function called `forAll`.

Besides Existential and Universally Quantified properties, there is another dimension along which you can distinguish what I call the *Essential* and the *Collateral* Properties.

* An *Essential Property* is the direct translation of the business requirement, like the example of the shipped books.<br/>Alternative names are *Obvious Property* and *Business Rule as Property*. You can read a Java example in Johannes Link's [Pattern: Business Rule as Property](https://blog.johanneslink.net/2018/07/16/patterns-to-find-properties/#pattern-business-rule-as-property)


* A *Collateral Property* is any observation that holds true in a context, and that can be indirectly deriveded from the business requirement. For example:
  * the fact that `sum(a, b)` is commutative
  * the observation that sorting a collection does not change its size (a so called "*invariant*")
  * the fact that in a bank transfer the sum of money between the two involved bank accounts remains constant (again, an invariant)
  * comparing your program's behavior with an *Test Oracle*, that is an alternative, simpler and predictable implementaion of the function under test (see [Test Oracle][test-oracle] on Wikipedia)
  * running both your system and a simplified model with the same randomized series of input values, and comparing the output and the state. This is called [Model-based testing][model-based-testing] and it is particularly powerful for stateful applicatins. There are entire article series dedicated to it.
  
Collateral Properties don't directly capture the requirements, but derive from them.<br/>
Also, in general they don't completely specify the behaviour of the code under test: usually you need a set of them to form a complete specification.

Collateral Properties are so popular in PBT &mdash; and in Design by Contract &mdash; that one could think they are specific to it.  There is the myth that developers must rack their brains to tranlate business requirements to mysterious mathematical properties such as commutativity, monotonicity and right-identity, and that consequently PBT is unfeasible for real-world scenarios.<br/> 
It's not at all like this.

Collateral Properties are more concrete than this, and they are often fun to find and implement. You can learn a lot about them from:

* Scott Wlaschin's [Choosing properties for property-based testing][choosing-properties]
* John Hughes's [How to Specify it!][how-to-specify-it] for which Johannes Link has written a brilliant version in Java, titled [How to Specify it! In Java!][how-to-specify-it-in-java].
* from Johannes Link's [Patterns to Find Good Properties][patterns-to-find-good-properties]
* from [Property-based Testing Patterns][property-based-testing-patterns]

## Essential is better than Collateral
I might be a voice outside the chorus, but I think Collateral Properties are a bit overrated &mdash; and often excessively feared. 

Many PBT tutorials start with the infamous reversal of a list example. I've never being completely happy with the classical implementation, exactly because it is based on a Collateral Property. The idea is that reversing a list can be tested observing that reversing the reversal of a list gives back the original list:

```csharp
[Property]
bool invariant_of_reversal(List<string> xs) => 
    AreListsEqual(xs, Revert(Revert(xs)));}
```

This says nothing about what *reversing a list* is, about its essence. It tackles a side effect, a derived observation.<br/>
In fact, it passes for the following dishonest implementation without batting an eyelid:

```csharp
IEnumerable<string> Reverse(IEnumerable<string> xs) => xs;
```

This sucks.

If you really wanted to capture the essence of revesing a list, you should start from the requirement:

```
Reversing a list is the action of changing 
the order of elements so that 
they appear in the opposite sequence:
the first element becomes the last element, 
the second element becomes the second-to-last element, 
and so on, until the last element 
becomes the first element.
```

Then you could try with a direct translation to an Essential Property. That would lead to something like:


```csharp
[Property]
bool specification_of_reversal(List<string> xs)
{
    var reversed = Reverse(xs);

    var eachItemHasBeenReversed =
        Enumerable.Range(0, xs.Count)
            .All(i => xs[i] == reversed[xs.Count - i - 1]);

    return eachItemHasBeenReversed;
}
```

This easily spots the dishonest implementation. Hurray!


In the [last part](property-testing-4) I show you how applying property testing affects the practice of TDD. Grab a fruit, bite it and let's get started.


# References
See [References](property-testing-references)


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/22)


[quickcheck]: https://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html
[fscheck]: https://fscheck.github.io/FsCheck/
[hedgehog]: https://hedgehog.qa/
[jqwik]: https://jqwik.net/
[junit-quickcheck]: https://pholser.github.io/junit-quickcheck/site/1.0/
[quicktheories]: https://github.com/quicktheories/QuickTheories
[scala-check]: https://scalacheck.org/
[test.check]: https://github.com/clojure/test.check
[kotest]: https://github.com/kotest/kotest
[hypothesis]: https://hypothesis.works/
[fast-check]: https://github.com/dubzzz/fast-check
[js-verify]: https://github.com/jsverify/jsverify
[stream_data]: https://github.com/whatyouhide/stream_data
[design-and-use-of-quickcheck]: https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html
[xunit-theory]: https://hamidmosalla.com/2017/02/25/xunit-theory-working-with-inlinedata-memberdata-classdata/ 
[universal-quantification]: https://en.wikipedia.org/wiki/Universal_quantification
[universal-quantifier]: https://ncatlab.org/nlab/show/universal+quantifier
[choosing-properties]: https://fsharpforfunandprofit.com/posts/property-based-testing-2
[model-based-testing]: https://en.wikipedia.org/wiki/Model-based_testing
[model-based-testing-fsharp]: https://fscheck.github.io/FsCheck//StatefulTestingNew.html
[model-based-testing-hedgehog]: https://jacobstanley.io/how-to-use-hedgehog-to-test-a-real-world-large-scale-stateful-app/
[model-based-testing-java]: https://johanneslink.net/model-based-testing/
[model-based-testing-makina]: https://hexdocs.pm/makina/readme.html#using-makina
[how-to-specify-it]: https://www.dropbox.com/s/tx2b84kae4bw1p4/paper.pdf
[how-to-specify-it-video]: https://www.youtube.com/watch?v=G0NUOst-53U
[test-oracle]: https://en.wikipedia.org/wiki/Test_oracle
[how-to-specify-it-in-java]: https://johanneslink.net/how-to-specify-it
[concolic-testing]: https://en.wikipedia.org/wiki/Concolic_testing
[crosshair]: https://github.com/pschanely/CrossHair
[property-based-testing-in-java]: https://blog.johanneslink.net/2018/03/24/property-based-testing-in-java-introduction/
[properties-are-easier]: https://blog.ploeh.dk/2021/02/15/when-properties-are-easier-than-examples/
[integrated-vs-type-based-shrinking]: https://hypothesis.works/articles/integrated-shrinking/
[patterns-to-find-good-properties]: https://blog.johanneslink.net/2018/07/16/patterns-to-find-properties/#patterns-to-find-good-properties
[lazy-programmer]: https://www.youtube.com/watch?v=IYzDFHx6QPY
[the-three-laws-of-tdd]: https://www.youtube.com/watch?v=qkblc5WRn-U
[the-prime-factor-kata]: http://www.butunclebob.com/ArticleS.UncleBob.ThePrimeFactorsKata
[prime-factorization-wolfram]: https://mathworld.wolfram.com/PrimeFactorization.html
[tdd-by-example]: https://www.pearson.com/en-us/subject-catalog/p/test-driven-development-by-example/P200000009421/9780321146533
[property-driven-development]: https://blog.johanneslink.net/2019/05/11/property-based-driven-development/
[triangulation-in-tdd]: https://dmitripavlutin.com/triangulation-test-driven-development/
[time-travelling]: https://wickstrom.tech/2019-11-17-time-travelling-and-fixing-bugs-with-property-based-testing.html
[bug-hunting]: https://johanneslink.net/how-to-specify-it/#5-bug-hunting
[property-based-testing-patterns]: https://blog.ssanj.net/posts/2016-06-26-property-based-testing-patterns.html

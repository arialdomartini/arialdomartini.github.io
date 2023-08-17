
---
layout: post
title: "Property-based Testing For The Rest Of Us - 2"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- bash
- zsh
---
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [A matter of naming](#a-matter-of-naming)
- [Properties](#properties)
- [References](#references)

<!-- markdown-toc end -->


# A matter of naming
I always suspected that the low adoption of TDD might be partly due to its poor naming. Some developers who never practiced TDD find it counterintuitive that they are supposed to write a test for an implementation even before that implementation exists. How can you blame them? It really sounds crazy.<br/>
If only tests were presented as the *requirements expressed with code*, the same skeptic developers would probably find TDD completely natural: of course you produce the code only after the requirements! Of course it would be absurd to write requirements as an afterthought.<br/>
If TDD was called *Requirement-Driven Design*, maybe there will be less resistance to the approach.

Along these lines, I think it would be fair to call TDD "Example-Driven Development", and PBD "Requirement-Driven Development".

This leads us to the definition of "Property", which I intentionally pushed back as much as possible. Before, I wished you to build an intuition of what PBT was really about.

# Properties
A Property is an observation on a piece of code that we expect to hold true regardless of the inputs.

To a certain extent, an assertion in TDD is a property that holds for a specific input:

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

The property here is that "*The book 'The Little Schemer' can be shipped to France*"  (`∃ Product "The Little Schemer" ∈ Books | it can be shipped`).

In PBT the property is  "All the books can be shipped to France" (`∀ Product ∈ Books | it can be shipped`):

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

* TDD resorts to Existential Quantified Properties: "*it does exist (`∃`) an example for which a property holds*"
* PBT uses Universally Quantified Properties: "*for all the values (`∀`) this property holds*". No surprises that all the PBT libraries define a function called `ForAll`.

Besised Existential and Universally Quantified properties, there is another dimension along which you can distinguish what I call the *Essential Properties* and the *Collateral* one

* An *Essential Property* is the direct translation of the business requirement, like the example of the shipped books.

* A *Collateral Property* is any observation that holds true in a context, and that can be indirectly deriveded from the business requirement. For example:
  * the fact that `sum(a, b)` is commutative
  * the observation that sorting a collection does not change its size (a so called "invariant")
  * the fact in a bank transfer the sum of money between the two involved bank accounts remains constant (again, an invariant)
  * running both your system and a simplified model with the same input, and comparing the outputs (this is called [Model-based testing][model-based-testing])

Collateral Properties are so popular in PBT &mdash; and in Design by Contract &mdash; that one could think they are specific to it. There is the myth that developers must rack their brains to test complex business rules translating them to mysterious mathematical properties such as commutativity and associativity, and that because of this PBT is only theoretical and utterly unfeasible for real-world scenarios. It's not at all like this. Collateral Properties are very much  down-to-earth, and also fun to implement. You can learn a lot about them from Scott Wlaschin's [Choosing properties for property-based testing][choosing-properties] and John Hughes's [How to Specify it!][how-to-specify-it].

## Essential is better than Collateral
Many PBT tutorials start with the infamous reversal of a list example. I've never being completely happy with the classical implementation, because it is based on a Collateral Property:

```csharp
[Property]
bool invariant_of_reversal(List<string> xs) => 
    AreListsEqual(xs, Revert(Revert(xs)));}
```

This says nothing about what *reversing a list" is.<br/>
In fact, it passes for the following dishonest implementation without batting an eyelid:

```csharp
IEnumerable<string> Reverse(IEnumerable<string> xs) => xs;
```

This sucks.

If you really wanted to capture the essence of the requirement:

```
Reversing a list is the action of changing the order of elements 
so that they appear in the opposite sequence:
the first element becomes the last element, 
the second element becomes the second-to-last element, 
and so on, until the last element becomes the first element.
```

you could try with a direct translation to an Essential Property, that would lead to something like:


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


# That's all
I hope you grasped the intuition. And that, if you think about it, in a way all tests are about properties. And that more or less, you already knew how to write a property.

Finally, I'm sure: if you are that kind of developer who likes to think by abstractions, you could have invented Property-based Testing already, and probably you had.

Happy testing, and have a great day!


# References
* [QuickCheck][quickcheck]: the original (a bit outdated) manual of the Haskell library
* [Haskell Hedgehog][haskell-hedgehog]
* [The Design and Use of QuickCheck][design-and-use-of-quickcheck]
* [xUnit Theory: Working With InlineData, MemberData, ClassData][xunit-theory]
* Discovering properties
  * [Choosing properties for property-based testing - Scott Wlaschin][choosing-properties]
  * [How to Specify it! - John Hughes][how-to-specify-it]
* Universal Quantification
  * [Universal Quantification][universal-quantification]
  * [Universal Quantifier - in ncatlab.org][universal-quantifier]
* Model-based Testing
  * [Model-based Testing][model-based-testing]
  * [Model-based Testing with Hedgehog][model-based-testing-hedgehog]
  * [Model-based Testing with FsCheck][model-based-testing-fsharp]
  * [Model-based Testing in Java with jqwik][model-based-testing-java]
  * [Model-based Testing with Makina][model-based-testing-makina]

Videos:

* [The lazy programmer's guide to writing thousands of tests - Scott Wlaschin][lazy-programmer]
* [How to Specify it! - John Hughes][how-to-specify-it-video]


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

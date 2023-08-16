
---
layout: post
title: "Property-based Testing For The Rest Of Us - 2"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- bash
- zsh
---
# A matter of naming
I always suspected that the low adoption of TDD might be partly due to its poor naming. Some developers who never practiced TDD find it counterintuitive that they are supposed to write a test for an implementation even before that implementation exists. How can you blame them? It really sounds crazy.<br/>
If only tests were presented as the *requirements expressed with code*, the same skeptic developers would probably find TDD completely natural: of course you produce the code only after the requirements! Of course it would be absurd to write requirements as an afterthought.<br/>
If TDD was called *Requirement-Driven Design*, maybe there will be less resistance to the approach.

Along these lines, I think it would be fair to call TDD "Example-Driven Development", and PBD "Requirement-Driven Development".

This leads us to the definition of "Property", which I intentionally postponed as much as possible. Indeed, I wanted you to build an intuition on what PBT was really about, before.

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

* A *Collateral Property* is any observation that holds true in a context, and that can be indirectly deriveded from the business requirement. For example, the following facts:
  * `sum(a, b)` is commutative
  * sorting a collection does not change its size (a so called "invariant")
  * in a bank transfer the sum of money between the two involved bank accounts remains constant (again, an invariant)

Collateral Properties are so popular in PBT &mdash; and in Design by Contract &mdash; that one could think they are specific to it. There is the myth that developers must rack their brains to test complex business rules translating them to mysterious mathematical properties such as commutativity and associativity. It's not at all like this. They are much more down-to-earth (you can learn a lot about them from Scott Wlaschin's [Choosing properties for property-based testing][choosing-properties]).



In a way, all tests are about properties. And you think about it, you already knew how to do property-based testing, didn't you?



## Testing is an act of awareness and design

Proponents of formal methods sometimes stress the notion of specification above that of implementation. However it is the inconsistencies between these two independent descriptions of the desired behavior that reveal the truth. We discover incomplete understanding in the specs and bugs in the implementation. Programming does not flow in a single direction from specifications to implementation but evolves by cross-checking and updating the two. Property-based testing quickens this evolution.
(from [Design and Use of QuickCheck][design-and-use-of-quickcheck])





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
* [Choosing properties for property-based testing - Scott Wlaschin][choosing-properties]
* [Universal Quantification][universal-quantification]
* [Universal Quantifier - in ncatlab.org][universal-quantifier]

Videos:

* [The lazy programmer's guide to writing thousands of tests - Scott Wlaschin][lazy-programmer]


[xunit-theory]: https://hamidmosalla.com/2017/02/25/xunit-theory-working-with-inlinedata-memberdata-classdata/ 
[universal-quantification]: https://en.wikipedia.org/wiki/Universal_quantification
[universal-quantifier]: https://ncatlab.org/nlab/show/universal+quantifier
[choosing-properties]: https://fsharpforfunandprofit.com/posts/property-based-testing-2

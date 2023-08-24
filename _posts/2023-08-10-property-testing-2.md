---
layout: post
title: "Property-based Testing For The Rest Of Us - Shut up and code!"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
include_in_index: false
tags:
- tdd
- functional programming
- property-based testing
---
## Index
1. [Utterly opinionated introduction to Property Testing](property-testing)
2. Shut up and code!
3. [It's properties all the way down](property-testing-3)
4. [Property-driven Development](property-testing-4)

# Shut up and code!

- [Let's code](#lets-code)
    - [Anatomy of a property test](#anatomy-of-a-property-test)
        - [Shrinkers](#shrinkers)
    - [Code, finally](#code-finally)
        - [Unravel the mysterious types](#unravel-the-mysterious-types)
        - [Wait, where's the assertion?](#wait-wheres-the-assertion)
    - [Toward real-world use cases](#toward-real-world-use-cases)
        - [Testing a DB repository](#testing-a-db-repository)
        - [A more complex functionality](#a-more-complex-functionality)
- [References](property-testing-references)
<!-- markdown-toc end -->


# Let's code
## Anatomy of a property test
Let's delve deeper into the concept of utilizing generators as modular components that are simple to combine and layer one upon the other.

<!-- A viable strategy could involve maintaining a distinction between the problem of generating values and enabling compositional structure. -->

For generating a random value, we need a function taking:

* the type of data to generate (so a function generic, say, on `List<int>`)
* a source of randomicity (such as a `new Random(seed)` instance)
* a size parameter for controlling the scale of generation (such as the length of the generated lists)

It's a good idea to wrap each data generating function into a container, so that you can compose uniformally shaped containers independently from their content. If you are familiar with Functional Programming, you would be probably thinking of data structures with Functor, Applicative and Monad instances. And you would be right.


Indeed, in Haskell `Gen` is defined as a `newtype`:

```haskell
newtype Gen a = MkGen {
    unGen :: QCGen -> Int -> a
}
```

FsCheck defines it in C# as:

```csharp
public sealed class Gen<a> : IGen
{
    internal readonly FSharpFunc<int, FSharpFunc<Random.StdGen, a>> item;

    internal Gen(FSharpFunc<int, FSharpFunc<Random.StdGen, a>> item) => this.item = item;
    
    ...
```

Hedgehog's definition boils down to something similar:


```fsharp
public struct Random<a>
{
    public static Random<a> NewRandom(FSharpFunc<Seed, FSharpFunc<int, a>> item) => new Random<a>(item);
    
    ...
```

In all cases, as you see, there is a function with the signature:

```haskell
Generator :: Random -> Size -> data
```

wrapped inside a container. 

You can imagine a Generator as the recipe describing the shape and rules of the desired test data: you can manipulate it in its container prior the execution, and combine it to build more elaborated generators.<br/>
When you are done, you feed it with a source of randomicity and a concrete size, so it starts emitting actual data.

The wrapping structure is designed to allow monadic effects, so that composing it is no different than composing Options, Eithers, Promises and other structures from the functional space. In a sense, nothing new to learn.

### Shrinkers
A last little note before getting our hands dirty.<br/>
I mentioned that, when PBT libraries find a counterexample, they narrow down it to the minimum relevant value, to simplify your life. This operation is performed by the so called *shrinkers*. You don't need to deal with them directly just yet: just be informed that, once you created a generator, in some libraries you need to wrap it into a more sophisticated structure, called `Arbitrary`, which adds shrinking capabilities. That's true in the QuickCheck family libraries.<br/>
Other libraries have integrated shrinkers, and they derive a shrinker the moment you define a generator, making sure that the same domain preconditions used during generation are preserved while shrinking. More on this on [Hypothesis - Integrated vs type based shrinking][integrated-vs-type-based-shrinking] and on [The Shrinking Challenge][the-shrinking-challenge].

Shrinking is probably the most useful feature of a PBT library because it generates counterexamples in which every element is relevant to the failure. It's easily your best allied during debugging and troubleshooting. We will see this in action in the last installment, with the Prime Factors Kata.

## Code, finally

Each library defines default generators for most the standard types. Let's finally write a real, runnable property test using one of such default generators:

```csharp
using FsCheck;
using FsCheck.Xunit;
using static FsCheck.Prop;

public class PropertyTesting
{
    [Property]
    Property square_of_numbers_are_non_negative()
    {
        Arbitrary<int> numbers = Arb.From<int>();

        int square(int n) => n * n;

        bool squareIsNotNegative(int n) => square(n) >= 0;

        return ForAll(numbers, squareIsNotNegative);
    }
}
```

The last line

```csharp
ForAll(numbers, squareIsNotNegative)
```

captures the requirement, although a very simple one.<br/>
If you prefer to see this property test as a one-liner, here it is:

```csharp
ForAll(Arb.From<int>(), n => n * n >= 0);
```

This test, fed to FsCheck, results in the execution of 100 tests.
Pretty neat, isn't it?

### Unravel the mysterious types
Focus on:

```csharp
Arbitrary<int> numbers = Arb.From<int>();
```

Is that a random number? A collection of random numbers?<br/>
It's neither. `Arbitrary<int>` is exactly the container we discussed earlier: it  contains a function that, when fed with a source of randomicity and a size, emits random values of `int`.

To better understand this, let's try to actually feed this `Arbitrary<int>` instance with a source of randomicity and a size. This can be done with `Gen.Sample()`. In the following snippet we are asking the function wrapped in `Arb.From<int>()` to generate `100` numbers smaller than `50`:

```csharp
[Fact]
void what_is_an_Arb()
{
    Arbitrary<int> arbitraryNumber = Arb.From<int>();

    var ns = Gen.Sample<int>(50, 100, arbitraryNumber.Generator).ToList();
        
    Assert.Equal(100, ns.Count);
    Assert.True(ns.TrueForAll(n => n <= 50));
}
```

Notice that you are not just generating random numbers: you are generating random numbers *that satisfy a custom domain rule you defined*, in this case simply *being smaller than `50`*.

If `arbitraryNumber` is neither an `int` nor a collection of `int`, of course you cannot directly use it to feed the `squareIsNotNegative(int n)` function.

This is one of the challenges in Property Testing. While in TDD you can just manage values, in PBT you have to deal with *abstractions* of values.<br/>
If you are familiar with Functional Programming, this concept should not sound new: instead of operating on primitive types, you lift all the types to an *elevated world*, where the code operates in an *effectful context*. 
The effect in the Property Testing elevated world is a controlled *randomicity*.

### Wait, where's the assertion?
Let's inspect again the property test we wrote:

```csharp
[Property]
Property square_of_numbers_are_non_negative()
{
    Arbitrary<int> numbers = Arb.From<int>();
    int square(int n) => n * n;
    bool squareIsNotNegative(int n) => square(n) >= 0;

    return ForAll(numbers, squareIsNotNegative);
}
```

Notice that it does not close with an assertion. That's disturbing! Worse, neither is it a `Fact` returning `void`: it's a function returning a `Property`. It all looks weird and magical.<br/>
In fact, it's just a bit of syntactic sugar. Let me write this property as a classical xUnit `Fact`:

```csharp
[Fact]
void square_of_numbers_are_non_negative_as_a_fact()
{
    Arbitrary<int> numbers = Arb.From<int>();
    int square(int n) => n * n;
    bool squareIsNotNegative(int n) => square(n) >= 0;

    Property property = ForAll(numbers, squareIsNotNegative);
        
    Check.QuickThrowOnFailure(property);
}
```

`ForAll` is a method that feeds the Generator with some source of randomicity and a default size (`100`, specifically), and which generates a `Property`. In other words, `ForAll` does not execute the test just yet: it gives you back an instance of `Property` that you might possibly compose with something else before the eventual execution. Yes, functional programmers have a real obsession with composition.

The actual assertion is performed by the final `Check.QuickThrowOnFailure(property)`.<br/>
If you crack open the xUnit code, you will convince yourself that an xUnit assertion is nothing but a piece of code that raises an exception when a particular condition holds.<br/>
xUnit's `Assert.True()` boils down to:

```csharp
public static void True(bool? condition, string userMessage)
{
    if (!condition.HasValue || !condition.GetValueOrDefault())
        throw new TrueException(userMessage, condition);
}
```

PBT libraries rely on this. `Check.QuickThrowOnFailure(property)` verifies all the generated predicates, and if one does not hold, it throws an exception, for xUnit to interpret as a failed test.<br/>
You can save some keyboard hits by decorating the test method with `[Property]` and returning an instance of `Property` instead of `void`. The PBT library will call `Check.QuickThrowOnFailure()` for you. No rocket science. We will see later even more concise ways to write a property test, as a simple predicate.

In Hedgehog, with F#, the syntax is a bit different:

```fsharp
test "Square of any number is not negative" {
    let numbers = Gen.int32 (Range.linear 0 100)
              
    let square n = n * n

    let squareIsNotNegative n = square n >= 0
	                            |> Property.ofBool

    numbers |> Property.forAll squareIsNotNegative |> Property.check
}
```

Notice how the `squareIsNotNegative` predicate in Hedgehog is passed to `Property.ofBool` so that it is wrapped into a higher level, composable `Property` structure.


## Toward real-world use cases
You don't have to think this approach only works with simple mathematical statements.<br/>

Let's see some more realistic examples.

Say you developed a serialization library. Testing it translates to making sure that:

* serializing an instance creates a string 
* deserializing that string brings the original instance back to life

```csharp
[Property]
Property serialization_deserialization_roundtrip()
{
    Arbitrary<Product> products = Arb.From<Product>();

    bool roundtripLooseNoInformation(Product product) =>
        Deserialize(Serialize(product)) == product; 

    return Prop.ForAll(products, roundtripLooseNoInformation);
}
```

Notice how both the arbitrary and the property are defined for a specific type. In fact, Property must be monomorphic: to my knowledge, there is no library able to test multiple types in polimorphic properties.

### Testing a DB repository
Nothing prevents you to do integration tests via a property. After all, a property test is an ordinary test, whose input is created out of thin air.

Very similarly to the serialization case, you could test that your `ProductRepository` is able to save a product on the db.

```csharp
record Product(Guid Id, string Name, decimal Price, Category Category);

public class ProductRepositoryPropertyTests
{
    private Repository _repository;

    public ProductRepositoryPropertyTests()
    {
        // setup your test DB
    }

    [Property]
    Property products_can_be_persisted()
    {
        Arbitrary<Product> products = Arb.From<Product>();

        bool canBeSavedOnDb(Product product)
        {
            _repository.Save(product);

            var found = _repository.LoadById(product.Id);

            return found == product;
        }

        return Prop.ForAll(products, canBeSavedOnDb);
    }
}
```

You got the idea. If you focus on the content of `canBeSavedOnDb()` you see this is pretty much an ordinary test.

Since a property is always expressed via a predicate, you can even make the test shorter and just write:

```csharp
[Property]
bool products_can_be_persisted_as_a_predicate(Product product)
{
    _repository.Save(product);

    var found = _repository.LoadById(product.Id);

    return found == product;
}
```

This is a perfectly legit property test. When executed, this will run as a Theory of 100 tests.<br/>
It does not look intimidating, does it?


### A more complex functionality
The previous approach works as long as you don't need to fine tune the creation of the input value. Let's see something more challenging.<br/>
Recoll the original requirement:

```
Food products are restricted from international shipping 
due to regulatory compliance, 
unless there is an active Promotion
```

With FsCheck in C# this could be translated to something like:


```csharp
bool Ship(Product product, Country country, Promotion promotion) => ...

[Property]
Property food_is_restricted_from_international_shipping_unless_there_is_an_active_promotion()
{
    Arbitrary<UseCase> useCases = Arb.From(
        from today in AnyDate
        from country in InternationalCountries
        from product in FoodProducts
        from promotion in ValidPromotion(today)
        select new UseCase(Today: today, Country: country, Product: product, Promotion: promotion));

    bool internationalShippingIsAllowed(UseCase useCase) =>
        Ship(useCase.Product, useCase.Country, useCase.Promotion) == true;

    return Prop.ForAll(useCases, internationalShippingIsAllowed);
}
```

I suggest to read it from bottom to top.

Notice how, instead of asking FsCheck to just inject any `Product`, the test is forging its input in a very specific way. It creates multiple correlated instances 

* an arbitrary date representing today
* a list of international destinations
* a product belonging to a specific category
* a promotion that is valid today

All is wrapped in a `UseCase` record.

Each input is generated by a `Gen` instance, and then combined with the other generators.<br/>
Random dates are created with:

```csharp
private readonly Gen<DateTime> AnyDate = 
    Arb.Generate<DateTime>();
```

The international countries are all the countries that are not local, whatever definition of local is in place:

```csharp
private static readonly Gen<Country> LocalCountry =
    Arb.Generate<Country>();
    
Gen<Country> InternationalCountries =
    from country in Arb.Generate<Country>()
    from local in LocalCountry
    where country != local
    select country;
```

Similarly, a food product is generated with:

```csharp
record Product(Guid Id, string Name, Category Category, decimal Price);

Gen<Product> FoodProducts =
    from name in Arb.Generate<string>()
    from price in Arb.Generate<decimal>()
    from category in Arb.Generate<Category>().Where(isFood)
    select new Product(name, price, category);
```

A valid promotion today (given that `today` is an arbitrary date), is:


```csharp
private readonly Gen<int> PositiveNumber = Arb.Generate<int>().Select(Math.Abs);

Gen<Promotion> ValidPromotion(DateTime today) =>
    from name in Arb.Generate<string>()
    from daysBefore in PositiveNumber
    from daysAfter in PositiveNumber
    select new Promotion(
        Name: name,
        ValidFrom: today.AddDays(-daysBefore),
        ValidTo: today.AddDays(daysAfter));
```

As you see, it's all very generic. No concrete values are ever provided.

This is honestly a mouthful of code, isn't it? Consider though that once defined you will reuse generators over and over in several tests.<br/>
Also, the equivalent in F# and Haskell is way more concise.<br/>


Time for a cake, before moving to [It's properties all the way down](property-testing-3)


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
[the-shrinking-challenge]: https://github.com/jlink/shrinking-challenge

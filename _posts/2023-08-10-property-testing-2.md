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

- [Let's code](#lets-code)
    - [Anatomy of a property test](#anatomy-of-a-property-test)
        - [Shrinkers](#shrinkers)
    - [Code, finally](#code-finally)
        - [Unravel the mysterious types](#unravel-the-mysterious-types)
        - [Wait, where's the assertion?](#wait-wheres-the-assertion)
    - [Toward real-world use cases](#toward-real-world-use-cases)
        - [Testing a DB repository](#testing-a-db-repository)
        - [A more complex functionality](#a-more-complex-functionality)

<!-- markdown-toc end -->


# Let's code
## Anatomy of a property test
Since Generators are meant to be composable, rather than simple functions emitting random values, they are wrappers of functions. We could use polymorphic functions taking:

* a random generator source
* a size parameter

and emitting

* values of a given type.

You can imagine a Generator as the recipe describing the shape and rules of the desired test data. Once fed it with a source of randomicity and a concrete size, it starts emitting actual data.

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

In all cases, as you see, it's a wrapped function with the signature:

```haskell
Generator :: Random -> Size -> data
```


The wrapping structure is designed to allow monadic effects, so to easily combine generators.

### Shrinkers
A last little note before getting our hands dirty.<br/>
I mentioned that, when PBT libraries find a counterexample, they narrow down it to the minimum relevant value, to simplify your life. This operation is performed by the so called *shrinkers*. You don't need to deal with them directly just yet: just be informed that, once you created a generator, in some libraries you need to wrap it into a more sophisticated structure, called `Arbitrary`, which adds shrinking capabilities. That's true in the QuickCheck family libraries. Other libraries have embedded shrinkers, that is, they create a shrinker the moment you define a generator.

Shrinking is probably the most useful feature of a PBT library because it generates counterexamples in which every element is relevant to the failure. It`s easily your best allied during debugging and troubleshooting.

## Code, finally

Each library defines default generators for most types. Let's finally write a real, runnable property test usinh a default generator:

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

Focus on the Point Free expression:

```csharp
Prop.forAll numbers squareIsNotNegative
```

It captures the requirement, although a very simple one.<br/>
If you prefer to see this property test as a one-liner, here it is:

```csharp
ForAll(Arb.From<int>(), n => n * n >= 0);
```

This test, fed to FsCheck, results in the execution of 100 tests.
Pretty neat, isn't it?

### Unravel the mysterious types
There are some puzzling elements in these test, though. Focus on:

```csharp
Arbitrary<int> numbers = Arb.From<int>();
```

Is that a random number? A collection of random numbers?<br/>
It's neither. It is the instance of a structure that, when fed with a source of randomicity and a size, can emit random numbers. It's the structure wrapping our random-generationg functions.

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

If `arbitraryNumber` is neither an `int` nor a collection of `int`, of course you cannot directly use it to feed the `squareIsNotNegative(int n)` function.<br/>
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

Notice that it does not close with an assertion. That's disturbing! Worse, it is neither a `Fact`, returning `void`. It's a function returning a `Property`. It all looks weird and magical.<br/>
In fact, that was just a bit of syntactic sugar. Let me write this property as a classical xUnit Fact:

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

The assertion is actually performed by the final `Check.QuickThrowOnFailure(property)`.<br/>If you crack open the xUnit code, you will convince yourself that an xUnit assertion is nothing but a piece of code that raises an exception when a particular condition holds. `Assert.True()` in xUnit boils down to:

```csharp
public static void True(bool? condition, string userMessage)
{
    if (!condition.HasValue || !condition.GetValueOrDefault())
        throw new TrueException(userMessage, condition);
}
```

FsCheck relies on this. `Check.QuickThrowOnFailure(property)` verifies all the generated predicates, and if one does not hold, it throws an exception that finally xUnit interprets as a failed test.<br/>
Decorating a method with `[Property]` and returning an instance of `Property` just does the same, saving you from calling `Check.QuickThrowOnFailure()`.  No rocket science.


The same test with in F# would look like this:

```fsharp
open Expecto
open FsCheck

[<Tests>]
let treeTests =
    testProperty
        "Square of any number is not negative"
        (let numbers = Arb.from<int>
         let square n = n * n
         let squareIsNotNegative (n: int) : bool = square n >= 0

         Prop.forAll numbers squareIsNotNegative)
```


`let squareIsNotNegative n = square n >= 0` is the property you want to prove wrong. As you see, it's just a simple predicate. This is, in essence, the requirement (together with the subset of input values).

In Hedgehog, the syntax is only slighly different:

```fsharp
test "Square of any number is not negative" {
    let numbers = Gen.int32 (Range.linear 0 100)
              
    let square n = n * n

    let squareIsNotNegative n = (square n >= 0) |> Property.ofBool

    numbers |> Property.forAll squareIsNotNegative |> Property.check
}
```

Notice how the `squareIsNotNegative` predicate in Hedgehog is passed to `Property.ofBool` so that it is wrapped into a higher level, composable `Property` structure.


## Toward real-world use cases
You don't have to think this approach only works with simple mathematical statements.<br/>

Let's see some examples, starting from a very simple one.

Say you developed a serialization library. Testing it translates to making sure that 

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

Notice how both the arbitrary and the property are defined for a specific type. In fact, Property must be monomorphic: I don't know of any PBT library supporting polimorphic properties.

### Testing a DB repository
Nothing prevents you to do integration tests via a property. After all, a property test is an ordinary test with arbitrary code, only input data is created out of thin air.

Very similarly to the serialization case, you could test that your `ProductRepository` is able to save a product on the db.

```csharp
record Product(Guid Id, string Name, decimal Price, Category Category);

public class ProductRepositoryPropertyTests
{
    private IProductRepository _repository;

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
bool products_can_be_persisted(Product product)
{
    _repository.Save(product);

    var found = _repository.LoadById(product.Id);

    return found == product;
}
```

By default, this will run as a Theory of 100 tests.<br/>
It does not look intimidating, does it?


### A more complex functionality
The previous approach works as long as you don't need to fine tune the creation of the input value. Let's see something more challenging<br/>
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
* a list of intenarnational destinations
* a product belonging to a specific category
* a promotion that is valid today

All is wrapped in a `UseCase` record.

Each input is generated by `Gen` instance, which are then combined together.<br/>
Random dates are generated with:

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


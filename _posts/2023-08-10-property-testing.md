---
layout: post
title: "Property-based Testing For The Rest Of Us"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- bash
- zsh
---
<!--More-->

It's no secret that getting started with Property-Based Testing (PBT) is hard. This series of articles does not have the presumption of changing this fact. It is merely the outcome of the observations and thoughts I have gathered during my personal journey.<br/>
However, I hope it can be of some help to the fellow programmer.

I will use mostly C# and F# examples, and only a bunch of Haskell bits here and there.


## What's the fuss about?
There is no fuss at all. Property Testing is a niche discipline. It's almost unknown outside the tiny world of Functional Programming. And this is a pity, because it is an amazingly powerful, effective and very rewarding technique.

In the Haskell world it is very popular thanks to QuickCheck, the grandfather of all the PBT libraries. In other ecosystems it is not given the attention it deserves.

## Why is it powerful?
Many would tell you it is because it's great at catching bugs. Some will stress how it similar to fuzzing testing in the way it randomizes the input values.

While all the above are true, I prefer to think I love it for a third reason: it elevates your comprehension of the domain.

Domain Experts communicate with us developers on 2 different levels.<br/>
First, they express the business rules using abstract and strict statements:

| Abstract rules                                                                                                                 |
|--------------------------------------------------------------------------------------------------------------------------------|
| "The catalog always lists products in alphabetical order"                                                                       |
| "Account names are unique and case insensitive"                                                                                |
| "We never apply more than 1 discount promotion to a single purchasing cart;<br/>we always select the most convenient discount" |

Then, to help us understand, they also provide us with some examples:

| Concrete examples                                                                                                                                                                                                       |
|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| "With alphabetical order I mean: Muffin, Coffee, Milk shall be printed as Coffee, Milk, Muffin"                                                                                                                         |
| "About account names, you cannot have 2 "john.doe". <br/>"john.doe" and "John.Doe" are the same account                                                                                                                 |
| "Say a customer purchases `2` cups of coffee, `1` milk and `1` muffin for `4` people.<br/>`4` people are entitled for `Promotion 1`, `20%` discount, `1 EUR`.<br/>Milk and Muffin activates `Promotion 2`, `0.8 EUR`.<br/>In this case, we apply `Promotion 1`" |

Both the levels are important.<br/>
On the one hand, abstract rules are very powerful, because they are concise and they have a general application.<br/>
On the other hand, the examples (which are derivated from abstract rules) ease the comprehension.

Unfortunately, when it comes to translating requirements to tests, we only code with examples. And this is risky: after all, the application must work in all the cases, not only in the few ones covered by the examples.

Indeed, we rarely do any effort for expressing the rules in their more general form.<br/>
Not our fault. We don't because the tools provided by TDD are very much example-based. It's mostly a technical limitation: we just don't know how to translate `"products are always sorted alphabetically"` without resorting to a specific list of products.

If TDD is about coding examples, Property-based Testing is about coding the pure rules.<br/>
PBT provides a way to express the business functionalities abstracting from the specific examples. In a sense, to capture their core essence.<br/>
The selling point of PBT is: it leads you to deepen your understanding of the domain problem by forcing you to write statements independent from specific values.

As a side effect, you will get an excellent tool for catching nasty bugs.

## Show me the code

OK, I see you are impatient. Here we go. I will give you 2 simple examples. In the next pages we will have the time to understand what happends behind the scenes.

Given the following integration test:

```csharp
record Product(Guid Id, string Name, Category Category, decimal Price);

[Fact]
void products_can_be_persisted()
{
    var product = new Product(
        Id: Guid.NewGuid(),
        Name: "The Little Schemer", 
        Category: Books, 
        Price: 16.50M);
    
    _repository.Save(product);

    var found = _repository.LoadById(product.Id);

    Assert.Equal(found, product);
}
```

the equivalent Property-based one would be:


```csharp
[Property]
bool products_can_be_persisted(Product product)
{
    _repository.Save(product);

    var found = _repository.LoadById(product.Id);

    return found == product;
}
```

Basically, the same test, without the specific value for `Product`.

As a second case, here's an F# xUnit Theory for the Fizz Buzz Kata:

```fsharp
[<Theory>]
[<InlineData(15)>]
[<InlineData(30)>]
[<InlineData(45)>]
[<InlineData(60)>]
let ``multiples of 15 return "fizzbuzz"`` (multipleOf15) =
    Assert.Equal("fizzbuzz", fizzbuzz multipleOf15)
```

This can be directly translated to

```fsharp
[<Property>]
let ``All the multiples of 15 return "fizzbuzz"`` () =
    gen {
        let! n = Arb.generate<int>
        let multipleOf15 = n * 15
		
        return fizzbuzz multipleOf15 = "fizzbuzz"
    }
```

Notice how `fizzbuzz multipleOf15 = "fizzbuzz"` is the direct translation of the requirement `All the multiples of 15 return "fizzbuzz"`.

## So, define Property-based Testing
Here's the bold statement.<br/>
Property Testing is TDD on steroids. It is about capturing the essence of business requirements &mdash; rather than some arbitrary, often unmotivated examples &mdash; and having them automatically tested almost as logical or mathematical statements, more or less for free.
## Too good. Where's the catch?
You are right. It would be too good, right?<br/>
I see 3 catches.

**First**: Property-based Testing is not as easy as TDD.<br/>
It's hard to start with, and overtime it keeps being more challenging than ordinary TDD. The libraries supporting it are usually more advanced, as they generally require some knowledge of Functional Programming: you should be prepared to have some understanding of how to write a lambda, how to map it to a functor, what combinators are, how to compose monads and the like.<br/>
But don't despair: those are very very rewarding challenges, you will enjoy them!

**Second**: the technical challelenge is possibly not even the thoughest one: figuring out which properties describe the business behavior is often the most confusing part.<br/>
Writing TDD tests is as easy as finding a collection of reference use cases: the customer ordered an apple (`.5 EUR`) and 3 books (`10 EUR` each), the total should be `30.5 EUR.` Easy peasy.<br/>
Writing Property Tests for an the same e-commerce site is a different kettle of fish: it is not even clear what a "property" is.<br/>
I'm afraid there are no silver bullets here, besides elbow grease and a lot of experience.

**Finally**: PBT's niche nature.<br/>
Compared to TDD, the documentation is not likely copious and the typical examples you can find online have often deceptively simple code, not directly applicable to real-world use cases. If you are looking for answers to your down-to-Earth needs, you will be disappointed to discover that much of the documentation will teach you over and over how to test the reversal of a list.<br/>
That sucks, but it's part of the challenge.


<!-- ## How does it work? -->

<!-- PBT is about providing specifications of the program behaviour in the form of Properties that should be always satisfied. An automated library will then test that the Properties hold in a large number of randomly generated cases. Or, better: the library tries to falsify your claims, finding counterexamples of the properties. -->

<!-- Specifications are expressed in the native programming language (C#, F#, Haskell, TypeScript), using combinators defined in the library itself.  -->

<!-- The library automatically generates a comprehensive array of test cases, exercising the software in ways human testers would never imagine, exposing even the most insidious of corner cases. Failures are automatically simplified, giving developers coherent, intelligible error messages. (Adapted from the [Haskell Hedgehog homepage](haskell-hedgehog)) -->

<!-- Confusing, isn't it? -->


## Say random again, say random again, I dare you!
Consider again the example I provided earlier:


```csharp
[Property]
bool products_can_be_persisted(Product product)
{
    _repository.Save(product);

    var found = _repository.LoadById(product.Id);

    return found == product;
}
```

When you run it, FsCheck will generate a comprehensive number of randomly generated instances of `Product`.

So, is FsCheck a library like AutoFixture for removing the need for hard-coded values and making the Arrange phase easir?<br/>
No, it's not.

Let me state this loudly: only at a first glance is Property-based Testing about generating random inputs. PBT is more about you than it is about the test runner.

When the domain expert of an e-commerce company tells you 

```
Food products are restricted from international shipping 
due to regulatory compliance, 
unless there is an active Promotion
```

and then you see an implementation such as:

```csharp
if(product.Type == Food && order.Destination != LocalCountry)
    throw new CannotBeSentException();
```

missing a check on an active Promotion, you sense there is a bug.<br/>
You understand that not because you exercised the code, mentally generating thousands of inputs, but because you are a sentient being and you are able to use logic.

Compared to you, C# is dumb. If only C# could do the same your brains does, using logic like in Prolog, relying on AI or on Automated Theorem Proving like in COQ, it could find counterexamples without wandering around aimlessly.<br/>
It's only incidental that your most beloved programming language is bovine.

Property Testing is the act of writing requirements in their essence. The strategies the library uses to prove you wrong are an internal, incidental implementation detail.


## Going Beyond Fixtures
Wow, if got this far, you must really be motivated. Let's enter the rabbit hole, starting with a definition. I promise we will get to code soon.

Let me do a step back and start from something you already know.

In TDD you often desire to exercise a piece of code with multiple input values, so to cover more than one single uses case.<br/>
Instead of sticking with a single input:

```csharp
[Fact]
void calcutates_the_sum_of_2_numbers()
{
    var sum = add(2, 3);
    
    Assert.Equal(5, sum);
}
```


you rather parametrize the test:

```csharp
[Theory]
[InlineData(   2,  3,    5)]
[InlineData(   2,  0,    2)]
[InlineData(   0,  2,    2)]
[InlineData(   2, -2,    0)]
[InlineData(9999, -2, 9997)]
void calcutates_the_sum_of_2_numbers(int a, int b, int expectedSum)
{
    var sum = add(a, b);
    
    Assert.Equal(expectedSum, sum);
}
```

This is fine, although a bit tedious.<br/>
One problem with xUnit's `InlineData` is that it only works with constant values, which are known at compile time. You can't use the instance of a class:

```csharp
[Theory]
//  This won't work
[InlineData(new Product(name: "Apple",  category: Categories.Fruits,   price: 0.90,  description: "Delicious Fuji apple"))
[InlineData(new Product(name: "'Nduja", category: Categories.Sausages, price: 9.50,  description: "Spicy. Original from Calabria"))
void discountable_products(Product product)
{
    var discountIsApplyed = _catalog.CanBeDiscounted(product);
    
    Assert.True(discountIsApplyed);
}


[InlineData(new Product(name: "Book",   category: Categories.FreeTime, price: 15.40,  description: "The Little Schemer"))
[InlineData(new Product(name: "HHKB2",  category: Categories.Keybords, price: 200.00, description: "The mythical Happy Hacking Keyboard 2"))
void not_discountable_products(Product product)
{
    var discountIsApplyed = _catalog.CanBeDiscounted(product);
    
    Assert.False(discountIsApplyed);
}

```

Sure enough, there are workarounds (see [xUnit Theory: Working With InlineData, MemberData, ClassData][xunit-theory]), but this bears the questions: 

* Are you sure the values of `description` and `name` are relevant for those tests? Are't they just distracting?
* Would it be a good idea to just have random values?
* How many different instances should be created to have a good use-case coverage?
* Are you sure you are not missing any important spot?


In an ideal world, it would be nice if you could write something like:


```csharp
[Property]
void any_product_classified_as_food_is_discountable([Food] Product product)
{
    Assert.True(_catalog.CanBeDiscounted(product));
}
```

Notice the `[Food]` attibute, hypotetically instructing the library what `"product classified as food"` means.<br/>
If we could write that

* the test would become independent from actual unnecessary values;
* explicitly referencing `Food` products, the test would be more expressive than a collection of specific cases; it would capture more directly the business rule `"Food products can be discounted"`
* The library would have the chance to discover that the case:

```csharp
new Product(name: ???, category: Categories.SoftDrinks, price: ??,  description: ???)}
```

fails.<br/>
Indeed, it would be super nice if the library could tell us:

```
I get the general rule. But, hey! I found a counterexample! Here it is:

  new Product(name: ???, category: Categories.SoftDrinks, price: ??,  description: ???)}

Don't even care about `name`, `price` and other fields: the element 
causing the problem is 

  `category = Categories.SoftDrinks.
  
Apparently, the production code is not considering soft drinks as a food. 
Either this this is a bug or your specification is incomplete.
```

Of coures, no Property-based Testing library is *that* smart. But they are not too far. They can really shrink down the counterexample, letting you focus on the minimum relevant values.

Proponents of formal methods sometimes stress the notion of specification above that of implementation. However it is the inconsistencies between these two independent descriptions of the desired behavior that reveal the truth. We discover incomplete understanding in the specs and bugs in the implementation. Programming does not flow in a single direction from specifications to implementation but evolves by cross-checking and updating the two. Property-based testing quickens this evolution. (from [Design and User of QuickCheck][design-and-use-of-quickcheck])


## All right, but the `[Food]` attribute does not exist.
Yes, this is still hypothetical, we don't have any `[Food]` attribute yet.
We'll solve this problem soon.

Let's pause a moment to ruminate an intuition: this approach is likely to lead you to a big paradigm shift. Since the library takes away from you the control over which values to base your tests, this forces you to design your tests in a very different way.

Think to the initial, stupid sum example:

```csharp
[Theory]
[InlineData(   2,  3,    5)]
[InlineData(   2,  0,    2)]
[InlineData(   0,  2,    2)]
[InlineData(   2, -2,    0)]
[InlineData(9999, -2, 9997)]
void calcutates_the_sum_of_2_numbers(int a, int b, int expectedSum)
{
    var sum = add(a, b);
    
    Assert.Equal(expectedSum, sum);
}
```

If you let a library randomly generate the test values:

```csharp
[Property]
void calcutates_the_sum_of_2_numbers(int a, int b)
{
    var sum = add(a, b);
    
    Assert.Equal(???, sum);
}
```

you will have no chance to write the assertion. No chances that the expected value is also randomly generated.<br/>
Neither is using `a + b` in the assertion a good choice:

```csharp
[Property]
void calcutates_the_sum_of_2_numbers(int a, int b)
{
    var sum = add(a, b);
    
    Assert.Equal(a + b, sum);
}
```

Indeed, this mirrors the implementation, which completely defies the idea of testing.<br/>
You are forced to think of some other *property* which holds whatever the input. For example:

```csharp
[Property]
void sum_is_commutative(int a, int b)
{
    Assert.Equal(add(a, b), add(b, a));
}

[Property]
void adding_zero_does_no_change_the_result(int a)
{
    Assert.True(a, add(a, 0))
}
```

I chose the silly sum example because it is the basis of the epic video [The lazy programmer's guide to writing thousands of tests][lazy-programmer] by Scott Wlashlin. It's a joy to watch, trust me.

As funny the sum example is, it is pointless for the real world cases.
In more complex cases, you would have properties such as:

```csharp
[Property]
void account_name_is_unique(
    [AllDifferent] Account[] existingAccounts, 
    [FormWithDuplicatedAccount] RegistrationForm form)
{
    var validationResult = _register(form);
    
    Assert.Equal(Error("Account already exists"), validationResult);

}
```

or

```csharp
[Property]
void no_discounts_is_applied_to_carts_without_food([CartContainingNoFoodProducts] List<Product> products)
{
    var plainSumOfPrices = products.Sum(p => p.Price);
    _cart.Add(products)
    
    var total = _cart.Checkout();
    
    Assert.Equal(plainSumOfPrices, total)
}
```

I hope you get how paramount the generation of values is, in PBT.

### Fine. But enough with fictional attributes, please
By now you should have built the intuition that just generating purely random values does not work. We need to craft *quasi-random* values, strictly satisfying some specific domain rules. Indeed, we need a way to instruct the test data generator which rules to stick to.

And it's time to say goodbye to our fictional attributes.<br/>
What about using functions instead?

```csharp
[Property]
void account_name_is_unique()
{
    Account[] existingAccounts = GenerateAllDifferent();
    RegistrationForm form = GenerateWithADuplicateFrom(existingAccounts);

    _application.Accounts = accounts;

    var validationResult = _register(form);
    
    Assert.Equal(Error("Account already exists"), validationResult);
}
```

A bit better. But it's a poor man's solution, and we can surely do much better.<br/>
I see the following traits:

* It's still unclear what's inside those functions. So far, we just moved the problem one level up.

* The test above only generates 1 set of random values. We are back to an example-based test. Ideally, we would like to generate thousands. Something like:

```csharp

record Input(Account[] ExistingAccounts, RegistrationForm form)

[Property]
void account_name_is_unique()
{
    Input[] inputs = Generate(10_000);

    inputs.ForEach(input =>
        _application.Accounts = input.accounts;

        var validationResult = _register(input.Accounts, input.Form);
    
        Assert.Equal(Error("duplicated"), validationResult);
    )
}
```

Notice how we needed a bit of boilerplate code to wrap the test inside a cycle.


* It might not be immediately apparent, but they way random values are generated is not very reusable.
If only the structures used by our homegrown generators were composable, you could have a second test with something like:

```csharp
    Account[] existingAccountsIncludingDisabledOnes = 
        GenerateAllDifferent()
            .ComposedWith(HavingAtLeast3DisabledAccounts());
```

It's very unlikley that such a generic `ComposedWith` method could be defined.

The problem is that our generator functions immediately return values. Once we have values, it's too late to modify the rules for generating further ones.<br/>
If instead it returned *structures able to eventually emit values*, such as wrappers of functions, you would still be in time to alter the domain rules before finally generating values.<br/>

Ideally, structures with solid compositional capabilities, such as monads.

Now probably, I just lost half of my readers.


### Test Data Generators
Great, still here, you brave! Let's see how deep is this rabbit hole, then.

The canonical answer in the Property Testing world is to use Generators. You can think of Generators as a code-based recipe for generating random data accordingly to some custom rules. So, not a trivial random value generator at all. It is a much more advanced structure, able to support you with challenges like:

* generate odd numbers, starting from small ones, and exponentially increasing them, up to the maximum value `N`
* generate instances of `Product`, 30% with prices between `10` and `100`, the rest with lower prices
* generate lists of `Product`s, without duplications, ordered by `description`, maximum 20 items
* generate couples of `Product`s, whose price difference is between `10` and `20`, with same `description` but different `category`, picked from the options `Book, Other, Laptop` only.
* generate a cart, containing up to `10` `Product`s, without exceeding the total of `100 EUR`.

There is virtually no limit to the complexity you can cover. We clearly need a language to express those domain rules.

Prior randomized testing tools required learning a special language and grammar to program the generation of complex test cases. QuickCheck was the first library providing an embedded Domain Specific Language (heavily based on Haskell's amazing type system), in the very same language tests are written in, for writing the test data generation specifications. 

As everything in Functional Programming, the secret is to start simple

* a structure able to generate random booleans: `Gen<int>`
* another able to generate random characters: `Gen<char>`

and then to build on top of composable abstractions:

* function able to generate a string can be built as a composition of char generators: `Gen.list Gen<int> -> Gen<int list>`
* a food `Product` generator can be built as an algebraic composition of other smaller generators:

xxx

```fsharp
```


Let's have a look to some real examples. Please, suspend for a while the judgment on syntax.

This (in Haskell) generates a random boolean values, with equal probability:

```haskell
oneof [return True, return False]
```

This (again in Haskell) generates random boleans weighting the probability of choosing each alternative by some factors:

```haskell
frequency [(2,return True), (1,return False)]
```

Here's an F# example for emitting lists with two elements (bewteen `1` and `100`), with the restriction that the two elements are different:

```fsharp
Gen.choose (1, 100)
|> Gen.two
|> Gen.filter (fun (x, y) -> x <> y)
|> Gen.map (fun (x, y) -> [x; y])
```

The following in F# generates users whose `FirstName` is one of `"Don"`, `"Henrik"` or `null`, a `LastName` with one of `"Syme"` and `"Feldt"` (but never `null`), and an `id` between `0` and `1000`:

```fsharp
type User = {
    Id : int
    FirstName : string
    LastName : string
}

type UserGen() =
   static member User() : Arbitrary<User> =
       let genFirsName = Gen.elements ["Don"; "Henrik"; null]
       let genLastName = Gen.elements ["Syme"; "Feldt"]
       let createUser id firstName lastName =
           {Id = id; FirstName = firstName ; LastName = lastName}
       let getId = Gen.choose(0, 1000)
       let genUser =
           createUser <!> getId <*> genFirsName <*> genLastName
       genUser |> Arb.fromGen
```

Finally, this one in Haskell generates random images like:

<img src="static/img/property-testing/quickcheck-generated-image.png"/>

```haskell
frequency [(2,return True), (1,return False)]

instance Arbitrary PixelRGB8 where
  arbitrary = PixelRGB8 <$> arbitrary <*> arbitrary <*> arbitrary

genImage :: Gen (Image PixelRGB8)
genImage = do
  f <- arbitrary
  (x, y) <- arbitrary `suchThat` ( \(x,y) -> x > 0 && y > 0 )
  return $ generateImage f x y
```


I don't expect you to understand the code above yet.<br/>
Just focus on the key messages:

* Generators are composable structures. Each language would use its own tricks: in C# they are classes.
* They are natively written in your preferred language. No extra languages to learn.
* They are compositional in nature. Combining Generators gives you another Generator. It's Generators all the way down.
* Once you understand the mechanic behind composing them, you've broken every limit. Composing stuff requires a bit of Functional Programming. This is were the fun starts.

Oh, wait: I forgot to mention that Properties too are made of composable structures.

So, in a sense, Property-based Testing is about decomposing your domain problem-space into small properties and generation rules, and then about describing the business functionalities as a composition of those building blocks, for an automated library to try to prove you wrong.




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

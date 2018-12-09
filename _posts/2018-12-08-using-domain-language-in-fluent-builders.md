---
layout: post
title: Using domain language in C# Fluent Builders
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>, <a href="https://github.com/ale7canna">Alessandro Canicatti</a>, <a href="https://github.com/staffoleo">Leonardo Staffolani</a>
tags:
- C#
---
So, by [using lambdas in the Fluent Builders](fluent-builder.html) our tests started containing expressions such as:

{% highlight csharp %}
    var order = AnOrder()
        .WithNumber(10)
        .WithDate(new DateTime(2018, 12, 24))
        .HavingArticle(a => a
            .WithPrice(32.50m)
            .WithCategory("books"))
        .HavingCustomer(c => c
            .WithName("Amelia"))
        .Build();

{% endhighlight %}

We found that this improved the readibility of tests, making them a bit more intelligible by non-technical people.

While the goal was not to implement BDD tests -- which are more conveniently written with specialized libraries -- we felt that the tests could benefit from having an even more fluent syntax, possibly using language closer to domain than to the technical implementation.
<!--more-->
# Index

* [Use of factory methods](#use-of-factory-methods)
* [Name methods after the domain rather than after entity fields](#name-methods-after-the-domain-rather-than-after-entity-fields)
* [Methods can calculate the fields values](#methods-can-calculate-the-fields-values)
* [A method can change more than one value](#methods-can-change-more-than-one-value)
* [Hide the implementation using default values](#hide-the-implementation-using-default-values)
* [Pre-fill values in the main constructor](#pre-fill-values-in-the-main-constructor)
* [Use dedicated Factory Methods to pre-fill values](#use-dedicated-factory-methods-to-pre-fill-values)
* [Use dedicated build methods to fill default values](#use-dedicated-build-methods-to-fill-default-values)
* [When is it too much?](#when-is-it-too-much)

We found a confirmation in Martin Fowler's [FluentInterface](https://www.martinfowler.com/bliki/FluentInterface.html) post:

> Probably the most important thing to notice about this style is that the intent is to do something along the lines of an internal [DomainSpecificLanguage](https://www.martinfowler.com/bliki/DomainSpecificLanguage.html). Indeed this is why we chose the term 'fluent' to describe it, in many ways the two terms are synonyms. The API is primarily designed to be readable and to flow. The price of this fluency is more effort, both in thinking and in the API construction itself. The simple API of constructor, setter, and addition methods is much easier to write. Coming up with a nice fluent API requires a good bit of thought.

We tried to go toward the direction of DSL, applying the following approaches.

## Use of factory methods

Why not replacing the `new` statements with Factory Methods? In the simplest cases, the result is just an innoquous, cosmetic change, such as:

{% highlight csharp %}
    var order = AnOrder()
        .WithNumber(10)
        .WithDate(new DateTime(2018, 12, 24))
        .Build();
{% endhighlight %}

The implementation is trivial: create a Factory Method, add a `using static` and make the constructor private:

{% highlight csharp %}
    using static FluentBuilder.OrderBuilder;

    public class OrderBuilder
    {
        public static OrderBuilder AnOrder() =>
            new OrderBuilder();

        private OrderBuilder()
        {
            _articleBuilder = new ArticleBuilder();
        }

        [..]
    }
{% endhighlight %}

In general, the use of Factory Methods offers the opportunity to build pre-filled entitities while still using expressive statements that speak the domain language. For example, one could feel the need to define `AnInvalidOrder()`, `AnAlreadyProcessedOrder()` or the like (see [Use a dedicated Factory Method to pre-fill values](#use-a-dedicated-factory-method-to-pre-fill-values)).

## Name methods after the domain rather than after entity fields
Methods need not to be be named after the entity's fields. In our example, it may have sense to prefer the names on the right:


|  Previous name        |  New name                 |
|:--------------------: |:-------------------------:|
| `WithDate()`          | `CreatedOn()`           |
| `HavingCustomer()`    | `CreatedBy()`             |
| `HavingArticle()`     | `ContainingAnArticle()`   |
| `WitName()`           | `Named()`                 |
| `WithCategory()`      | `InTheCategory()`         |

<br />
These changes can be safely performed with no other side effects, and to the benefit of the overall readibility:

{% highlight csharp %}
    var order = AnOrder()
        .WithNumber(10)
        .CreatedOn(new DateTime(2018, 12, 24))
        .ByACustomer(c => c
            .Named("Amelia"))
        .ContainingAnArticle(a => a
            .WithPrice(32.50m)
            .InTheCategory("books"))
        .Build();
{% endhighlight %}

## Methods can calculate the fields values

A builder method can be decoupled not only from the underlying field name, but also from its value or its format.

Say for example that the customer age is relevant for a test, and that `Customer` entity has a field to store the birth date:

{% highlight csharp %}
    public class Customer
    {
        public string Name { get; set; }
        public DateTime BirthDay { get; set; }
    }
{% endhighlight %}

It is trivial to add a `WithBirthday()` -- or a `BornOn()` -- method in `CustomerBuilder`:

{% highlight csharp %}
    public class CustomerBuilder
    {
        private string _name;
        private DateTime _birthday;

        public CustomerBuilder BornOn(DateTime dateTime)
        {
            _birthday = dateTime;
            return this;
        }

        public Customer Build() => 
            new Customer
            {
                Name = _name,
                BirthDay = _birthday
            };
    }
{% endhighlight %}

The entity would be built with:

{% highlight csharp %}
    var order = AnOrder()
        .ByACustomer(c => c
            .Named("Amelia")
            .BornOn(new DateTime(1973, 10, 01))
        .Build();
{% endhighlight %}

But maybe the test is focused on the age of customers, rather than on their birtday, for example to test the different paths taken whether the customer is an adult or not. In this case, it might have sense to replace `BornOn()` with `Aged()`, and write test like:

{% highlight csharp %}
    var order = AnOrder()
        .ByACustomer(c => c
            .Named("Amelia")
            .Aged(17))
        .Build();
{% endhighlight %}

`Aged()` could be easily implemented with something like:

{% highlight csharp %}
    public class CustomerBuilder
    {
        private string _name;
        private DateTime _birthday;

        public CustomerBuilder Aged(int age)
        {
            _birthday = DateTime.Now.AddYears(-age);
            return this;
        }
        public Customer Build() => 
            new Customer
            {
                Name = _name,
                BirthDay = _birthday
            };
    }
{% endhighlight %}

I would be very cautious in putting too much logic in the builder. Builders are supposed to be syntactic sugar around constructors and setters, and they should not contain too much magic.

## Methods can change more than one value
Maybe a domain concept affects more than one value. Say for example that the order comprises 3 fields: one to mark it as a gift, one for including the invoce in the box and a last one to print the sender name. 

It might have sense to define methods such as `AsAGift()` that affects both the values, if it helps the test to be more concise and expressive:

{% highlight csharp %}
    public class CustomerBuilder
    {
        private bool _gift;
        private bool _includeInvoice;
        private bool _printSenderName;_

        public CustomerBuilder AsAGift()
        {
            _gift = true;
            _includeInvoice = false;
            _printSenderName = true;
            
            return this;
        }
    }
{% endhighlight %}

The same considerations of the previous paragraph stand: I would not push this too far. The builder shouldn't be to opaque, and most often than not, explicit is better than implicit. Concisiness is not always a benefit for tests readibility.

## Hide the implementation using default values
From time to time we found that some values were needed by the implementation, but they were in fact not relevant at all to describe the test case. The rule of thumb is to reference only the values that are useful to describe the test case, and to set all the irrilevant (but needed) fields to some default values.

For example, if the test is about books, it would be nicer to have a code like:

{% highlight csharp %}
    var order = AnOrder()
        .ContainingAnArticle(a => a
            .InTheCategory("books"))
        .Build();
{% endhighlight %}

rather than:

{% highlight csharp %}
    var order = AnOrder()
        .WithNumber(10)
        .CreatedOn(new DateTime(2018, 12, 24))
        .ByACustomer(c => c
            .Named("Amelia"))
        .ContainingAnArticle(a => a
            .WithPrice(32.50m)
            .InTheCategory("books"))
        .Build();
{% endhighlight %}

In the former case, when the entity is built a lot of information, completely irrilevant to the specific test case, is mentioned. The resulting test is harder to understand and in general less expressive.

Yet, this information is needed, so the builder cannot refrain from filling all the needed entity's fields.

We found 3 options:

#### Pre-fill values in the main constructor
The idea is: when the Builder is invoked, it puts the entity in a known and valid state. For example:

{% highlight csharp %}
    var order = AnOrder()
        .Build();
{% endhighlight %}

would create an order with some default date, with an article having some default price, a customer with a default name and so on. The subsequent methods can modify the values at need. If a test needs to exercise some code relative to the creation date, it would do well to mention the date as following:

{% highlight csharp %}
    var order = AnOrder()
        .CreatedOn(new DateTime(2017, 11, 10))
        .Build();
        
    var result = _sut.CloseIfOlderThanOneMonth(order);
    
    [...]_
{% endhighlight %}

If another test focuses on the article, it can just omit the information about the creation date, relying on the fact that Builder will create a valid order anyway.

To implement this, just add the default value in the Factory Method:

{% highlight csharp %}
    public static OrderBuilder AnOrder() =>
        new OrderBuilder()
        {
            _date = new DateTime(2018, 10, 10),
            _number = 42
        };
{% endhighlight %}

Of course, the Factory Method can itself use the fluent interface:

{% highlight csharp %}
    public static OrderBuilder AnOrder() =>
        new OrderBuilder()
            .WithNumber(42)
            .CreatedOn(new DateTime(2018, 10, 10))
{% endhighlight %}

#### Use dedicated Factory Methods to pre-fill values
The previous trick can hide too much information and make the test too opaque. It can make sense to have more than one Factory Method, with expressive names that clearly conveys their intent, such as `AnEmptyOrder()` or `AnOrderWithOneArticle()`, so tests can focus only on the relevant information:

{% highlight csharp %}
    [Fact]
    public void an_order_cannot_contain_no_articles()
    {
        var order = AnEmptyOrder()
            .Build();

        var result = _sut.Check(order);
        
        result.Should().Be("Order must reference at least one article")
    }
    
    [Fact]
    public void orders_created_on_XMas_have_precedence()
    {
        var order1 = AnOrderWithOneArticle()
            .CreatedOn(new DateTime(2018, 12, 24))
            .Build();
        
        var order2 = AnOrderWithOneArticle()
            .CreatedOn(new DateTime(2018, 12, 23))
            .Build();
        
        var orders = new List<Order> { order1, order2 };
            
        var result = _sut.Sort(orders);
        
        [...]
        
        result.Should().Be("Orders cannot be created on Christmas");
    }

{% endhighlight %}

Again, the implementation is pretty simple:

{% highlight csharp %}
    public static OrderBuilder AnEmptyOrder() =>
        new OrderBuilder();

    public static OrderBuilder AnOrderWithOneArticle() => 
        AnEmptyOrder()
            .WithNumber(100)
            .CreatedOn(new DateTime(2018, 10, 10))
            .ContainingAnArticle(a => a
                .InCategory("some category")
                .WithPrice(50.50m));
{% endhighlight %}

In other words, it could make sense to move into the builder itself some of the expressions that happen to be repeated in the tests and that add no valuable information to the specific test cases.

We found thou that abusing this approach can make the tests a bit opaque, as the Factory Methods hide too much information.

#### Use dedicated build methods to fill default values
As an alternative, we found it convenient to have methods such as `WithOneArticle()` that fill the entity withoot requiring the programmer to specify values (non relevant for the test case), without making the test too implicit:

{% highlight csharp %}
    var order = AnOrder()
        .CreatedOn(new DateTime(2017, 11, 10))
        .WithOneArticle()
        .Build();
{% endhighlight %}

# When is it too much?
Adopting this approach we found that sometimes the complexity introduced by the builders can overtake the benefits of readibility and domain-focused tests. This can happen especially if the builders exceed with magical methods, if they have too implicit behaviours and if they hide too much information. We also found that the biggest risky methods are the one that tend to have overlapping side effects, and that can become hard to combine in a single expression.

For this reason, we try to keep the builders as simple as possible (but not simpler). In other words, we know these techniques are tools at our disposal, but we apply them conscientiously. We mostly focus on tests, not on builders: builders are meant to ease the test writing, and they are supposed to be reused in a very high number of tests, without modifications. When we feel the need to enhance or modify a builder, it happen that the modification causes controversies and discussions. In all the cases, we tend to prefer simplicity to completeness.

A good rule of thumb is the one suggested by Leonardo:

> When you feel that the builder code would benefit from being test covered, it's the sign it has gone too far.

Find here [the complete example](https://github.com/arialdomartini/fluent-builder).

ciao!

---
layout: post
title: A Fluent Builder in C#
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>, <a href="https://github.com/ale7canna">Alessandro Canicatti</a>, <a href="https://github.com/staffoleo">Leonardo Staffolani</a>
tags:
- C#
---
We show how we write our domain-focused, fluent builders that support nested entities by using lambdas, in C#.
<!--more-->

* [Builders with Fluent Interface](#builders-with-fluent-interface)
* [Nested Entities](#nested-entities)
* [A single, flat builder](#a-single-flat-builder)
* [Two separate builders](#two-separate-builders)
* [Lambdas](#lambdas)

## Builders with Fluent Interface
In our integration tests we make heavy use of builders designed with [Fluent Interface](https://www.martinfowler.com/bliki/FluentInterface.html).

Say we have a class `Order` with some properties:

{% highlight csharp %}
public class Order
{
    public int Number { get; set; }
    public DateTime Date { get; set; }
}
{% endhighlight %}

A builder with a Fluent Interface lets create instances of `Order` with a syntax like:

{% highlight csharp %}
var order = new OrderBuilder()
    .WithNumber(10)
    .WithDate(new DateTime(2018, 12, 24))
    .Build();
}
{% endhighlight %}

The builder's code is something like:

{% highlight csharp %}
public class OrderBuilder
{
    private int _number;
    private DateTime _date;

    public static OrderBuilder AnOrder() => new OrderBuilder();

    public OrderBuilder WithNumber(int i)
    {
        _number = i;
        return this;
    }

    public OrderBuilder WithDate(DateTime dateTime)
    {
        _date = dateTime;
        return this;
    }
    
    public Order Build() =>
        new Order
        {
            Number = _number,
            Date = _date
        };
}
{% endhighlight %}

A bit verbose and not so convenient, at least if compared with an ordinary Object Initializer. An Object Initializer gets to the same result with way less code:

{% highlight csharp %}
var order = new Order
{
    Number = 10,
    Date = new DateTime(2018, 12, 24)
}
{% endhighlight %}

## Nested entities

Things get more interesting when the entities have other nested entities. Say for example `Order` has a property `Article`, a separate entity which in turn has got a couple properties:

{% highlight csharp %}
public class Order
{
    public int Number { get; set; }
    public DateTime Date { get; set; }
    public Article Article { get; set; }
}

public class Article
{
    public decimal Price { get; set; }
    public string Category { get; set; }
}
{% endhighlight %}

There are a few options for a Fluent Interface with nested entities. We experimented with 3 of them.

### A single, flat builder

The simplest approach requires the `OrderBuilder` to have a method for each of the `Article`'s properties:

{% highlight csharp %}
var order = new OrderBuilder()
    .WithNumber(10)
    .WithDate(new DateTime(2018, 12, 24))
    .WithArticlePrice(32.50m)
    .WithArticleCategory("books")
    .Build();
}
{% endhighlight %}

Sincerely, this is not ideal, as it pollutes the `OrderBuilder` with methods, making the reuse of the code for building an `Article` nearly impossible. 

### Two separate builders

A better option is to provide `Article` with its own builder, and to make it accessible from `OrderBuilder` with something like:

{% highlight csharp %}
var order = new OrderBuilder()
    .WithNumber(10)
    .WithDate(new DateTime(2018, 12, 24))
    .WithArticle()
        .WithPrice(32.50m)
        .WithArticleCategory("books")
    .End()
    .Build();
}
{% endhighlight %}

With this approach `WithArticle()` is supposed to return an `ArticleBuilder`, while `End()` would return the original `OrderBuilder`. In other words, `WithArticle()` and `End()` respectively begins and ends a nested scope.

This is already better than the previous attempt, but it is not yet ideal, because the implementation gets easily complicated.<br/>
For example, `End()` must return an instance of `OrderBuilder`, but it is not at all obvious how the `ArticleBuilder` instance would keep memory of the `OrderBuilder` instance . Probably, it has to receive it with the `WithArticle()`. Nothing impossible, but it makes the code a bit convoluted.

Also, the scope, highlighted with the indentation

{% highlight csharp %}
    .WithArticle()
        .WithPrice(32.50m)
        .WithArticleCategory("books")
    .End()
{% endhighlight %}

is artificial: it will probably be removed by the IDE with operations like "Reformat code" or the like. Not the best result.

### Lambdas

We found a third approach very convenient:

{% highlight csharp %}
var order = new OrderBuilder()
    .WithNumber(10)
    .WithDate(new DateTime(2018, 12, 24))
    .HavingArticle(a => a
        .WithPrice(32.50m)
        .WithCategory("books"))
    .Build();
{% endhighlight %}

The method `HavingArticle()` takes a lambda operating on the `ArticleBuilder`. It's the lambda itself that defines the nested scope. The method `HavingArticle` returns the instance of `OrderBuilder`, just like all the other methods, making it trivially simple to continue with the fluent syntax. The scope is clearly defined, and the indentation is natively managed by the editor. 

Also the implementation is nothing hard:

{% highlight csharp %}
public class OrderBuilder
{
    private readonly ArticleBuilder _articleBuilder = new ArticleBuilder();

    public OrderBuilder HavingArticle(Func<ArticleBuilder, ArticleBuilder> func)
    {
        func(_articleBuilder);
        return this;
    }
    
    [...]
}
{% endhighlight %}

When `OrderBuilder` is asked to the build the `Order` instance, it delegates the construction of the nested entity `Article` to `ArticleBuilder`:

{% highlight csharp %}
    public class OrderBuilder
    {
        [...]

        private readonly ArticleBuilder _articleBuilder;

        public Order Build() =>
            new Order
            {
                Number = _number,
                Date = _date,
                Article = _articleBuilder.Build()
            };
    }
{% endhighlight %}

The `ArticleBuilder` would be nothing special but an ordinary builder:

{% highlight csharp %}
public class ArticleBuilder
    {
        public ArticleBuilder WithPrice(decimal price)
        {
            _price = price;
            return this;
        }

        private decimal _price;
        private string _category;

        public Article Build()
        {
            return new Article
            {
                Price = _price,
                Category = _category
            };
        }

        public ArticleBuilder WithCategory(string category)
        {
            _category = category;
            return this;
        }
    }
{% endhighlight %}

As seen, `ArticleBuilder` is just another ordinary builder, with a Fluent Interface just like the `OrderBuilder`. What's interesting is that `ArticleBuilder` can have another nested builder, enabling the possibility for even deeper nested scopes.

We decided to name the methods that introduce a nested scope with the prefix `Having`, rather than with the prefix `With`, to help the developer identify the nested entities, but of course this just a matter of tastes.

Find here [the complete example](https://github.com/arialdomartini/fluent-builder/blob/ed4333132e3da0afa4b63992af830a22219155fe/FluentBuilder/Order.cs).


# Builders that use the domain language
Over time, using this style of fluent builders, we realized that it would be useful to adapt the resulting syntax to the domain language. That's the topic of the [second part](using-domain-language-in-fluent-builders.html) of this post.

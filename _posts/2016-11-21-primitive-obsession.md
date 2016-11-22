---
layout: post
title: "Autofac And Primitive Obsession: How I Learned To Love The Injection Of Configuration Parameters"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>, <a href="https://github.com/lucax88x/">Luca Trazzi</a>
tags:
- C#
- IoC
- AutoFac
---
Registering components in Autofac is straightforward, as long as no primitive dependencies (such as connection strings, URLs and configuration parameters in general) are involved. This post describes the strategy for dealing with primitive dependencies.

* [The Ordinary Case](#the-ordinary-case)  
* [Here Come The Primitives](#here-come-the-primitives)
* [Pain Points](#pain-points)
* [Service Locator Is The Wrong Solution](#service-locator-is-the-wrong-solution)
* [Overcoming The Primitive Obsession](#overcoming-the-primitive-obsession)
* [Value Objects In Action](#value-objects-in-action)
* [`implicit` and `explicit` To The Rescue](#implicit-and-explicit-cast-operators-to-the-rescue)
* [Putting It All Together](#putting-it-all-together)
* [Conclusion](#conclusion)

## The Ordinary Case

Say you have a class `Foo` depending on `Bar`: 

{% highlight csharp %}
class Foo
{
    public Foo(Bar bar) { }
}

class Bar {}
{% endhighlight %}

Registering both of them
with Autofac is just simple as writing:

{% highlight csharp %}
var builder = new ContainerBuilder();
builder.RegisterType<Foo>();
builder.RegisterType<Bar>();
{% endhighlight %}

This is enough for Autofac: since it knows how to create instances of `Bar` (it's just a matter of invoking its default constructor), it also knows how to build instances of `Foo`.

This works with no additional configurations even if dependencies are reversed (e.g `Bar` depends on `Foo`) or if relationships are implicit, for example when `Foo` depends on `Func<Bar>`, or on `List<Bar>` and the like: Autofac [is smart enough](http://docs.autofac.org/en/latest/resolve/relationships.html) to build an instance of the right class and inject it into the right component.

## Here Come The Primitives

Troubles come when one of the dependencies is a primitive. Suppose that `Foo` also depends on a connection string, which you decided to represent as a `string`:

{% highlight csharp %}
class Foo
{
    public Foo(Bar bar, string connectionString) { }
}
{% endhighlight %}

You are offered a bunch of native Autofac facilities for registering this class.<br />
Either you can use a lambda:

{% highlight csharp %}
builder.Register(c =>
{
    string connectionString = "someConnectionString";
    var bar = c.Resolve<Bar>();
    return new Foo(bar, connectionString);
});
{% endhighlight %}

or you can continue using the ordinary `RegisterType<>()`, enhanced with the `withParameter()` facility:

{% highlight csharp %}
builder.RegisterType<Foo>()
    .WithParameter("connectionString", "someConnectionString");
{% endhighlight %}

## Pain Points

This is tollerable as long as there there are just a very little number of primitive dependencies.<br />
It starts stinking when it ends up with code like the following:


{% highlight csharp %}
builder.Register(c =>
{
    string connectionString = "someConnectionString";
    string url = "http://some.url";
    string maxUsers = 19;
    
    var bar = c.Resolve<Bar>();
    var baz = c.Resolve<Baz>()
    return new Foo(bar, baz, connectionString, url, maxUsers);
});
{% endhighlight %}

or

{% highlight csharp %}
builder.RegisterType<Foo>()
	.WithParameter("connectionString", "someConnectionString")
	.WithParameter("url", "http://some.url")
	.WithParameter("maxUsers", 19);
{% endhighlight %}

Not only is it verbose and repetitive, but the resulting code is also affected by a couple of problems:

* both the URL and the connection string are represented with the same class (a string), giving no chances to the compiler to know which is which; consequently, it is very possible to switch their values without giving the receiving class the opportunity to detect the issue but at runtime.<br />
Would you spot the problem in the following code?

{% highlight csharp %}
class Foo
{
    public Foo(Bar bar, Baz baz, string connectionString, string url, int maxUsers) { }
}

builder.Register(c =>
{
    var bar = c.Resolve<Bar>();
    var baz = c.Resolve<Baz>()
    return new Foo(bar, baz, "http://some.url", "someConnectionString", 19);
});
{% endhighlight %}

The compiler wouldn't, and that's a pity;

* `withParameter` references parameters by name, as a string, so simple refactoring tasks such as renaming variables become very fragile.<br />
The following code compiles, but it will throw an exception at runtime the moment you will try to resolve `Foo`:


{% highlight csharp %}
class Foo
{
    public Foo(Bar bar, Baz baz, string connectionString, string url, int maxUsers) { }
}

builder.RegisterType<Foo>()
	.WithParameter("connectionstring", "someConnectionString")
	.WithParameter("url", "http://some.url")
	.WithParameter("maxUsers", 19);
{% endhighlight %}

Yes, it's just a matter of a capitalized `S`. Hard to spot, isn't it?

The bad habit of using primitive types to represent domain ideas is a smell called [Primitive Obsession](https://sourcemaking.com/refactoring/smells/primitive-obsession).<br />
Let's see how to avoid it without endng up with ugly Autofac registration statements.

## Service Locator Is The Wrong Solution
Why do you need configuration parameters, in the first place? Of course because you want the freedom to change them at runtime, presumably by using a configuration file:

{% highlight csharp %}
var connectionString = ConfigurationManager.AppSetting["myConnection"];

builder.Register(c =>
{
    var bar = c.Resolve<Bar>();
    return new Foo(bar, connectionString);
});

{% endhighlight %}

This may suggest you a trick you could be tempted to use: to directly inject `ConfigurationManager.AppSetting` into your classes:

{% highlight csharp %}
class Foo
{
    public Foo(Bar bar, NameValueCollection config)
    {
        var connectionString = config["myConnection"];
    }
}

builder.RegisterType<Foo>();
builder.RegisterInstance(ConfigurationManager.AppSetting).As<NameValueCollection>();
{% endhighlight %}

Voilà. No more primitives!<br />
You could even be inclined to define a custom service for collecting all of your configuration parameters:

{% highlight csharp %}
class MyConfigs
{
    public T Get<T>(string key)
    {
        ...
    }

    public void LoadFromConfigfile()
    {
        ...
    }
}

builder.RegisterType<Foo>();
builder.RegisterInstance(new MyConfig().LoadFromConfigfile());
{% endhighlight %}

so that you can easily inject it, as a glorified configuration parameters repository:

{% highlight csharp %}
class Foo
{
    Foo(Bar bar, MyConfigs conf)
    {
        var maxUsers = conf.Get<int>("maxUsers");
    }
}
{% endhighlight %}

This seems to solve most of the problems related to Primitive Obsession, right?

Well, yes, at least it solves the ugly Autofac registration statements issue. In fact, it introduces some additional problems, possibly worse than the ones it was supposed to solve.

The problem is: that's a Service Locator.<br />
I strongly suggest you to read the seminal Mark Seemann's post [Service Locator Is An Anti-Pattern](http://blog.ploeh.dk/2010/02/03/ServiceLocatorisanAnti-Pattern/) which collects a lot of strong arguments on why you should avoid using the Service Locator pattern. Basically, the root of Service Locator's evil is that it hides class dependencies, causing run-time errros and maintenance additional burden.

Service Locator pattern is mostly related to services, while here you are dealing with values without behaviour, simple strings and integers; yet Mark Seemann's argument apply: injecting a configuration-parameters locator is an anti-pattern anyway.<br />

**Just don't do it.**

## Overcoming The Primitive Obsession

Let me try to convince you that there is something deeply wrong with injecting a primitive.<br />
Say you have 2 configuration parameters: `maxUsers` and `numerOfItemsPerPage`. They can be defined in 2 completely different contexts, represent 2 completely different ideas, and have nothing to share.<br />
Yet you can represent both of them with the same type, `int`.

That's the root error: when you use the very same class for 2 completely different purposes, it's just like collapsing `CustomerController` and `NHibernateSession` to a single class: by doing so, you would give no chance neither to Autofac nor to the compiler itself to distinguish the former from the latter.

It's easy to see why representing the customer controller and the NHibernate session with 2 dedicated classes is valuable: it isn't hard to see that the idea can be profitably applied to `maxUsers` and `numerOfItemsPerPage` well, and in general to any primitive configuration parameter. It would give you the opportunity the rely on the compiler: instead of having a constructor which takes 3 indistinguishable integers:

{% highlight csharp %}
class Foo
{
    public Foo(int maxDownloads, int itemsPerPage, int numberOfDays) { ... }
}
{% endhighlight %}

you would actually have 3 distinct parameters, each well defined with its specific class, just like all the other domain ideas:

{% highlight csharp %}
class Foo
{
    public Foo(MaxDownloads maxDownloads, ItemsPerPage itemsPerPage, NumberOfDays numberOfDays) { ... }
}
{% endhighlight %}

So, the basic trick for dealing with primitives in Autofac is: **don't use primitives**. Just represent your configuration parameters with non-primitive types.

## Value Objects In Action
Ok. Sounds simple.<br />
So, instead of declaring `maxUsers` and `numerOfItemsPerPage` as `int`, all you have to do is to define 2 separate non-primitive types inheriting from `int`:

{% highlight csharp %}
class MaxUsers : int {}
class NumerOfItemsPerPage : int {}
{% endhighlight %}

Unfortunately, this is not allowed in C#, since primitive types are sealed.<br />

![primitives are sealed classes](static/img/sealedclass.png)

You definitely have to resort on a workaround: use a DTO as a wrapper of the primitive value.<br />

{% highlight csharp %}
class MaxUsers
{
    public int Value { get; }

    public MaxUsers(int value)
    {
        Value = value;
    }
}
{% endhighlight %}

Think about it: isn't it exactly what you are already used to do, when dealing with compound configuration parameters? For example, chances are you had the need to inject into an instance the url, the username and the password for accessing a web service, and you decided to group them into a single DTO:

{% highlight csharp %}
class BarServiceAuthParameters
{
    public string Url { get; }
    public string Username { get; }
    public string AccessToken { get; }

    public BarServiceAuthParameters(string url, string username, string accessToken)
    {
        Url = url;
        Username = username;
        AccessToken = accessToken;
    }
}
{% endhighlight %}
Wasn't it easy to inject, since it's an ordinary class?<br />
The trick is: use the same strategy also when dealing with single primitive values.

In the the very short post [Primitive Obsession](http://wiki.c2.com/?PrimitiveObsession) J.B. Rainsberger claims those kind of [Value Object](http://martinfowler.com/bliki/ValueObject.html)

> [...] become "attractive code", meaning literally a class/module that attracts behavior towards it as new methods/functions. For example, instead of scattering all the parsing/formatting behavior for dates stored as text, introduce a DateFormat class which attracts that behavior as methods called parse() and format(), almost like magic.

So, it's likely that just by introducing a class for representing an URL or a connection string you will end up enhancing it with some formatting or validation logic, which you could not do with a plain, primitive `string`.


Unfortunately, this solution has got it's drawbacks too. Now it's just more difficult to consume the `ConnectionString`. You need to write:

{% highlight csharp %}
connectionString.Value
{% endhighlight %}

instead of

{% highlight csharp %}
connectionString
{% endhighlight %}

since it's not a string anymore.<br />
It's also more difficult to assign it a value. Instead of

{% highlight csharp %}
var connectionString = "foobar";
{% endhighlight %}

you need

{% highlight csharp %}
var connectionString = new ConnectionString("foobar"); 
{% endhighlight %}

That's bad.<br />

## `implicit` and `explicit` Cast Operators To The Rescue

Let's see what you can do in order to make that Value Object resemble a primitive.

It would be nice if it were possible to implicitly cast it from and to `string`.<br />
Actually, that's not too hard to achieve. There is a technique Jimmy Bogard brillantly exposed in his post [Dealing with primitive obsession](https://lostechies.com/jimmybogard/2007/12/03/dealing-with-primitive-obsession) that  makes a smart use of the cast operators `implicit` and `explicit` and allows to make you consume and create your Value Objects as they are primitives.

Go and read the post. You will learn that by defining your Value Object as

{% highlight csharp %}
public class ConnectionString
{
    public string Value { get; private set; }

    public ConnectionString(string value)
    {
        Value = value;
    }

    public static implicit operator string(ConnectionString connectionString)
    {
        return connectionString.Value;
    }

    public static implicit operator ConnectionString(string value)
    {
        return new ConnectionString(value);
    }
}
{% endhighlight %}

you will get the benefit to consuming and creating it as a primitive, as in the following example:

{% highlight csharp %}

class Foo
{
    public ConnectionString Conn;
}


var foo = new Foo();
foo.Conn = "barbaz";        // implicitly converts from string to ConnectionString

string s = foo.Conn;        // implicitly converts from ConnectionString to string 
var z = (string) foo.Conn;  // explicit conversion works as well
{% endhighlight %}

## Putting It All Together

Once you define all your configuration parameters as Value Objects (with the `implicit` trick), you can use them like the following:

{% highlight csharp %}
class Foo
{
    public Foo(Bar bar, URL url, ConnectionString conn) { ... }
}

var builder = new ContainerBuilder();
builder.RegisterType<Bar>();
builder.RegisterType<Foo>();

builder.RegisterInstance((URL) "http://some.url");
builder.RegisterInstance((ConnectionString) "some connection string");
builder.RegisterInstance((MaxdownloadableFiles) 180);

// builder.RegisterInstance("some connection string").As<ConnectionString>(); // this doesn't work


{% endhighlight %}

Luca's GitHub hosta a <a href="https://github.com/lucax88x/PrimitiveObsession/tree/master/src">sample project</a> leveraging this technique.

## Conclusion

* Don't use primitives. Use Value Objects;
* Avoid using the Service Locator pattern: it's bad;
* Define Value Objects wrapping your configurations, using the `implicit` cast operator to make them behave as a primitive;
* Register them with `RegisterInstance()`.

You will get:

* easy and straightforward Autofac register statements, without using lambdas or `withParameter()`;
* static compile time type checking on injected parameters (no more dependencies neither on parameters order nor on parameters names);
* easier refactoring, you can easily rename parameters or change the order of them, without breaking anything;
* all the advantages of using Value Objects, including the possibility to validate the primitive value;
* the above solution can used also for collection of primitives or classes, such as `List<string>`.

(<a href="https://github.com/lucax88x/PrimitiveObsession/blob/master/README.md">Post repository</a> on Luca Trazzi's Github)

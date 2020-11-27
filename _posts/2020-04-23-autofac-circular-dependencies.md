---
layout: post
title: "Constructor/Constructor circular dependencies resolution with Autofac"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- C#
- IoC
- AutoFac
---
This post describes a possible approach for registering components with circular dependencies (e.g. `Foo` needs `Bar`, and `Bar` needs `Foo`) with Autofac, with both components taking their dependencies through their constructor (the so called [Constructor/Constructor Dependencies](https://autofaccn.readthedocs.io/en/latest/advanced/circular-dependencies.htmlhighlight=constructor%20injection#constructor-constructor-dependencies)).

### TL;DR
Use an [Implicit Relationship Type](https://docs.autofac.org/en/latest/resolve/relationships.html) and inject `Lazy<Foo>` instead of `Foo`.
<!--more-->
## The problem
Say we have a class `Foo` depending on `Bar`, and in the meanwhile `Bar` depending on `Foo`. Ideally, we would like to have each component taking its dependencies through the constructor:

``` csharp
class Foo
{
    internal Foo(Bar bar) {}
}

class Bar
{
    internal Bar(Foo foo) {}
}
```
This would create 2 mutual runtime dependencies, and Autofac would detect the problem at runtime throwing a `DependencyResolutionException`, with the message "`Circular component dependency detected`":

``` csharp
[Fact]
void circular_dependencies_are_not_resolved()
{
    var builder = new ContainerBuilder();
    builder.RegisterType<Foo>();
    builder.RegisterType<Bar>();

    using var container = builder.Build();
    using var scope = container.BeginLifetimeScope();

    var invoking = scope.Invoking(s =>
        s.Resolve<Foo>().Should().NotBeNull());

    invoking.Should()
        .Throw<Autofac.Core.DependencyResolutionException>()
        .Which.InnerException.Message.Should().Contain(
            "Circular component dependency detected");
}
```

The Autofac manual offers 2 workarounds (see [Circular Dependencies](https://docs.autofac.org/en/latest/advanced/circular-dependencies.html)), and both rely on [Property Injection](https://autofaccn.readthedocs.io/en/latest/register/prop-method-injection.html): instead of injecting dependencies through constructor, add a settable property and register the component with `PropertiesAutowired`, using the `PropertyWiringOptions.AllowCircularDependencies` option. One component might use Constructor Injection, but not both. The following tests shows both the approaches:

## Property/Property Dependencies
In this use case, both components use Property Injection:

``` csharp
public class PropertyPropertyTest
{
    public class Foo
    {
        public Bar Bar { get; set; }
    }

    public class Bar
    {
        public Foo Foo { get; set; }
    }

    [Fact]
    void circular_dependencies_are_resolved()
    {
        var builder = new ContainerBuilder();
        builder.RegisterType<Foo>()
            .InstancePerLifetimeScope()
            .PropertiesAutowired(PropertyWiringOptions.AllowCircularDependencies);
        builder.RegisterType<Bar>()
            .InstancePerLifetimeScope()
            .PropertiesAutowired(PropertyWiringOptions.AllowCircularDependencies);

        using var container = builder.Build();
        using var scope = container.BeginLifetimeScope();

        var foo = scope.Resolve<Foo>();
        var bar = scope.Resolve<Bar>();

        foo.Should().NotBeNull();
        foo.Bar.Should().Be(bar);
        bar.Foo.Should().Be(foo);
    }
}
```
See [Property/Property Dependencies](https://autofaccn.readthedocs.io/en/latest/advanced/circular-dependencies.htmlhighlight=constructor%20injection#property-property-dependencies) for further details.

## Constructor/Property Dependencies
In this use case, one component uses Property Injection, while the other use Constructor Injection:
```csharp
public class ConstructorPropertyTest
{
    public class Foo
    {
        public Bar Bar { get; }
        public Foo(Bar bar)
        {
            Bar = bar;
        }
    }

    public class Bar
    {
        public Foo Foo { get; set; }
    }

    [Fact]
    void circular_dependencies_are_resolved()
    {
        var builder = new ContainerBuilder();
        builder.RegisterType<Foo>()
            .InstancePerLifetimeScope();
        builder.RegisterType<Bar>()
            .InstancePerLifetimeScope()
            .PropertiesAutowired(PropertyWiringOptions.AllowCircularDependencies);

        using var container = builder.Build();
        using var scope = container.BeginLifetimeScope();

        var foo = scope.Resolve<Foo>();
        var bar = scope.Resolve<Bar>();

        foo.Should().NotBeNull();
        foo.Bar.Should().Be(bar);
        bar.Foo.Should().Be(foo);
    }
}

```

See [Constructor/Property Dependencies](https://autofaccn.readthedocs.io/en/latest/advanced/circular-dependencies.htmlhighlight=constructor%20injection#constructor-property-dependencies) on the Autofac manual.

## Event Handlers
A third approach based on the use of Event Handlers is described in [Property Injection](https://autofaccn.readthedocs.io/en/latest/register/prop-method-injection.html?highlight=property#property-injection):

> To support circular dependencies, use an activated event handler:
>
> `builder.Register(c => new A()).OnActivated(e => e.Instance.B = e.Context.Resolve<B>());`

In this case, the registration does not rely on `PropertiesAutowired`, but it still requires Property Injection.

## Constructor/Constructor Dependencies
In this use case, both components use Constructor Injection.

[The manual states that](https://autofaccn.readthedocs.io/en/latest/advanced/circular-dependencies.htmlhighlight=constructor%20injection#constructor-constructor-dependencies) Constructor/Constructor Dependencies is not directly supported, and it also discourages from using tricky workarounds.

A possible approach that sounds legit is to postpone the resolution of one of the services by using the [Implicit Relationship Type](https://docs.autofac.org/en/latest/resolve/relationships.html) `Lazy`:

```csharp
class Foo
{
    public Foo(Bar bar) { }
}

class Bar
{
    public Bar(Lazy<Foo> foo) { }
}
```

In this case, Autofac would happily resolve both services. When `Foo` is resolved, Autofac would detect the need to resolve `Bar`; luckily, the resolution of `Bar` won't need the creation of an instance of `Foo`, since the dependency is lazy. This allows both the constructors to succeed. `Bar` can eventually get a reference to the `Foo` instance with `foo.Value`.

This works provided that `foo.Value` is not used in `Bar`'s constructor (i.e., before the `Foo`'s constructor has successfully finished its execution), and that the components are not registered as `InstancePerDependency` (a constraint that is shared with the Property/Property and the Constructor/Property workarounds).

The following test shows a complete implementation:

``` csharp
public class ConstructorConstructorTest
{
    public class Foo
    {
        public Bar Bar { get; }
        public Foo(Bar bar)
        {
            Bar = bar;
        }
    }

    public class Bar
    {
        private readonly Lazy<Foo> _foo;
        public Foo Foo => _foo.Value;

        public Bar(Lazy<Foo> foo)
        {
            _foo = foo;
        }
    }

    [Fact]
    void circular_dependencies_are_resolved()
    {
        var builder = new ContainerBuilder();
        builder.RegisterType<Foo>()
            .InstancePerLifetimeScope();
        builder.RegisterType<Bar>()
            .InstancePerLifetimeScope();
        using var container = builder.Build();
        using var scope = container.BeginLifetimeScope();

        var foo = scope.Resolve<Foo>();
        var bar = scope.Resolve<Bar>();

        foo.Should().NotBeNull();
        foo.Bar.Should().Be(bar);
        bar.Foo.Should().Be(foo);
    }
}
```

## Getting rid of circular dependencies
Autofac's support for circular dependencies is somehow limited for a reason: [cyclic dependencies are evil](https://fsharpforfunandprofit.com/posts/cyclic-dependencies/ "Cyclic dependencies are evil") and should be avoided. Some languages (like F#) are very strict on this regard, and they can even require the code components to be defined in dependency order, with the explicit goal to disallow any cyclic dependency.

So, the use of this workaround (provided that is has no bad side effects) should be considered the last resort. I still think that when a circular dependency is found, it should be considered a smell and be refactored and removed.

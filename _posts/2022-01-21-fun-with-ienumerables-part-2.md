---
layout: post
title: "Fun with IEnumerable - Part II - Encapsulating an Action in IEnumerable"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- C#
---

In the [first installment](fun-with-ienumerables-part-1) of this [short series](fun-with-ienumerables) we saw how consuming an `IEnumerable` is more than just *enumerating* a collection: indeed, it implies *executing* some code. And we saw how to (cowardly) write weird code to make our `IEnumerable` behave in funny ways.

If we want to continue posing as professional craftspersons, we need professional patterns. And implementing Dependency Injection via `IEnumerable` sounds a good starting point.

**Warning**: this post is a *divertissement*. By no means I would suggest using this approach in productive software.
<!--more-->
# Consuming an action
Say you have a class with a dependency

```csharp
class MyTargetClass
{
    private readonly ISomeService _dependency;

    internal MyTargetClass(ISomeService dependency) =>
        _dependency = dependency;

    internal void SomeMethod() =>
        _dependency.DoSomething();
}
```

and assume that for the sake of simplicity we can live with a simple `Action`


```csharp
class MyTargetClass
{
    private readonly Action _dependency;

    internal MyTargetClass(Action dependency) =>
        _dependency = dependency;

    internal void SomeMethod() =>
        _dependency.Apply();
}
```

Our goal is to use an `IEnumerable` of whatever type instead of an `Action`. In order to hide this in an `IEnumerable`


```csharp
void FunctionToBeInjected()
{
    Console.WriteLine("It worked");
}
```

we just need

```csharp
IEnumerable<int> Hide()
{
    yield return -1;
    FunctionToBeInjected();
}
```

Let' make it more generic:

```csharp
IEnumerable<int> Hide(Action a)
{
    yield return -1;
    a();
}

IEnumerable<int> dependencyAsIEnumerable = 
    Hide(FunctionToBeInjected);
```

Cool. Now, whoever tries to materialize that collection of numbers will execute `FunctionToBeInjected()`. Inject the `Action` (as an `IEnumerable<int>`) into `MyTargetClass` is straightforward


```csharp
class MyTargetClass
{
    private readonly IEnumerable<int> _dependency;

    internal MyTargetClass(IEnumerable<int> dependency) =>
        _dependency = dependency;

    internal void SomeMethod()
    {
        var _ = _dependency.Count(); // this executes the injected action
    }
}


var target = new MyTargetClass(dependencyAsIEnumerable);
target.SomeMethod();
```

Invoking the action is just a matter of materializing the `IEnumerable`. It will get us back an `int` value, which we will happily ignore.

## Beautification

Such a beautify and solid approach deserves a more idiomatic code.<br/>

This makes it easier to see the `IEnumerable` as something that can be executed 

```csharp
static class EnumerableDependencyInjection
{
    internal static void Execute<T>(this IEnumerable<T> enumerable) =>
        enumerable.Aggregate((_, v) => v);
}

class MyTargetClass
{
    private readonly IEnumerable<int> _dependency;

    internal MyTargetClass(IEnumerable<int> dependency) =>
        _dependency = dependency;

    internal void SomeMethod()
    {
        _dependency.Execute();
    }
}
```

Ah, much better than the ugly `var _ = _dependency.Count();` we had before.<br/>
The code for disguising an `Action`` as a collection could be made more generic

```csharp
class EnumerableDependencyInjectionHelper
{
    internal static IEnumerable<T> DisguiseAsEnumerableOf<T>(Action a)
    {
        IEnumerable<T> arbitraryCode()
        {
            yield return default(T);
            a();
        }

        return arbitraryCode();
    }
}
```

Here how you would use it:


```csharp
private static void InjectMe()
{
    Console.WriteLine("It worked");
}

[Fact]
void dependency_injection_with_IEnumerable()
{
    var writer = new StringWriter();
    Console.SetOut(writer);

    IEnumerable<int> dependency = 
        DisguiseAsEnumerableOf<int>(InjectMe);

    new MyTargetClass(dependency)
        .SomeMethod();

    Assert.Equal($"It worked{NewLine}", writer.GetStringBuilder().ToString());
}
```



# Gimme a Func
Yes, I know: this is already  much better than using a stupid IoC framework.<br/>
The sad part is:

* the client cannot pass any argument to the `Action`

* it's an `Action`, not a `Func`: there's no return value to get back

Our exquisite Dependency Injection can only be used for fire and forget stuff. Can this approach be possibly enhanced? Get prepared for the [last installment](fun-with-ienumerables-part-3), where we implement a real Dependency Injection.

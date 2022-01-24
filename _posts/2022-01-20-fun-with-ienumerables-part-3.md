---
layout: post
title: "Fun with IEnumerable - Part III - Horribly unconvenient Dependency Injection via IEnumerable"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- C#
---

In the [second installment](fun-with-ienumerables-part-2) of this [series](fun-with-ienumerables) we injected into a client an `Action`, which we cleverly disguised as an `IEnumerable` of some arbitrary type.

But that's not enough: we want to send it an argument and to get some results. So, we want a `Func<A,B>` rather than an `Action`. The problem is, disguising a `Func` as an `IEnumerable` is all but easy. Try yourself.

**Warning**: this post is satiric. Should you find this implementation of Dependency Injection worth to be used in production, I recommend you to consult a good doctor.
<!--more-->
# Awful limitations
The action is embedded into the enumerable as

```csharp
IEnumerable<int> EncapsulateIntoAnEnumerable(Action a)
{
    yield return -1;
    a();
}
```

The action is then invoked when the `IEnumerable` is materialized with `Count()`, `ToList()`, a `foreach` or the like.

We have a couple of restrictions here:

* even if the `Action a` had a parameter (such as in `Action<T>`), we would like the client to pass the argument. But that's too late: the compiler would complain that this must be done in `EncapsulateIntoAnEnumerable` already

* although we can easily have a `Func` instead of an `Action`, that would be constrained to return the same type as the encapsulating `IEnumerable`. That's a limitation we could not accept. We want to embed a `Func<A, B>` in an `IEnumerable<C>`, with independent `A`, `B` and `C`.

You can try to play around these horrible limitations, but you will be stuck with the cruel fact that when you enumerate an `IEnumerable<C>` all you can get back is a stupid, irrilevant instance of `C`, while we wanted a `B`, as in `Func<A,B>`. 


## Exceptions to the resque
Which better option than using an `Exception` to control the application flow? It's a splendid  design choice. [Ward Cunningham](https://web.archive.org/web/20140430044213/http://c2.com/cgi-bin/wiki?DontUseExceptionsForFlowControl) would be proud of us.

Here we go!

## Hiding the function

This is how we can hide a `Func<A,B>` inside an Exception inside an `IEnumerable<C>`
`
```csharp
internal class InjectHelper
{
    internal static IEnumerable<C> DisguiseAsIEnumerable<A, B, C>(Func<A, B> toBeInjected)
    {
        IEnumerable<C> Encapsulate()
        {
            yield return default(C);
            throw new EnumerableException<A, B>
            {
                Function = toBeInjected
            };
        }

        return Encapsulate();
    }
}

class EnumerableException<A, B> : Exception
{
    internal Func<A, B> Function { get; init; }
}
```


## Retrieving the Function
And this is how we would retrieve and execute it:

```csharp
internal static class Retrieve
{
    internal static B Execute<A, B, C>(this IEnumerable<C> enumerable, A argument)
    {
        try
        {
            enumerable.Aggregate((_, v) => v);
            throw new Exception("It will never get here");
        }
        catch (EnumerableException<A, B> e)
        {
            return e.Function.Invoke(argument);
        }
    }
}
```

## Usage
An example of usage would be

```csharp


class MyTargetClass
{
    // It's a collection of decimals
    private readonly IEnumerable<decimal> _decimals;

    internal MyTargetClass(IEnumerable<decimal> decimals)
    {
        _decimals = decimals;
    }

    internal int SomeMethod(string s)
    {
        // No, it's a function.
        return _decimals.Execute<string, int, decimal>(s);
    }
}

public class InjectingFunc
{
    int ToBeInjected(string s)
    {
        return s.Length;
    }    

    [Fact]
    public void injecting_func()
    {
        IEnumerable<decimal> decimals =
            DisguiseAsIEnumerable<string, int, decimal>(ToBeInjected);
        var client = new MyTargetClass(decimals);

        var result = client.SomeMethod("foo");

        result.Should().Be(3);
    }

}
```

Isn't it abominable?

Enjoy coding!


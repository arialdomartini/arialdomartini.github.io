---
layout: post
title: "Fun with IEnumerable - A puzzling quiz"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- C#
---
Could you explain the following?

```csharp
[Test]
public void puzzling_test()
{
    IEnumerable<string> strings = SomeStrings;
    Assert.AreEqual(new[] { "Foo", "Bar", "Baz" }, strings.ToArray());


    var test1 = strings.Select(s => new string(s.ToCharArray()));
    Assert.AreEqual(new[] { "Foo", "Bar", "Baz" }, test1.ToArray());


    var test2 = strings.Select(s => "" + new string(s.ToCharArray()));
    Assert.AreEqual(new[] { "Foo", "Bar", }, test2.ToArray());
    // Hey, what happened to Baz??

            
    try
    { 
        var test3 = strings.Select(s => new string(s.ToCharArray()) + "");
        Assert.Null(test3.ToArray());
    }
    catch (PlatformNotSupportedException)
    {
        Assert.Pass();
    }

            
    var test4 = strings.Select(s => new string(s.ToCharArray()) + "");
    Assert.AreEqual(new[]
    {
        "Argument Foo of type System.String is not assignable to parameter type int32",
        "Argument Bar of type System.String is not assignable to parameter type int32",
        "Constructor 'Baz' has 0 parameter(s) but is invoked with 0 argument(s)"
    }, test4.ToArray());
}
```
<!--more-->
I swear that [this test is green](/static/img/fun-with-ienumerables/puzzling-test.png). How can it possibly be?

## Fun with IEnumerables, a series

It's actually a dumb and deceitful trick, which we will use and abuse in a short, 3 episode series.

* [Part I - Funny collection behaviors](fun-with-ienumerables-part-1)
* [Part II - Encapsulating an Action in IEnumerable](fun-with-ienumerables-part-2)
* [Part II - Pointless, unconvenient and unconventional Dependency Injection via IEnumerable](fun-with-ienumerables-part)

  

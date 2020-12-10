---
layout: post
title: "Private fields are not that private, after all"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- C#
---
**TL;DR**: the following code is just legit:

```csharp
class Foo
{
    private string _privateField;
    
    void DoSomething(Foo anotherInstance)
    {
        anotherInstance._privateField = "doh!";
    }
}
```
<!--more-->
Private fields are on types, not on instances. This means that every instance of a class can access any private fields of any other instance of the same class.<br/>
This may come as a surprise.

That's defined in the [C# Language Specification](https://www.ecma-international.org/publications/files/ECMA-ST-ARCH/ECMA-334%201st%20edition%20December%202001.pdf): `private` limits access to the containing type, not to the containing instance.

## Why?
[One of the possible reasons](https://stackoverflow.com/a/6983855/202443) is because otherwise the compiler would have big issues in figuring out if code such this should be considered valid or not:

```csharp
class Foo
{
    private string _privateField;
    
    void DoSomething(Foo justTheSameInstance)
    {
        justTheSameInstance._privateField = "doh!";
    }
    
    void CallWithSelf()
    {
        DoSomething(this);
    }
}
```

One could argue that the code above should be invalid in any case, as the signature of `DoSomething` takes no enforcement at all on which instance it could receive.

[Another argument](https://stackoverflow.com/a/30604512/202443) is that private fields are designed for encapsulation, and for protecting the client of a class from knowing the internals of the implementation

    * users are protected from implementation changes breaking their code
    * designers are protected from having to keep implementation details features unchanged forever

A class needn't be protected from itself and from the changes in its own code. In other words, private fields are meant to protect a class source code from *other* programmers.

[Rephrasing this argument](https://stackoverflow.com/a/6983838/202443), the purpose of the `private` access modifier is to lower the mutual dependence of different modules of code, rather than different objects in memory. 

A last argument could be that access of private fields from other instances makes writing the code for objects equality easier.

Whatever the argument, the visibility of `private` fields seems to violate the Least Astonishment Principle, to the point that the average C# programmer might perceive the `_` right after `.` in `justTheSameInstance._privateField` as a possible syntax error, or an abused naming convention.

## What about other languages?
Java in on the same boat

```java
class Foo {
    private String foo;
    
    void DoSomething(Main anotherInstance){
        anotherInstance.foo = "doh!";
    }
}
```

and so is C++.

But not all languages took the same decision: Scala provides instance private fields.

---
layout: post
title: "Explicit Interface Implementation is a design best practice"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- C#
most_read: true
---
* A C# classe can implement an interface either implicitly or explicitly
* **Implement explicitly wherever possible**: it is a good practice
* In fact, there are rare cases for Implicit Implementation.
* Indeed, objects should be manipulated solely in terms of their interface, not in terms of their concrete types. 

* With Explicit Implementation **methods can be `internal`**, which is good for encapsulation
* Explicit Implementation is a design best practice, as it promotes Loose Coupling and enforces the Dependency Inversion Principle.
* It gets rid of **Ghost Public Methods**, promoting maintenaibility

<!--more-->

## Interfaces can be implemented either implicitly or explicitly
In C#, a class can implement an interface either implicitly or explicitly:

```csharp
interface ISomeService
{
    public void DoSomething();
}

internal class ImplicitImplementation : ISomeService
{
    public void DoSomething() { }
}

internal class ExplicitImplementation : ISomeService
{
    void ISomeService.DoSomething() {  }
}
```

## Disambiguate members with same signature
For [Microsoft C# Progamming Guide][microsoft-programming-guide], Explicit Interface Method Implementation (EIMI) is only useful when  a class implements two interfaces containing members with the same name and signature.<br/>
But that's only half of the story: Explicit Implementation provides way more benefits.

## Inject dependencies in terms of an interface
Fact is: Explicit Implementation and Depencency Injection are a match made in heaven.<br/>

Code using Dependency Injection and interfaces works fine, regardless if the injected services are implemented explicitly or impicitly:

```csharp
class Client
{
    readonly ISomeService _service;
    
    internal Client(ISomeService service)
    {
        _service = service;
    }

    internal void SomeMethod()
    {
        _service.DoSomething();
    }
}
```

Here, rightly, `service` is injected in terms of `ISomeService` and `Client` would never know the corresponding concrete class. And, indeed, it needn't know: it is the core of the [Dependency Inversion Principle][dependency-inversion].

We implement interfaces exactly because we want to separate the contract from the implementation, and decouple the clients from implementation.<br/>

We don't inject `service` as its concrete class, because it would just create unnecessary and noxious coupling. 


## Program to an interface, not an implementation
That's the key: **implementing the interface explicitly forces all the clients to use the interface. Who wants to use the concrete class, will have to downcast**.

Isn't it a limitation? No: it is in fact a feature. Because you will never need to downcast.

**If you find yourself casting down to an implementation, you are doing it wrong.**

Interfaces are created in the first place to promote *Programming To An Interface* and discouraging *Programming To An Implementation*.

The idea behind *Programming To An Interface* is to base the programming logic on the contract, rather than on the internal implementation details. *Programming To The Interface* makes code more reusable.

> There are two benefits to manipulating objects solely in terms of the interface defined by abstract classes:
>
> * Clients remain unaware of the specific types of objects they use, as long as the objects adhere to the interface that clients expect.
> * Clients remain unaware of the classes that implement these objects. Clients only know about the abstract class(es) defining the interface.
> This so greatly reduces implementation dependencies between subsystems that it leads to the following principle of reusable object-oriented design:
> * Program to an interface, not an implementation.
>
> **Don’t declare variables to be instances of particular concrete classes**. Instead, commit only to an interface defined by an abstract class

[Design Pattern: Elements of Reusable Object-Oriented Software][gof]

## The case for Implicit Inferface Implementation 
What if you need to declare variables of particular concrete classes?<br/>
I challenge that this is even needed. In fact, I can't think of a single case for an instance, passed down to a method or to a class constructor in terms of an interface, to be cast down to the concrete type. 

In other words, **there is no single reason to have Implicit Implementation on dependencies and parameters**. 

Explicit Implementation is just as a way to enforce that, and to prevent the consumption of concrete classes. As such, it is a best design practice, as it enforces the Dependency Inversion Principle.

If there's an interface, why ever circumventing it?

## What about tests?
> **TL;DR**<br/>
> Prefer testing to the interface over testing on the implementation.

Tests too should only deal with explicitly implemented method: they should exercise the public interface anyway, as they are meant to test the behaviour of a class just like it is consumed by its clients. 

You don't test private methods for the same reason: private methods are an implementation detail that could change during the refactor cycle, they are private for hiding the implementation, and you don’t want to test implementation details.

**Private methods and methods not part of any interface are alike, and the same rational applies**: tests should always exercise a System Under Test in terms of its interface, rather than of its implementation. Explicit Implementation would just enforce this.

```csharp
public class ExplicitClassTest
{
    readonly ISomeService _sut;  // System Under Test is defined in terms of its contract
    
    public ExplicitClassTest()
    {
        _sut = new ExplicitImplementation();
    }
    
    void test_some_functionality()
    {
        _sut.DoSomething();  // this consumes the interface, so the behavior, rather than the implementation
        ...
    }
}
```


## Implicit Implementation forces using public
On the contrary, Implicit Implementation comes with some drawbacks: it pollutes the code with unneeded `public` modifiers.

To promote encapsulation, a class should use **the lowest access modifier needed**. There's no use of having a public class with public methods, if the class is not meant to be exposed to other projects.

Unfortunately, implicitly implemented methods need to be `public`.

The following code won't compile:

```csharp
interface ISomeService
{
    internal void Foo();
}

class SomeService : ISomeService
{
    internal void Foo() {}  // this needs to be public.
}
```

`Foo()` needs to be `public` regardless of how it was designed to be consumed. That's unfortunate.

Even worse, marking it as `public` in the class is not enough: the compiler would complain that also the upstream method in `ISomeService` needs to be made `public`. So, **the problem would propagate up to the whole interface**.

```csharp
interface ISomeService
{
    public void Foo();
}

class SomeService : ISomeService
{
    public void Foo() {}
}
```

## Explicit Implementation honors the minimum visibility

This does not happen with Explicit Implementation.<br/>
The following code would happily compile:

```csharp
interface ISomeService
{
    internal void Foo();
}

class SomeService : ISomeService
{
    void ISomeService.Foo() { }
}
```


## What about tests, again? How to make internal classes visible to test projects?
Just add the following to the project `.csproj` file:

```xml
<ItemGroup>
    <InternalsVisibleTo Include="YourTestProject" />
</ItemGroup>
```

If you stick to the convention of having test project names ending with `.Test`, you can also use:


```xml
<ItemGroup>
    <InternalsVisibleTo Include="$(MSBuildProjectName).Test" />
</ItemGroup>
```




## Ghost Public Methods
> **TL;DR**<br/>
> Explicit Implementation promotes maintenaibility 

When a method is removed from an interface (e.g. it's moved to another interface during a refactoring), explicit implementation would force the compiler to look all over the code and identify all the places where the method is no longer needed. With Implicit Implementation, a class might end up having am unused, ghost public function.

For example:

```csharp
internal interface ISomeService
{
    public void MethodOne();
    public void MethodTwo();
}

internal class ImplicitImplementation : ISomeService
{
    public void MethodOne() { }
    public void MethodTwo() { }
}

internal class ExplicitImplementation : ISomeService
{
    void ISomeService.MethodOne() {  }
    void ISomeService.MethodTwo() {  }
}
```

If the `MethodTwo()` is removed from the interface:


```csharp
internal interface ISomeService
{
    // void MethodTwo(); // removed
}

internal class ImplicitImplementation : ISomeService
{
    public void MethodOne() { }
    public void MethodTwo() { }  // <- Ghost Public Function
}

internal class ExplicitImplementation : ISomeService
{
    void ISomeService.MethodOne() {  }
    void ISomeService.MethodTwo() {  }  // <- This won't compile
}
```


## The case of F#
F# only supports Explicit Implementation.

That's one of the justifications:

> A well designed object-oriented program will have a strong focus on behavior rather than data, so it will use a lot of
> polymorphism, either using "duck-typing" or explicit interfaces, and will try to avoid having explicit knowledge of the 
> actual concrete classes being passed around.
[What are types for?][fsharp-types] in [F# For Fun and Profit][fsharp-profit]

Seems like the same argument I just described.


## References: 
[dependency-inversion]: https://en.wikipedia.org/wiki/Dependency_inversion_principle
[microsoft-programming-guide]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/interfaces/explicit-interface-implementation
[why-i-use-explicit]: https://www.codeproject.com/Articles/392516/Why-I-use-explicit-interface-implementation-as-a-d
[gof]: https://www.pearson.com/us/higher-education/program/Gamma-Design-Patterns-Elements-of-Reusable-Object-Oriented-Software/PGM14333.html 
[fsharp-types]: https://fsharpforfunandprofit.com/posts/overview-of-types-in-fsharp/
[fsharp-profit]: https://fsharpforfunandprofit.com


* [Explicit Interface Implementation][microsoft-programming-guide], in Microsoft C# Programming Guide
* [Why I use explicit interface implementation as a default implementation technique][why-i-use-explicit]
* [Design Pattern, Elements of Reusable Object-Oriented Software- Addison-Wesley Professional][gof]
* [Overview of types in F#][fsharp-types], in [F# For Fun And Profit][fsharp-profit]
* [Dependency Inversion Principle][dependency-inversion]


[Comments](https://github.com/arialdomartini/arialdomartini.github.io/discussions/5)

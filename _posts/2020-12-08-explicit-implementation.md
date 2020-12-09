---
layout: post
title: "Explicit Interface Method Implementation is a design best practice"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- C#
---
**TL;DR**<br/>
* In C# interfaces can be implemented either implicitly or explicitly
* I've started applying Explicit Implementation wherever possible
* I figured out that there are not so many use cases for Implicit Implementation. After all, objects should be manipulated solely in terms of their interface, rather than in terms of their concrete types. 
* I ended up seeing Explicit Implementation as a design best practice, as it promotes Loose Coupling and enforces the Dependency Inversion Principle.

<!--more-->

## Interfaces can be implemented either implicitly or explicitly
In C#, a class can implement an interface either implicitly or explicitly:

```csharp
internal interface ISomeService
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
As a matter of fact, the [Microsoft C# Progamming Guide][microsoft-programming-guide] suggests the Explicit Interface Method Implementation (EIMI) only as a way to resolve the rare cases where a class implements two interfaces that contain a member with the same name and signature.

## The problem with casting
When consumed in terms of implementation, classes implemented explicitly lead to cumbersome code:

```csharp
internal class Client
{
    internal void SomeMethod()
    {
        var implicit = new ImplicitImplementation();
        implicit.DoSomething(); // this just works
        
        var explicit = new ExplicitImplementation();
        explicit.DoSomething(); // this won't compile
        ((ISomeService)explicit).DoSomething(); // this works, but it's horrible
    }
}
```

Finally, there's no doubt that Implicit Implementation is, by far, the most common style.

## So what's the big deal?
Despite the drawbacks above, I've decided to start using more and more extensively the Explicit Implementation, for the reasons depicted below. So far I found a lot of benefits.<br/>
Bear with me. 

## Inject dependencies in terms of an interface
The fact is, Explicit Interface Method Implementation and Depencency Injection are a match made in heaven.<br/>
If you are used to use Dependency Injection, chances are you would find Explicit Implementation beneficial too.

The following code:

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

works regardless if `service` is implemented explicitly or impicitly. 

That's a trivial implementation of Dependency Injection where, rightly, `service` is injected in terms of an interface. `Client` would never know which is the corresponding concrete class.<br/>
And, indeed, it needn't know: that's a just the application of the Dependency Inversion Principle:

> * High-level modules should not depend on low-level modules. Both should depend on abstractions (e.g., interfaces).
> * Abstractions should not depend on details. Details (concrete implementations) should depend on abstractions.

[Dependency Inversion Principle][dependency-inversion]

Interfaces encourage loose coupling. We implement interfaces exactly because we want to separate the contract from the implementation, and decouple the clients from implementation.<br/>

Injecting `service` in terms of a specific class would just create useless and noxious coupling. 

## Program to an interface, not an implementation
> **TL;DR**<br/>
> If you find yourself casting down to an implementation, you are doing it wrong.

There's a reason why you define intefaces in the first place. Why not consuming them, rather then casting?

I tend to use Explicit Interface Implementation when I want to promote "Programming To An Interface" and discourage "Programming To An Implementation".

The idea behind Programming To An Interface is to base the programming logic on the interfaces of the objects used, rather than on the internal implementation details. Programming to the interface reduces dependency on implementation specifics and makes code more reusable.

> There are two benefits to manipulating objects solely in terms of the interface defined by abstract classes:
> * Clients remain unaware of the specific types of objects they use, as long as the objects adhere to the interface that clients expect.
> * Clients remain unaware of the classes that implement these objects. Clients only know about the abstract class(es) defining the interface.
> This so greatly reduces implementation dependencies between subsystems that it leads to the following principle of reusable object-oriented design:
> * Program to an interface, not an implementation.
> Don’t declare variables to be instances of particular concrete classes. Instead, commit only to an interface defined by an abstract class

[Design Pattern: Elements of Reusable Object-Oriented Software][gof]

I can't think of a single case in which an instance, passed down to a method or to a class constructor, in terms of an interface should be cast down to the concrete type. In other words, I don't see the reason to have Implicit Implementation on dependencies and parameters. 

Explicit Implementation is just as a way to enforce that, and to prevent the consumption of concrete classes. As such, it is a best design practice, as it enforces the Dependency Inversion Principle.

If there's an interface, why ever circumventing it?

## Implicit Implementation forces using public
Also, Implicit Implementation comes with some drawbacks: they pollute the code with unneeded `public` modifiers.

To promote encapsulation, a class should use the lowest access modifier needed. There's no use of having a public class with public methods, if the class is not meant to be exposed to other projects.

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

Even worse, marking it as `public` in the class is not enough: the compiler would complain that also the upstream method in `ISomeService` needs to be made `public`.<br/>
So, the problem would propagate up to the whole interface.

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

This does not happen with Explicit Implementation. The following would happily compile:

```csharp
interface ISomeService
{
    internalvoid Foo();
}

class SomeService : ISomeService
{
    void ISomeService.Foo() { }
}
```

## What about tests?
> **TL;DR**<br/>
> Prefer testing to the interface over testing on the implementation.

Tests too should only deal with explicitly implemented method: tests should exercise the public interface anyway, as they are meant to test the behaviour of a class as it is consumed by its clients. 

You don't test private methods for the same reason: private methods are an implementation detail that could change during the refactor cycle, they are private for hiding the implementation, and you don’t want to test implementation details.

Private methods and methods not part of any interface are alike, and the same rational applies: tests should always exercise a System Under Test in terms of its interface, rather than of its implementation. Explicit Implementation would just enforce this.

```csharp
public class ExplicitClassTest
{
    readonly ISomeService _sut;  // System Under Test is defined in terms of its public interface
    
    public ExplicitClassTest()
    {
        _sut = new ExplicitImplementation();
    }
    
    void test_some_functionality()
    {
        _sut.DoSomething();  // this tests the interface, so the behavior, rather than the implementation
        ...
    }
}
```

## Ghost public functions
> **TL;DR**<br/>
> EIMI promotes maintenaibility 

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

## Bottom line
I just tried sticking to the rule of using EIMI wherever possible for a while, and honestly I soon figured out that sticking to some design best practices (loose coupling, Dependency Inversion, encapsulation) was just made easier and enforced.

I would just recommend trying that.

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

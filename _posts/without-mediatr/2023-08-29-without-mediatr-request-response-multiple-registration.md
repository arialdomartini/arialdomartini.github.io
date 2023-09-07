---
layout: post
title: "Without MediatR - Request/response, multiple registration"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
include_in_index: false
tags:
- C#
- MediatR
---
# Request/response, multiple registration

## With MediatR

What happens if two different handlers for the same request are registered with MediatR?<br/>
Say you have `HandlerA` and `HandlerB` for the same request `Echo`:

```csharp
public record Echo(string Message) : IRequest<string>;

public class HandlerA : IRequestHandler<Echo, string>
{
    public Task<string> Handle(Echo request, CancellationToken cancellationToken) => Task.FromResult(request.Message);
}

public class HandlerB : IRequestHandler<Echo, string>
{
    public Task<string> Handle(Echo request, CancellationToken cancellationToken) => Task.FromResult("doh!");
}
```

and that you record them in the IoC container:

```csharp
cfg.Scan(scanner =>
{
    scanner.AssemblyContainingType(typeof(With));
    scanner.IncludeNamespaceContainingType<Echo>();
    scanner.WithDefaultConventions();
    scanner.AddAllTypesOf(typeof(IRequestHandler<,>));
});
            
cfg.For<IMediator>().Use<Mediator>();
```

Which response is returned by:

```csharp
var echoResponse = await mediator.Send(new Echo("some message"));
        
Assert.Equal(???, echoResponse); 
```

Answer: it depends on the order the 2 handlers are defined in the file. Move `HandlerB` before `HandlerA` and the result will change.

That's not a problem with IoC. The same also happens with a [direct configuration of MediatR](https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/RequestResponseMultipleRegistration/Direct/With.cs).

The issue is, by design MediatR dispatches the request to the last recorded handler, silently ignoring any other previously registered ones.

## Without MediatR
This just does not apply to the OOP solution. You are in full control of which instance is injected into `Client`'s costructor.

Some IoC containers such as Autofac may optionally exhibit the same behavior, but you still have authority over the instances to get (for example, with the [Autofac's Default Registrations](https://docs.autofac.org/en/latest/register/registration.html#default-registrations)).

Furthermore, if you wish to deliver the same request to multiple handlers, that is easily done both

* [with a fire and forget style](without-mediatr-request-response-multiple-handlers) and
* [collecting the return values](without-mediatr-request-response-multiple-handlers-with-reply)


## FAQs
* [The OOP solution creates coupling!](#the-oop-solutioncreates-coupling)
* [Isn't this exactly what MediatR does?](#isn-t-this-exactly-what-mediatr-does)
* [But the more decoupling the better, right?](#but-the-more-decoupling-the-better--right)
* [This is all theoretical](#this-is-all-theoretical)
* [The OOP solution violates CQRS!](#the-oop-solution-violates-cqrs)

### The OOP solution creates coupling!
Doesn't the OOP implementation create a strong coupling between `Client` and `PingHandler`?

**Answer**<br/>
No, it does not. `Client` and `PingHandler` are decoupled by `IPingHandler`.

Here's a strong-coupled implementation:

```csharp
class Client
{
    void ping_request_response()
    {
        var response = new PingHandler().Ping();

        Assert.Equal("Pong", response);
    }
}
```

`Client` directly depends on `PingHandler` and cannot be separated by it.<br/>
Injecting an instance of `PingHandler` would slightly improve the coupling, but not completely:

```csharp
class Client
{
    private readonly PingHandler _pingHandler;

    internal Client(PingHandler pingHandler)
    {
        _pingHandler = pingHandler;
    }

    void ping_request_response()
    {
        var response = new PingHandler().Ping();

        Assert.Equal("Pong", response);
    }
}
```

In both the cases, `Client` depends on the `PingHandler` implementation.<br/>
In UML:

![Client depends directly on PingHandler](static/img/without-mediatr/high-coupling.png)

This is *high-coupling*.

The conventional OOP implementation for loose coupling is based on the application of the [Dependency Inversion Principle][dependency-inversion-principle].<br/>
This mandates that 

1. High-level modules do not import anything from low-level modules, and both depend on abstractions.
2. Abstractions do not depend on details. Details depend on abstractions.

Translated to our use case: `Client` should depend on an an interface, not directly on the implementation. In other words, coupling is obtained by interposing an `IPingHandler` between `Client` and `PingHandler`.

![Client depends on an abstraction of PingHandler](static/img/without-mediatr/with-depencency-inversion.png)

```csharp
class Client
{
    private readonly IPingHandler _pingHandler;

    internal Client(IPingHandler pingHandler)
    {
        _pingHandler = pingHandler;
    }

    internal string UsePingHandler()
    {
        // do work
        return _pingHandler.Ping();
    }
}
```


### Isn't this exactly what MediatR does?
MediatR too interposes an abstraction between `Client` and `PingHandler`. Therefore, isn't it a legit implementation of the Dependency Inversion Principle?

**Answer**<br/>
No, it's not. MediatR interposes an extra abstraction layer.

![Client depends on an abstraction of PingHandler](static/img/without-mediatr/with-mediatr.png)

With MediatR `Client` depends on `IM-ediator` which in turn depends on `IRequestHandler<Ping, string>`, implemented by `PingHander`.<br/>
It's an unnecessary extra hop, that has negative effects.

### But the more decoupling the better, right?
What's the issue? An extra level of abstraction is harmless.

**Answer**<br/>
Unfortunately, it's not. It brings some negative consequences:

* `Client` now depends on `IMediator`, from which it can send whatever request to whatever handler.

```csharp
class Client
{
    private readonly IMediator _mediator;

    internal Client(IMediator mediator)
    {
        _mediator = mediator;
    }

    void aync uses_ping_handler()
    {
        var response = await _mediator.Send(new LaunchRocket());
    }
}
```

This created an implicit, global coupling: as a matter of fact, `Client` can reach the whole application.<br/>
Technically, this is a violation of the [Interface Segregation Principle][interface-segregation].

* The `Client`'s signatures are not honest anymore: you cannot infer which class `Client` depends on only by inspecting its signatures. Compare the constructors:


```csharp
internal Client(IMediator mediator)
```

with

```csharp
internal Client(IPingHandler handler)
```

This is a violation of the [Explicit Dependencies Principle][explicit-dependencies-principle].
    
### This is all theoretical
I don't see any practical problem. Only academic fixations.

**Answer**<br/>
There are pragmatic consequences. You will find some examples in the next pages of this article:

* If you record more than one handler, they will be silently ignored
* You cannot send a request to multiple handlers
* Sending a subclass of a Request results in a failure

### The OOP solution violates CQRS!
The MediatR solution has got a `Ping` class to represent a Command or a Query object. This is CQRS. The OOP solution is inferior.

**Answer**<br/>
If you mean that methods should either be commands performing an action, or queries returning data without side effects, you mean [CQS][cqs], not CQRS.<br/>
In fact, conforming to CQS does't necessarily require a class for any request. You are free to define one, though, if you like:

```csharp
file class Ping{}

file interface IPingHandler
{
    string Ping(Ping ping);
}                           

file class PingHandler : IPingHandler
{
    string IPingHandler.Ping(Ping ping) => "Pong";
}
```

Fittingly, the compiler warns that `ping` is unused and completely redundant. And this makes sense, if you think about it.<br/>
In MediatR for each and every method call a dedicated Request class must be defined, even if it does not have any property &mdash; so even if it physically transport no information at all &mdash. MediatR always needs a marker class to figure out which handler to dispatch the call to.<br/>
The plain OOP approach does not need that: method dispatching is directly performed with the native language mechanism based on methods, interfaces and classes, which is unsurprisigly sounder and more robust (for example, sending a subclass of a Request does not result in a failure, as it does with MediatR).

In conclusion: you are free, but not forced, to use a request class. You have the full flexibility to use any of the following approaches, and still be CQS compliant:

```csharp
file interface IPeopleService
{
    string DoSomething(PersonRequest person);
}

file interface IPeopleService
{
    string DoSomething(Person person, bool someCondition, string message);
}
```

# References

* [You probably don't need MediatR][you-probably-dont-need-mediatr]
* [wihout-mediatr repository][without-mediatr-repo]

* [Dependency Inversion Principle - Wikipedia][dependency-inversion-principle]
* [Explicit Dependencies Principle][explicit-dependencies-principle]
* [Interface Segregation Principle][interface-segregation]
* [Command-Query Separation (CQS)][cqs]

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/7)


[you-probably-dont-need-mediatr]: https://arialdomartini.github.io/mediatr
[without-mediatr-repo]: github.com/arialdomartini/without-mediatr
[dependency-inversion-principle]: https://en.wikipedia.org/wiki/Dependency_inversion_principle
[explicit-dependencies-principle]: https://docs.microsoft.com/en-us/dotnet/architecture/modern-web-apps-azure/architectural-principles#explicit-dependencies
[interface-segregation]: https://en.wikipedia.org/wiki/Interface_segregation_principle
[cqs]: https://en.wikipedia.org/wiki/Command%E2%80%93query_separation
[query-object]: https://martinfowler.com/eaaCatalog/queryObject.html
[command-object]: 

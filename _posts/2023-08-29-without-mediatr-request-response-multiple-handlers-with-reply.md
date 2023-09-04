---
layout: post
title: "Without MediatR - Request/response, multiple registration"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
include_in_index: false
tags:
- C#
- MediatR
---
# Request/response, multiple handlers, with reply

| MediatR       |
|---------------|
| Not supported |
| Not supported |

## With MediatR
This pattern is not supported by MediatR. By design, [Request/response messages are dispatched to a single handler][mediatr-basics].

## Withot MediatR
There are no reasons why a message cannot be dispatched to multiple class instances, and their replies reported to the caller. 

Indeed, this is a so common pattern in parallel / distributing computing that it has a name, [MapReduce][map-reduce].

.NET supports this style: think for example to [Task.WhenAll Method][when-all]: it creates a task returning a collection of all the results of the completed `Task<TResult>`, for you to reduce.<br/>

OOP offers multiple patterns for implementing this style: for example, you might define the handlers as observers of your client with [GoF Observer Pattern][observer-pattern], or representing multiple targets as a single one, with [Composite Pattern][composite-pattern].

The idea of MapReduce is so pervasive that it is even embedded in C#. Given a collection of handlers, you can easily map the invocations to each instance with `Select`, and reduce the results with `Aggregate`: 

```csharp
handlers
    .Select(h => h.Handle(message))
	.Aggregate((accumulator, current) => combine(accumulator, current))`
```

So, let's translate the dispatch to multiple handlers, to plain OOP, without using MediatR.<br/>
If you only want to collect all the return values, just use the *map* part of MapReduce:

```csharp
class Client
{
    private readonly IEnumerable<IMyHandler> _handlers;

    internal Client(IEnumerable<IMyHandler> handlers)
    {
        _handlers = handlers;
    }

    internal void DispatchToAll()
    {
        var message = new SomeRequest("my message");

        var dispatchAll = _handlers.Select(h => h.DoSomething(message));

        Task.WhenAll(dispatchAll).Wait();
    }
}
```
[code](https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/RequestResponseMultipleDispatch/Collection/Without.cs)

You can combine this with the classical [GoF Composite Pattern][composite-pattern], so `Client` would not even know it is dealing with multiple handlers. In the following snippet, `Handlers` deals with MapReduce, offering `Client` an ordinary `IMyHandler` interface, identical to the one of a single handler:

```csharp
class Handlers : IMyHandler
{
    private readonly IEnumerable<IMyHandler> _handlers;

    internal Handlers(IEnumerable<IMyHandler> handlers)
    {
        _handlers = handlers;
    }
    
    Task IMyHandler.DoSomething(SomeRequest request)
    {
        var dispatchAll = _handlers.Select(h => h.DoSomething(request));
        Task.WhenAll(dispatchAll).Wait();
        return Task.CompletedTask;
    }
}

class Client
{
    private readonly IMyHandler _handler;

    internal Client(IMyHandler handler)
    {
        _handler = handler;
    }

    internal async Task DispatchToAll()
    {
        var message = new SomeRequest("my message");

        await _handler.DoSomething(message);
    }
}
```
[code](https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/RequestResponseMultipleDispatch/Composite/Without.cs)


If you need to aggregate all the results to a single value, you are free to write your custom logic in `Handlers`, even to inject it as a replacable [Strategy][strategy-pattern].

For the sake of simplicity, let's have handlers returning booleans, and a call-site reducing the return values with a trivial `and`. Of course, there are no inherent constraints to scale this up to whatever complex data structure and domain logic:

```csharp
class ValidatorA : IMyValidator
{
    bool IMyValidator.Validate(SomeRequest request)
    {
        // domain logic
        return true;
    }
}

class ValidatorB : IMyValidator
{
    bool IMyValidator.Validate(SomeRequest request)
    {
        // domain logic
        return false;
    }
}
```

Using again a [Composite Pattern][composite-pattern], the MapReduce logic would be:

```csharp
class Validators : IMyValidator
{
    private readonly IEnumerable<IMyValidator> _handlers;

    internal Validators(IEnumerable<IMyValidator> handlers)
    {
        _handlers = handlers;
    }
    
    bool IMyValidator.Validate(SomeRequest request) => 
        _handlers
            .Select(h => h.Validate(request))  // map
            .Aggregate((acc, i) => acc && i);  // reduce
}
```

For the `Client` point of view, `Validators` is an ordinary handler &mdash; that's loosly-coupling at work. Under the hood, `Validate(request)` dispatches `request` to all the validators, and returns back a consolidated, reduced value:

```csharp
class Client
{
    private readonly IMyValidator _validator;

    internal Client(IMyValidator validator)
    {
        _validator = validator;
    }

    internal string DispatchToAll(SomeRequest request) => 
        _validator.Validate(request) 
            ? "all good!" 
            : "sorry, the request is not valid";
}
```
[code](https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/RequestResponseMultipleDispatchWithReduce/Without.cs)

## FAQs
### Can't this be applied with MediatR too?
**Question**<br/>
What about performing that MapReduce in a MediatR handler?

**Answer**<br/>
That would not work.<br/>
You can think of defining a handler for `SomeRequest`, and use its code to programmatically dispatch the same request to other handlers. But it's not hard to realize you will be back to the same starting problem: there is no way to use MediatR for invoking those handlers; again, Request/response messages are dispatched to a single handler.<br/>
It the request dispatch is done manually, without the support to the MediatR pipelines, this would just defy the reason why MediatR was used in the first place.

C# already provides a very powerful and flexible message dispatching mechanism. It is type safe, checked at compile time, compatible by design with all the possible other OOP and FP patterns.<br/>
MediatR wrapped the native C# event dispatch with an abstraction, foundamentally reinventing it in a less flexible flavor.

I tend to listen to code: if something is hard to develop, either I'm trying to design the wrong solution, or I'm using the wrong tool.

# References
* [Observer Pattern][observer-pattern]
* [Composite Pattern][composite-pattern]
* [Strategy Pattern][strategy-pattern]
* [Task.WhenAll Method][when-all]
* [MediatR Basics][mediatr-basics]
* [MapReduce][map-reduce]

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/22)

[observer-pattern]: https://en.wikipedia.org/wiki/Observer_pattern
[composite-pattern]: https://en.wikipedia.org/wiki/Composite_pattern
[strategy-pattern]: https://en.wikipedia.org/wiki/Strategy_pattern
[when-all]: https://learn.microsoft.com/en-us/dotnet/api/system.threading.tasks.task.whenall?view=net-7.0
[mediatr-basics]: https://github.com/jbogard/MediatR/wiki#basics
[map-reduce]: https://en.wikipedia.org/wiki/MapReduce

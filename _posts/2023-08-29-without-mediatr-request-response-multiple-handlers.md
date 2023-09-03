---
layout: post
title: "Without MediatR - Request/response, multiple handlers"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
include_in_index: false
tags:
- C#
- MediatR
---
# Request/response, sending the same request to multiple handlers

| MediatR       |
|---------------|
| Not supported |
| Not supported |

## With MediatR

This is not supported. By design, Request/response messages are dispatched to a single handler (see [MediatR wiki - Basics][mediatr-basics]).

If you register more than a handler for the same request, only the last one will be dispatched the request, while [the other ones will be silently discarded](without-mediatr-request-response-multiple-registration).

## Without MediatR
With plain OOP and FP there are plenty of options for dispatching the same call to multiple targets.

You might inject more than one handler and just cycle on them:

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
[code][multiple-dispatch-collection]

You could use the classical [GoF Composite Pattern][composite-pattern] to transparently treat group of handlers as a single instance. The idea is to move the cycle in a separate class, and to give that class the same interface of an ordinary handler:

```csharp
file class Handlers : IMyHandler
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
```

The client would have no clue whether the passed handler is a collection of handlers or a single instance &mdash; and in this case, which one &mdash; making it apparent once again that the plain OOP approach does exhibits low-coupling:

```csharp
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
[code][multiple-dispatch-composite-pattern]


# References

* [MediatR wiki - Basics][mediatr-basics]
* [Without MediatR - Request/response, multiple registration](without-mediatr-request-response-multiple-registration)
* [Composite Pattern - Wikipedia][composite-pattern]
* Code
  * [Multiple dispatch with a collection][multiple-dispatch-collection]
  * [Multiple dispatch with Composite Pattern][multiple-dispatch-composite-pattern]

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/22)

[mediatr-basics]: https://github.com/jbogard/MediatR/wiki#basics
[composite-pattern]: https://en.wikipedia.org/wiki/Composite_pattern
[multiple-dispatch-collection]: https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/RequestResponseMultipleDispatch/Collection/Without.cs#L6
[multiple-dispatch-composite-pattern]: https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/RequestResponseMultipleDispatch/Composite/Without.cs

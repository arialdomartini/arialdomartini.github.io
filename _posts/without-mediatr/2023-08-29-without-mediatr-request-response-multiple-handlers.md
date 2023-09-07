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
You want to deliver the same message to more than 1 target. 

We will explore the two cases, whether you want to perform a fire-and-forget (this page), or you wish to collect a reply back from all the targets, a case covered by [Multiple handlers with return value](without-mediatr-request-response-multiple-handlers-with-reply).


## With MediatR
This is not supported. By design, Request/response messages are dispatched to a single handler (see [MediatR wiki - Basics][mediatr-basics]).

If you register more than a handler for the same request, only the last one will be dispatched the request, while [the other ones will be silently discarded](without-mediatr-request-response-multiple-registration).

The idiomatic way to cover this case is to merge the logic of the ideal multiple handlers in a single one (see [Question: Multiple parallel send with merge result][parallel-send]).

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

You might prefer a functional approach and use an enhanced method dispatch, such as:

```csharp
internal static class CompositeExtensions
{
    internal static async Task InvokeAll<T>(this IEnumerable<T> handlers, Func<T, Task> f)
    {
        await Task.WhenAll(handlers.Select(f));
    }
}
```

## FAQs
* [This can be done with notifications!](#this-can-be-done-with-notifications)
* [How can you dispatch a request to multiple targets and get a response?](how-can-you-dispatch-a-request-to-multiple-targets-and-get-a-response)

### This can be done with notifications!
You can send the same message to multiple targets using Notifications instead of Requests!

**Answer**<br/>
No, that's not equivalent. There are 2 notable differences:

1. MediatR raises an error if there is no handler registered for a specific request. With notifications, it does not, and the dispatch silently fails.

2. Requests can return a value, Notifications cannot.

Queries/Commands and Notifications are not equivalent.

### How can you dispatch a request to multiple targets and get a response?
**Answer**<br/>
Very good question. This deserves [a dedicated page](without-mediatr-request-response-multiple-handlers-with-reply).


# References

* [MediatR wiki - Basics][mediatr-basics]
* [Without MediatR - Request/response, multiple registration](without-mediatr-request-response-multiple-registration)
* [Without MediatR - Request/response, multiple dispatch with return value](without-mediatr-request-response-multiple-handlers-with-reply)
* [Composite Pattern - Wikipedia][composite-pattern]
* Code
  * [Multiple dispatch with a collection][multiple-dispatch-collection]
  * [Multiple dispatch with Composite Pattern][multiple-dispatch-composite-pattern]
* [Question: Multiple parallel send with merge result][parallel-send]

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/7)

[mediatr-basics]: https://github.com/jbogard/MediatR/wiki#basics
[composite-pattern]: https://en.wikipedia.org/wiki/Composite_pattern
[multiple-dispatch-collection]: https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/RequestResponseMultipleDispatch/Collection/Without.cs#L6
[multiple-dispatch-composite-pattern]: https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/RequestResponseMultipleDispatch/Composite/Without.cs
[parallel-send]: https://github.com/jbogard/MediatR/issues/609

---
layout: post
title: "Without MediatR - Request/response, handler not returning a value"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
include_in_index: false
tags:
- C#
- MediatR
---
# Request/response with a handler not returning a value
## With MediatR
In MediatR there used to be two flavors of request types (see [Request types][mediatr-request-types]):

* `IRequest<TResponse>`, for requests returning a value
* `IRequest`, for the ones not returning any value

Depending on which case the handler covered, it needed to implement either:

* `IRequestHandler<TRequest, TResponse>`, or
* `IRequestHandler<TRequest>`

So, for requests not needing any return value, the code is like:

```csharp
class PingHandler : IRequestHandler<Ping>
{
    internal static bool HasBeenCalled { get; set; }
    
    public Task Handle(Ping request, CancellationToken cancellationToken)
    {
        // do work
        return Task.CompletedTask;
    }
}
```
[code](https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/RequestResponseNotReturningAValue/With.cs)


Although this is still supported, since version 5 (see [Migration guide 4.1.0 > 5.0.1][migration-guide]) requests handler that return a value, and those that do not were unified into one single type `IRequestHandler<T, U>`. Handler not returning a value shall return `Unit`.

```csharp
class Ping : IRequest<Unit> { }

class PingHandler : IRequestHandler<Ping, Unit>
{
    public Task<Unit> Handle(Ping request, CancellationToken cancellationToken)
    {
        // do work
        return Task.FromResult(Unit.Value);
    }
}
```
[code](https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/RequestResponseNotReturningAValue/ReturningUnit/With.cs)


## Without MediatR
C# method dispathing supports `void` methods out of the box, so there is nothing special you have to do other than declaring the method as void:

```csharp
class PingHandler : IPingHandler
{
    void IPingHandler.Ping()
    {
        // do work
    }
}
```
[code](https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/RequestResponseNotReturningAValue/Without.cs)

Or course, nothing prevents you from returning `Unit`, if you prefer. It's plain C#, after all:

```csharp
interface IPingHandler
{
    Unit Ping(string whatever);
}
```

## FAQs
### Why is the OOP approach better?
**Answer**<br/>
The interface of `PingHandler` does not change only because one of its methods returns `void` instead of `string` (e.g., there is no impact on the IoC registration).

By the way, notice that you have the freedom to return either `void` or `Task`, depending on the asynchronous nature of the method.

# References
* [Request types - MediatR Wiki][mediatr-request-types]
* [MediatR - Migration Guide 4.1.0 > 5.0.1][migration-guide]

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/7)

[mediatr-request-types]: https://github.com/jbogard/MediatR/wiki/#request-types
[migration-guide]: https://github.com/jbogard/MediatR/issues/274#issuecomment-394716576

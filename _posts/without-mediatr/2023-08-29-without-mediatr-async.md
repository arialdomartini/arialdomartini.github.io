---
layout: post
title: "Without MediatR - Async"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
include_in_index: false
tags:
- C#
- MediatR
---
# Async
## With MediatR
Both send and publish are asynchronous from the IMediator side, with corresponding synchronous and asynchronous-based interfaces/base classes for requests/responses/notification handlers.

So a handler can be make async as follows:

```csharp
public class PingHandler : IRequestHandler<Ping, Pong>
{
    public async Task<Pong> Handle(Ping request, CancellationToken cancellationToken)
    {
        await DoPong(); // Whatever DoPong does
    }
}
```
[code](https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/Async/With.cs)

## Without MediatR
The plain OOP approach relies on the native C# functionalities, so of course handler methods can be made async:

```csharp

interface IPingHandler
{
    Task<Pong> Handle(Ping request);
}

class PingHandler : IPingHandler
{
    async Task<Pong> IPingHandler.Handle(Ping request)
    {
        // do work
        return await Task.FromResult(new Pong());
    }
}
```
[code](https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/Async/Without.cs)

## FAQs
### Which approach is preferrable?
**Answer**<br/>
With MediatR the `IRequestHandler` interface imposes returning a `Task`.<br/>
With OOP there are no constraints on the signature:

```csharp
interface IPingHandler
{
    Pong Handle(Ping request);
}

class PingHandler : IPingHandler
{
    Pong IPingHandler.Handle(Ping request)
    {
        // do work
        return new Pong();
    }
}
```



### Can IMediatR be synchronous?
**Answer**<br/>
No. From the IMediator side, the interface is async-only.

See the discussion on the [Synchronous .Send()][syncrhonous-send] and [Synchronous RequestHandlers and await][synchronous_request-handlers-and-await] on MediatR GitHub Issues.

### Is it possible to have two handlers for the same class, one synchronous and the other asynchronous?
**Answer**<br/>
With MediatR no, it is not. [A single handler handling multiple requests](without-mediatr-request-response-multiple-requests) requires defining 2 separate Request classes.

With plain OOP, it is a matter of defining an overload or a separate handler method:

```csharp
interface IPingHandler
{
    Task<Pong> HandleAsync(Ping request);
    Pong Handle(Ping request);
}
```
[code](https://github.com/arialdomartini/without-mediatr/tree/master/src/WithoutMediatR/Async/BothAsyncAndSync)

See a discussion on [StackOverflow][async-only].

# References
* [Composite Pattern][composite-pattern]
* [Observer Pattern][observer-pattern]
* [C# Events][events]
* [Synchronous .Send() - MediatR issues][syncrhonous-send]
* [Synchronous RequestHandlers and await - MediatR issues][synchronous_request-handlers-and-await]
* [Have a synchronous handler and an asynchronous handler - StackOverflow][async-only]

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/7)

[composite-pattern]: https://en.wikipedia.org/wiki/Composite_pattern
[observer-pattern]: https://en.wikipedia.org/wiki/Observer_pattern
[events]: https://learn.microsoft.com/en-us/dotnet/csharp/programming-guide/events/
[async-only]: https://stackoverflow.com/questions/48007262/have-a-synchronous-handler-and-an-asynchronous-handler
[syncrhonous-send]: https://github.com/jbogard/MediatR/issues/170
[synchronous_request-handlers-and-await]: https://github.com/jbogard/MediatR/issues/267

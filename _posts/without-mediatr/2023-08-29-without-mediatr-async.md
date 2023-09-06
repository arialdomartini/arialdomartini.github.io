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


# References
* [Composite Pattern][composite-pattern]
* [Observer Pattern][observer-pattern]
* [C# Events][events]

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/22)

[composite-pattern]: https://en.wikipedia.org/wiki/Composite_pattern
[observer-pattern]: https://en.wikipedia.org/wiki/Observer_pattern
[events]: https://learn.microsoft.com/en-us/dotnet/csharp/programming-guide/events/

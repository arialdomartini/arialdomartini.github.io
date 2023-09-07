---
layout: post
title: "Without MediatR - Behaviors"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
include_in_index: false
tags:
- C#
- MediatR
---
# Behaviors
MediatR behaviors are a way to add cross-cutting concerns to the processing pipeline of requests and notifications. They intercept and modify requests and notifications before they reach their respective handlers.

Pipelines are classically implemented in OOP with [Chain Of Responsibility][chain-of-responsibility], 

The astute reader can easily discern that the Chain of Responsibility diagram unmistakably resembles a pipeline:

![Chain of Responsibility as an execution pipeline](static/img/without-mediatr/chain-of-responsibility.png)


The Chain of Responsibility patter is structurally nearly identical to [Decorator][decorator-pattern] and it is interesting that not using Decorators is the declared goal that MediatR aims to achieve:

> New in MediatR 3.0 are behaviors, which allow you to build your own pipeline<br/>
> directly inside of MediatR without resolving to using decorators around your handlers

## With MediatR
You need to implement the dedicated interface `IPipelineBehavior<TRequest, TResponse>`:

```csharp
public class LoggingBehavior<TRequest, TResponse> : IPipelineBehavior<TRequest, TResponse>
    where TRequest : IRequest<TResponse>
{
    private readonly ILogger<LoggingBehavior<TRequest, TResponse>> _logger;

    public LoggingBehavior(ILogger<LoggingBehavior<TRequest, TResponse>> logger)
    {
        _logger = logger;
    }

    public async Task<TResponse> Handle(TRequest request, RequestHandlerDelegate<TResponse> next, CancellationToken cancellationToken)
    {
        _logger.LogInformation($"Handling {typeof(TRequest).Name}");
        var response = await next();
        _logger.LogInformation($"Handled {typeof(TResponse).Name}");

        return response;
    }
}
```
[code](https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/Behaviors/With.cs)

This special handler shall be registered as an `IPipelineBehavior<Ping, Pong>`:

```csharp
cfg.For<IPipelineBehavior<Ping, Pong>>().Add(new LoggingBehavior<Ping, Pong>(_logger));
```

## Without MediatR
A possible implementation of a Chain of Responsibility is something like:

```csharp
interface IHandler
{
    Result HandleRequest(Request request);
}

class ConcreteHandler1 : IHandler
{
    private IHandler _nextHandler;

    ConcreteHandler1(IHandler nextHandler)
    {
        _nextHandler = nextHandler;
    }

    Result IHandler.HandleRequest(Request request)
    {
        // pre-processing
		
        var response = nextHandler.HandleRequest(request);
		
		// post-processing
		
		return response;
    }
}
```

which is not surprisingly very similar to the `LoggingBehavior<TRequest, TResponse>` above, with a very important difference: while the MediatR handler implements `IPipelineBehavior<TRequest, TResponse>`, the OOP chain of responsibility implements the very same interface of the original handler. In that sense, the Chain of Responsibility pattern is similar to the Decorator: the resulting chain can transparently passed to the client as a replacement of the handler.

```csharp
class LoggingBehavior : IPingHandler
{
    private readonly ILogger _logger;
    private readonly IPingHandler _next;

    public LoggingBehavior(ILogger logger, IPingHandler next)
    {
        _logger = logger;
        _next = next;
    }

    async Task<Pong> IPingHandler.Handle(Ping request)
    {
        _logger.LogInformation($"Handling Ping");
        var response = await _next.Handle(request);
        _logger.LogInformation($"Handled Pong");

        return response;
    }
}
```
[code](https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/Behaviors/Without.cs)

## FAQs
### Which implementation is preferrable?
**Answer**<br/>
As anticipated in the index, replacing MediatR is a matter of using an ordinary interface and, when needed, implementing a decorator.

The OOP approach is generally more linear, as it does not rely on any special interface: the behavior can continue exposing the domain-driven interface of the OOP handler. 

The developer is in full control of the order and the way behaviors are combined together (e.g. using a constructor or a `SetNext()` method).


# References
* [Behaviors - MediatR][mediatr-behaviors]
* [Decorator Pattern][decorator-pattern]
* [Chain of Responsibility][chain-of-responsibility]
* [Chain of Responsibility - refactoring.guru][chain-of-responsibility-refactoring-guru]

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/7)

[mediatr-behaviors]: https://github.com/jbogard/MediatR/wiki/Behaviors
[decorator-pattern]: https://en.wikipedia.org/wiki/Decorator_pattern
[chain-of-responsibility]: https://en.wikipedia.org/wiki/Chain-of-responsibility_pattern
[chain-of-responsibility-refactoring-guru]: https://refactoring.guru/design-patterns/chain-of-responsibility

---
layout: post
title: "Without MediatR - Exception Handling"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
include_in_index: false
tags:
- C#
- MediatR
---
# Exception Handling
## With MediatR
Exception handling is implemented with behaviors, by using the `IPipelineBehavior` concept. It requires to add the RequestExceptionProcessorBehavior to the request execution Pipeline.

Consider an handler raising a specific exception:

```csharp
public class PingProtectedResourceHandler : IRequestHandler<PingProtectedResource, Pong>
{
    public Task<Pong> Handle(PingProtectedResource request, CancellationToken cancellationToken)
    {
        throw new ForbiddenException();
    }
}
```

The exception can be captured by registering a `IRequestExceptionHandler<PingResource, Pong, ForbiddenException>` exception handler:

```csharp
public class AccessDeniedExceptionHandler : IRequestExceptionHandler<PingResource, Pong, ForbiddenException>
{
    private readonly TextWriter _writer;

    public AccessDeniedExceptionHandler(TextWriter writer) => _writer = writer;

    public async Task Handle(PingResource request,
        ForbiddenException exception,
        RequestExceptionHandlerState<Pong> state,
        CancellationToken cancellationToken)
    {
        await _writer.WriteLineAsync($"---- Exception Handler: '{typeof(AccessDeniedExceptionHandler).FullName}'").ConfigureAwait(false);
        state.SetHandled(new Pong());
    }
}
```

## Without MediatR
### Replicating the MediatR design
This is easily done by using the [Decorator Pattern][decorator-pattern], that is by wrapping the message handler in a class aimed to handling the exceptions, and implementing the very same interface the original message handler implements, so that it can be transparently replaced:

```csharp
public class AccessDeniedExceptionHandler : IPingProtectedResourceHandler
{
    private readonly IPingProtectedResourceHandler _handler;

    public AccessDeniedExceptionHandler(IPingProtectedResourceHandler handler)
    {
        _handler = handler;
    }

    async Task<Pong> IPingProtectedResourceHandler.Handle(PingProtectedResource request)
    {
        try
        {
            return await _handler.Handle(request);
        }
        catch (Exception e)
        {
            // handle the exception
			
            return new Pong();
        }
    }
}
```
[code](https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/ExceptionHandling/WithIoC/Without.cs)

Each IoC container provides a specific way to decorate an instance with a class, for example with

```csharp
cfg
    .For<IPingProtectedResourceHandler>()
	.Use<PingProtectedResourceHandler>();

cfg
    .For<IPingProtectedResourceHandler>()
	.DecorateAllWith<AccessDeniedExceptionHandler>();
```

Decorating by hand is a matter of passing the handler to the decorator's constructor.

```csharp
new AccessDeniedExceptionHandler(new PingProtectedResourceHandler())
```

### Just use try/catch
The previous approach mimics the design with MediatR.

The plain OOP approach is about replacing the message dispatching via MediatR with the message dispatching natively implemented by C#. This, in turn, already implements exception handling, including capturing exceptions and propagating them up the call stack. There is rarely the need to reinvent the wheel.

The C# runtime already searches automatically the call stack for a suitable exception handler. An exception handler is just a block of code enclosed within a `catch` construct. So, in most of the cases, you might find that the MediatR exception handling mechanism can be simply replaced by using a plain old `try/catch` construct, either in the call site or in the handler.

## FAQs
### Which approach is preferrable?
**Answer**<br/>
You have surely noticed that all the use cases are covered either with the [Composite Pattern][composite-pattern] or with the Decorator, if not even natively by the language, and always without the need for specific interfaces. 

There is value in the consistency and simplicity of this approach. The less you need to re-invent the wheel, the more the approach is natively supported by the language, the better.

# References
* [Decorator Pattern][decorator-pattern]
* [Composite Pattern][composite-pattern]


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/7)

[decorator-pattern]: https://en.wikipedia.org/wiki/Decorator_pattern
[composite-pattern]: https://en.wikipedia.org/wiki/Composite_pattern


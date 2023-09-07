---
layout: post
title: "Without MediatR - Request/response, subtyping"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
include_in_index: false
tags:
- C#
- MediatR
---
# Request/response, subtyping
What happens sending a subtype of a request?

## With MediatR
A handler defined as:

```csharp
class PingHandler : IRequestHandler<Ping, string>
{
    public Task<string> Handle(Ping request, CancellationToken cancellationToken) => 
        Task.FromResult("Pong");
}
```

receives instances of both `Ping` and subtypes of `Ping`:

```csharp
record Ping : IRequest<string>;
record SubTypeOfPing : Ping;
```
[code](https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/RequestResponseSubtyping/With.cs)


## Without MediatR
Method dispatching in C# is polymorhic by design, so no surpsises that everything works out-of-the-box.<br/>

This equally succeds both with `Ping` and `SubTypeOfPing`:

```csharp
record SubTypeOfPing : Ping;
record Ping;

class PingHandler : IPingHandler
{
    Task<string> IPingHandler.Handle(Ping request) => 
        Task.FromResult("Pong");
}


var response = await _client.UsePingHandler(new Ping());
Assert.Equal("Pong", response);

response = await _client.UsePingHandler(new SubTypeOfPing());
Assert.Equal("Pong", response);
```
[code](https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/RequestResponseSubtyping/Without.cs)


## FAQs
### Which approach is preferrable?
**Answer**<br/>
C# exhibits a [strong behavioral subtyping][behavioral-subtyping], so the OOP approach is compliant with the [Liskov Substitution Principle][liskov], which states that subtypes must be substitutable for their base types without altering the correctness of the program.<br/>
So, this behavior is natively supported by the language and consistently applied.

With MediatR the polymorphic dispatch relies on the capabilities of the underlying dependency injection library.<br/>
The behavior is a bit more sensitive and depending on the setup you might encounter some surprises.

As an example, the following would result in an `InvalidOperationException`, despite `AddTransient<IRequestHandler<Ping, string>, PingHandler>()` looks like a legit registration:

```csharp
file record SubTypeOfPing : Ping;
file record Ping : IRequest<string>;

file class PingHandler : IRequestHandler<Ping, string>
{
    public Task<string> Handle(Ping request, CancellationToken cancellationToken)
    {
        return Task.FromResult("Pong");
    }
}

var serviceProvider =
    new ServiceCollection()
        .AddTransient<IRequestHandler<Ping, string>, PingHandler>()
        .BuildServiceProvider();

var mediator = new Mediator(serviceProvider);
mediator.Send(new SubTypeOfPing()); // throws an exception
```
[code](https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/RequestResponseSubtyping/Direct/With.cs)

You may read more in the discussion at [Issue - Polymorphic dispatch not working](https://github.com/jbogard/MediatR.Extensions.Microsoft.DependencyInjection/issues/24).

### What happens if a handler for the subtype is also registered?
What if a handler for `SubTypeOfPing` is also registered?

```csharp
public class SubTypeOfPingHandler : IRequestHandler<SubTypeOfPing, string>
{
    public Task<string> Handle(SubTypeOfPing request, CancellationToken cancellationToken)
    {
        return Task.FromResult("This is the handler for the subtype");
    }
}
```

Which handler will get the request, `SubTypeOfPingHandler` or `PingHandler`?

**Answer**<br/>
It all depends on the order of registration, because [MediatR does not support registering multiple handlers for the same request](without-mediatr-request-response-multiple-registration).


# References
* [Liskov Substitution Principle][liskov]
* [Behavioral Subtyping][behavioral-subtyping]

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/22)

[liskov]: https://en.wikipedia.org/wiki/Liskov_substitution_principle
[behavioral-subtyping]: https://en.wikipedia.org/wiki/Behavioral_subtyping

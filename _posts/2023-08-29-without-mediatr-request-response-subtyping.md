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
Having

```csharp
record Ping : IRequest<string>;
record SubTypeOfPing : Ping;
```

and a handler registerd for `Ping`

```csharp
class PingHandler : IRequestHandler<Ping, string>
{
    public Task<string> Handle(Ping request, CancellationToken cancellationToken) => 
        Task.FromResult("Pong");
}
```

it turns out that `_mediator.Send(new Ping())` succeeds, while `_mediator.Send(new SubTypeOfPing())` fails with an exception:

```csharp
System.InvalidOperationException
No service for type 'MediatR.IRequestHandler`2[WithoutMediatR.RequestResponseSubtyping.<With>__SubTypeOfPing,System.String]' has been registered.
   at Microsoft.Extensions.DependencyInjection.ServiceProviderServiceExtensions.GetRequiredService(IServiceProvider provider, Type serviceType)
   ...
```
[code](https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/RequestResponseSubtyping/With.cs)

## Without MediatR
Method dispatching in C# is polymorhic.

```csharp
record SubTypeOfPing : Ping;
record Ping;

class PingHandler : IPingHandler
{
    Task<string> IPingHandler.Handle(Ping request) => 
        Task.FromResult("Pong");
}
```

This equally succeds both with `Ping` and `SubTypeOfPing`:

```csharp
var response = await _client.UsePingHandler(new Ping());
Assert.Equal("Pong", response);

response = await _client.UsePingHandler(new SubTypeOfPing());
Assert.Equal("Pong", response);
```
[code](https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/RequestResponseSubtyping/Without.cs)


## FAQs
### Which approach is correct?
**Answer**<br/>
C# exhibits a [strong behavioral subtyping][behavioral-subtyping], so the OOP approach is compliant with the [Liskov Substitution Principle][liskov], which states that subtypes must be substitutable for their base types without altering the correctness of the program. This is how all the object-oriented programming languages are designed.

MediatR has the opposite behavior.

### This is an intentional and legit MediatR design choice
***Answer**<br/>
It could be, but this is inconsistent with MediatR itself.

Sending requests does not support subtyping, publishing notifications does.<br/>
That is, while this fails with an exception:

```csharp
record Ping : IRequest<string>;
record SubTypeOfPing : Ping;

await mediator.Send(new SubTypeOfPing())
```

the following just succeeds:

```csharp
record Bar : INotification;
record SubTypeOfBar : Bar;

await mediator.Publish(new SubTypeOfBar());
```
[code](https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/RequestResponseSubtyping/With.cs)

With this regards, C# is always consitent with itself.


# References
* [Liskov Substitution Principle][liskov]
* [Behavioral Subtyping][behavioral-subtyping]

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/22)

[liskov]: https://en.wikipedia.org/wiki/Liskov_substitution_principle
[behavioral-subtyping]: https://en.wikipedia.org/wiki/Behavioral_subtyping

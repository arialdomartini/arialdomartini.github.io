---
layout: post
title: "Without MediatR - Request/response, multiple requests"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
include_in_index: false
tags:
- C#
- MediatR
---
# Request/response, single handler handling multiple requests

What about having a class being responsible for handling more than one request?<br/>

## With MediatR
Imagine having 2 different requests:

```csharp
record Ping : IRequest<string> { }
record Echo(string Message) : IRequest<string>;
```

Defining a handler able to handle both the types of request with MediatR is as simple as implementing both `IRequestHandler<Ping, string>` and `IRequestHandler<Echo, string>`:


```csharp
class MyHandler :
    IRequestHandler<Ping, string>,
    IRequestHandler<Echo, string>
{
    public Task<string> Handle(Ping request, CancellationToken cancellationToken)
    {
        return Task.FromResult("Pong");
    }

    public Task<string> Handle(Echo request, CancellationToken cancellationToken)
    {
        return Task.FromResult(request.Message);
    }
}
```
[code](https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/RequestResponseMultipleRequests/Direct/With.cs)

## Without MediatR
The plain OOP implementation is equally simple:

```csharp
interface IMyHandler
{
    string Ping();
    string Echo(string message);
}

class MyHandler : IMyHandler
{
    string IMyHandler.Ping() => "Pong";
    string IMyHandler.Echo(string message) => message;
}
```

## FAQs
### How is the OOP approach preferrable?
**Answer**<br/>
Compare the interface of `MyHandler` with MediatR:

```csharp
Task<string> Handle(Ping request, CancellationToken cancellationToken);
Task<string> Handle(Echo request, CancellationToken cancellationToken);
```

and without:

```csharp
string Ping();
string Echo(string message);
```

The naming in the OOP interface is domain-driven; With MediatR the code tends to contain many repetitions of `Send` and `Handle`, which are a missed opportunity to use a business-driven, obiquitous language. The reference to domain notions is moved from the method name to the parameters. This once again how MediatR is basically a re-implementation of the native C# method dispatching.

It is also interesting to compare the 2 class types, and to notice how in one notions from the domain such as `Ping` and `Echo` mix with notions from the infrastructure, inherited from an external library:

```csharp
class MyHandler :
    IRequestHandler<Ping, string>,
    IRequestHandler<Echo, string>
```

and how the other is instead purely domain-driven:

```csharp
class MyHandler : 
	IMyHandler
```

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/7)

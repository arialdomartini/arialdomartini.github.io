---
layout: post
title: "Without MediatR - Streams and AsyncEnumerables"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
include_in_index: false
tags:
- C#
- MediatR
---
# Request/response, Streams and AsyncEnumerables
## With MediatR
MediatR requires you to use a special type for the request, and a special one for the request handler:

```csharp
record StreamRequest : IStreamRequest<string>;}

class StreamHandler : IStreamRequestHandler<StreamRequest, string>
{
    public async IAsyncEnumerable<string> Handle(StreamRequest request, CancellationToken cancellationToken)
    {
        yield return "foo";
        yield return "bar";
    }
}
```
[code](https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/Stream/With.cs)

## Without MediatR
With plain OOP, there's nothing special you have to change, other than returning an `IAsyncEnumerable` of results. The rest is already natively supported by C#:

```csharp
interface IStreamHandler
{
    IAsyncEnumerable<string> Ping();
}

class StreamHandler : IStreamHandler
{
    async IAsyncEnumerable<string> IStreamHandler.Ping()
    {
        yield return "foo";
        yield return "bar";
    }
}
```
[code](https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/Stream/Without.cs)

# References

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/22)



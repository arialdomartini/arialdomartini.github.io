---
layout: post
title: "Without MediatR"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- C#
- MediatR
most_read: true
---
This is an appendix of [You probably don't need MediatR][you-probably-dont-need-mediatr]: it offers practical guidance to implement the MediatR's functionalities using a plain object-oriented approach.

It goes through all the examples in the MediatR readme file and all the snippets in the 'samples' project: for each of them, it provides an implementation based on OOP standard patterns, together with some design considerations.
<!--more-->

All the code mentioned hereinafter is available in the [without-mediatr][without-mediatr-repo] repository.<br/>

# TL; DR
* A simple handler implementing a custom, domain-based interface already covers the majority of the MediatR's functionalities (Request/response, void returns, Streams and AsyncEnumerables, Notifications, polymorphic dispatch and async)

```csharp
interface IMyHandler
{
    string Echo(string message, int whatever);
}

class MyHandler : IMyHandler
{
    string IMyHandler.Echo(string message, int whatever) => "do work";
}
```

* Other functionalities such as notifications and pipelines are easily implemented with the [Composite Pattern](composite-pattern).

* As surprising it might seem, this is really all that's necessary.<br/>
Not only does this cover all of the MediatR functionalities, but also some not possible with MediatR, such as pipelines for notifications, handling of multiple requests and sending requests to multiple handlers.

* A plain OOP approach typically demonstrates better design qualities compared to one built with MediatR.


# Table of contents
* Request/response
  * [Request/response](without-mediatr-request-response)
  * [Without returning any value](without-mediatr-request-response-not-returning-a-value)
  * [Registration of multiple handlers](without-mediatr-request-response-multiple-registration)
  * Sending the same request to multiple handlers
	  * [Fire and forget](without-mediatr-request-response-multiple-handlers)
      * [Collecting the return values](without-mediatr-request-response-multiple-handlers-with-reply)
* [Streams and AsyncEnumerables](without-mediatr-streams)
* Notifications
  * [Notifications](without-mediatr-notifications)
  * [Custom notification publishers](without-mediatr-notifications-custom-notification-publisher)
* [Subtyping](without-mediatr-request-response-subtyping)



# References

* [You probably don't need MediatR][you-probably-dont-need-mediatr]
* [wihout-mediatr repository][without-mediatr-repo]
* [Composite Pattern][composite-pattern]

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/22)

[you-probably-dont-need-mediatr]: https://arialdomartini.github.io/mediatr
[without-mediatr-repo]: https://github.com/arialdomartini/without-mediatr
[composite-pattern]: https://en.wikipedia.org/wiki/Composite_pattern

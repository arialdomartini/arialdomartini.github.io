---
layout: post
title: "Without MediatR - Notifications, Custom Notification Publisher"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
include_in_index: false
tags:
- C#
- MediatR
---
# Notifications - Custom Notification Publisher
## With MediatR
When publishing notifications, it is possible to pass MediatR an instance of `INotificationPublisher` that controls the way the handlers are invoked:

```csharp

_container = new Container(cfg =>
{
    cfg.For<IMediator>().Add(s => new Mediator(s, new CustomNotificationPublisher()))
}

class CustomNotificationPublisher : INotificationPublisher
{
    async Task INotificationPublisher.Publish(IEnumerable<NotificationHandlerExecutor> handlerExecutors, INotification notification, CancellationToken cancellationToken)
    {
        foreach (var handler in handlerExecutors)
        {
            await handler.HandlerCallback(notification, cancellationToken).ConfigureAwait(false);
        }
    }
}
```
[code](https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/CustomNotificationPublisher/With.cs)

## Without MediatR
This it no special case: it's the implementation of the composite we already saw in [handling notifications without MediatR](without-mediatr-notifications).

## FAQs
### Why should the OOP implementation be preferrable?
**Answer**<br/>
A MediatR Custom Notification Publisher

* is global
* only works for Notifications, since [registering multiple request handlers is not supported](without-mediatr-request-response-multiple-registration).

With the OOP approach:

* this is no special case
* different custom notification publishers can be used for different handlers
* it works for both notifications and reqeusts.

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/22)

---
layout: post
title: "Without MediatR - Notifications"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
include_in_index: false
tags:
- C#
- MediatR
---
# Notifications
## With MediatR
From the implementation point of view, publishing a notification is pretty similar to sending a Request:

```csharp
public record PingNotification : INotification;

public class Pong : INotificationHandler<PingNotification>
{
    public Task Handle(PingNotification request, CancellationToken cancellationToken)
    {
        // do work
         return Task.CompletedTask;
    }
}

await mediator.Publish(new PingNotification());
```
[code](https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/Notification/With.cs)

Compared to Request/response, there are some crucial differences:


| Use case                     | Notification handling                                          | Request handling                                                                                           |
|------------------------------|----------------------------------------------------------------|------------------------------------------------------------------------------------------------------------|
| No handlers registered       | Published notifications are not delivered                      | Sending requests raise an exception                                                                        |
| Multiple registered handlers | Notifications are delivered to all the registered handers      | [Any extra registered handler is silently ignored](without-mediatr-request-response-multiple-registration) |

## Without MediatR
Publishing a notification is no different than [sending a request to multiple handlers in a fire and forget fashion](without-mediatr-request-response-multiple-handlers).

Given a handler interface, you can define how many handlers you desire:

```csharp
internal interface IPingNotificationHandler
{
    Task NotifyPing();
}

class Pong1 : IPingNotificationHandler
{
    Task IPingNotificationHandler.NotifyPing()
    {
	    // do work
        return Task.CompletedTask;
    }
}

class Pong2 : IPingNotificationHandler
{
    Task IPingNotificationHandler.NotifyPing()
    {
	    // do work
        return Task.CompletedTask;
    }
}
```

A simple `_handlers.ToList().ForEach(h => h.NotifyPing())` would dispatch the notification to all instances. This can be combined with the [GoF Composite Pattern][composite-pattern], so for the client a collection of handlers would look like a single instance:

```csharp
class PingNotificationComposite : IPingNotificationHandler
{
    private readonly IEnumerable<IPingNotificationHandler> _handlers;

    public PingNotificationComposite(IEnumerable<IPingNotificationHandler> handlers)
    {
        _handlers = handlers;
    }

    Task IPingNotificationHandler.NotifyPing()
    {
        _handlers.ToList().ForEach(h => h.NotifyPing());

        return Task.CompletedTask;
    }
}

class Client
{
    private readonly IPingNotificationHandler _handler;

    internal Client(IPingNotificationHandler handler)
    {
        _handler = handler;
    }

    internal void DoWork()
    {
        _handler.NotifyPing();
    }
}
```
[code](https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/Notification/Without.cs)

For the sake of completeness, here are two additional implementations. However, the one presented above is likely the simplest and is therefore recommended.

### With Observer Pattern
A classical approach is to implement the [Observer Pattern][observer-pattern], which is natively supported by .NET, although a bit ceremonius.

The pattern is trivial:  Observers subscribe to a Subject (the observed instance); this, in turn would take care of cycling through the subscribed instances.

```csharp
ISubject subject = new Subject();
subject.Subscribe(new Pong1());
subject.Subscribe(new Pong2());

subject.SendMessage(new Ping(Message: "some message"));
```

The Observed object is simply:

```csharp
class Subject : ISubject
{
    private readonly ISet<IObserver<Ping>> _observers = new HashSet<IObserver<Ping>>();

    IDisposable IObservable<Ping>.Subscribe(IObserver<Ping> observer)
    {
        _observers.Add(observer);

        return new Unsubscriber(observer, _observers);
    }

    void ISubject.SendMessage(Ping message)
    {
        foreach (var observer in _observers)
        {
            observer.OnNext(message);
        }
    }
}
```
[code](https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/Notification/Observer/Without.cs)

### With Events
[Events][events] are another classical option, natively implemented in C#, for subscribing instances to notifications. Events are an implementation of the Observer Pattern, so it is no surprise that the implementation is almost the same of `PingNotificationComposite`:


```csharp
class Pong1
{
    internal string Received { get; private set; }

    internal void Notify(object? sender, Ping ping)
    {
        Received = $"Pong1 received {ping.Message}";
    }
}

class Pong2
{
    internal string Received { get; private set; }

    internal void Notify(object? sender, Ping ping)
    {
        Received = $"Pong2 received {ping.Message}";
    }
}

class Subject : ISubject
{
    internal event EventHandler<Ping> Handlers;

    void ISubject.Subscribe(EventHandler<Ping> zop)
    {
        Handlers += zop;
    }

    void ISubject.SendMessage(Ping message)
    {
        Handlers.Invoke(this, message);
    }
}


var pong1 = new Pong1();
var pong2 = new Pong2();
	
ISubject subject = new Subject();
subject.Subscribe(pong1.Notify);
subject.Subscribe(pong2.Notify);

subject.SendMessage(new Ping("some message"));
```
[code](https://github.com/arialdomartini/without-mediatr/blob/master/src/WithoutMediatR/Notification/Events/Without.cs)


# References
* [Composite Pattern][composite-pattern]
* [Observer Pattern][observer-pattern]
* [C# Events][events]

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/22)

[composite-pattern]: https://en.wikipedia.org/wiki/Composite_pattern
[observer-pattern]: https://en.wikipedia.org/wiki/Observer_pattern
[events]: https://learn.microsoft.com/en-us/dotnet/csharp/programming-guide/events/

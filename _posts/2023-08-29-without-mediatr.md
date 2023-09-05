---
layout: post
title: "Without MediatR"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- C#
- MediatR
most_read: true
---
This is an extension of [You probably don't need MediatR][you-probably-dont-need-mediatr]: it offers practical guidance for those looking to implement all of MediatR's functionalities using a plain object-oriented or functional approach.

It goes through all the examples in the MediatR readme file and all the snippets in the 'samples' project: for each of them, it provides an implementation based on OOP standard patterns, together with some design considerations.
<!--more-->

All the code mentioned below is available in the [without-mediatr][without-mediatr-repo] repository.<br/>

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

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/22)

[you-probably-dont-need-mediatr]: https://arialdomartini.github.io/mediatr
[without-mediatr-repo]: https://github.com/arialdomartini/without-mediatr

---
layout: post
title: "State Monad For The Rest Of Us"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- F#
- Functional Programming
most_read: true
---
<!--more-->
# Table of chapters
If you are completely new to Monads, the ticket to get you in the loop
could be the gentle introduction of [Monads for the Rest of
Us][monads-for-the-rest-of-us] which uses C# examples. This series,
instead, uses F# and is only slightly more challenging.  
Nonetheless, no previous knowledge of F# is needed, neither is
understanding of monads. In fact, the series starts from the scratch
and builds an intuition on State Monad from the very ground up.

## Summary
Here's a summary of what you will find:

* First, you build a simple, recursive algorythm on a
  recursive data structure modeling a tree.
* Trying to calculate something different on the same tree,
  you discover a pattern: the code of the new solution code uses the
  very same idea of the previos solution. 
* Easy peasy, but along the way, you discover you have invented
  Functors.
* You realize that Functors are not boxes but function lifters, and
  they are not hard at all.
* Armed with your best intentions, you try to solve a seemingly
  similar problem reusing the idea of Functors, but you hit a brick
  wall: Functors are stateless, and your problem is stateful.
* You solve the problem violating Functional Programming, and the
  result is embarassingly easy.
* You want to solve the problem in a purely functional way too. You
  make it, but the resulting code is convoluted and will not scale.
* You try to separate the pure logic from the state management and you
  invent Applicative Functors.
* You repeat the exercise in a slightly different way and you invent
  Monads. It's cool, but you are not particularly proud of the syntax.
* You discover do-notation / Computation Expressions, and you get the
  best of 2 worlds: the code is pure, yet it has an imperative syntax.


Enjoy the journey!


* [Chapter 1](state-monad-for-the-rest-of-us-1): in which you play with recursive types and recursive functions.
* [Chapter 2](state-monad-for-the-rest-of-us-2): in which you invent Functors.

# References

* [State Monad For The Rest Of Us - source code][source-code]
* [Arialdo Martini - Monads for the Rest of Us][monads-for-the-rest-of-us]

[source-code]: https://github.com/arialdomartini/state-monad-for-the-rest-of-us
[monads-for-the-rest-of-us]: https://arialdomartini.github.io/monads-for-the-rest-of-us

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/30)

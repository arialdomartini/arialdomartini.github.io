---
layout: post
title: "State Monad For The Rest Of Us"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- F#
- Functional Programming
include_in_index: true
most_read: false
ftrou: true
---
Distilling the State Monad from the very ground up. In F#, no prior knowledge needed.
<!--more-->

If you are completely new to Monads, the ticket to get you in the loop
could be the gentle introduction of [Monads for the Rest of
Us][monads-for-the-rest-of-us] which uses C# examples. This series,
instead, uses F# and is only slightly more challenging. Nonetheless,
no previous knowledge of F# is needed, neither is understanding of
monads. In fact, the series starts from the scratch and builds an
intuition on State Monad from the very ground up.

Important note: *using* a state monad is amazingly easy; *building*
one from the scratch is a bit more challenging. Part I and II are
about building it. Part III is way easier, since it is about just
using an already existing implementation..

## Table of chapters

### Part I
In which you solve a stateful problem with pure functions.

* [Chapter 1](state-monad-for-the-rest-of-us-1): in which you play
  with recursive types and recursive functions.
* [Chapter 2](state-monad-for-the-rest-of-us-2): in which you find
  that binary trees have no more secrets for you.
* [Chapter 3](state-monad-for-the-rest-of-us-3): in which you invent
  Functors.
* [Chapter 4](state-monad-for-the-rest-of-us-4): in which you hit a
  brick wall.
* [Chapter 5](state-monad-for-the-rest-of-us-5): in which you face an
  existential crossroad.
* [Chapter 6](state-monad-for-the-rest-of-us-6): in which you discover
  that Procrastination&reg; is The Functional Solution to all problems.

### Part II
In which you develop the State Monad.

* [Chapter 7](state-monad-for-the-rest-of-us-7): in which you use a
  bit of type modeling to hide the state handling logic.
* [Chapter 8](state-monad-for-the-rest-of-us-8): in which you discover
  Applicative Functors.
* [Chapter 9](state-monad-for-the-rest-of-us-9): in which you learn to
  manipulate the state like it was mutable.
* [Chapter 10](state-monad-for-the-rest-of-us-10): a short one in
  which you play with signatures.
* [Chapter 11](state-monad-for-the-rest-of-us-11): in which you
  finally give birth to a State Monad.
* [Chapter 12](state-monad-for-the-rest-of-us-12): in which you
  see LINQ for what it is: a monadic engine.

### Part III
In which you play with some State Monad use cases (and you see how
easy they are)

Enjoy the journey!

## References

* [State Monad For The Rest Of Us - source code][source-code]
* [Arialdo Martini - Monads for the Rest of Us][monads-for-the-rest-of-us]

[source-code]: https://github.com/arialdomartini/state-monad-for-the-rest-of-us
[monads-for-the-rest-of-us]: https://arialdomartini.github.io/monads-for-the-rest-of-us

## Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/30)


{% include fp-newsletter.html %}

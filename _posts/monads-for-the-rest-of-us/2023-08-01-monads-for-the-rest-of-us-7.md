---
layout: post
title: "Monads for the rest of us, in C# - Part 6"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- C#
- Functional Programming
include_in_index: false
---
** In which you realize that Functors are not containers **

If you want to understand Functors, it help to develop a little different intuition on Monads. And in order to do so, it helps to go back to the roots. Don't worry, it will take only few minutes.

Imagine you have 2 ordinary functions:

```haskell
f :: A -> B
g :: B -> C
```

![2 type-compatible ordinary functions](static/img/nond-for-the-rest-of-us/ordinary-functions-2-functions.png)

You know that they are type compatible: the output of `f` has the same type of the input of `g`. So you can both:

* *apply* `f` to the output of `g`
* *compose* `f` and `g` and build a new function `f . g`

![2 type-compatible ordinary functions](static/img/nond-for-the-rest-of-us/ordinary-functions-2-functions-composed.png)


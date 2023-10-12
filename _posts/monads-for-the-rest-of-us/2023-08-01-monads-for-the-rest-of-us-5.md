---
layout: post
title: "Monads for the rest of us, in C# - Part 5"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- C#
- Functional Programming
include_in_index: false
---
I hope you enjoyed your icecream.  
But let's pick up where we left off. We stated that composing monadic functions can be built on top of `Apply` &mdash; or `bind`, `>>=`, `SelectMany`: call it as you like. Time to put that in practice.


# What about the other monads?
What you have obtained with the IO monad can be easily done with other kinds of side-effect. Let's see some examples.



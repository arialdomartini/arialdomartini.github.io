---
layout: post
title: "Monads for the rest of us, in C# - Part 6"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- C#
- Functional Programming
include_in_index: false
---
**In which you discover that Bind is a combinator and you feel illuminated**

# Bind as a combinator
If you want to understand Functors, it help to develop a little different intuition on Monads. And in order to do so, it helps to go back to the roots.  
Don't worry, it will take only few seconds.

Imagine you have 2 ordinary functions:

```haskell
f :: A -> B
g :: B -> C
```

![2 type-compatible ordinary functions](static/img/nond-for-the-rest-of-us/ordinary-functions-2-functions.png)

You know that they are type-compatible: the output of `f` has the same type of the input of `g`. So you can both:

* *apply* `f` to the output of `g`
* *compose* `f` and `g` and build a new function `g . f`

![2 type-compatible ordinary functions composed](static/img/nond-for-the-rest-of-us/ordinary-functions-2-functions-composed.png)

Ok, we are done with the roots. I promised it would be fast.

Let's see what happens with monadic functions. Suppose you have:

```haskell
f :: A -> Monad<B>
g :: B -> Monad<C>
```

![2 monadic functions](static/img/nond-for-the-rest-of-us/monadic-functions-2-functions.png)

They are not type-compatible: the output of `f` has a different type than the type requested by `g`. You cannot just feed `g` with the output of `f`:

![2 monadic functions cannot be directly bound](static/img/nond-for-the-rest-of-us/monadic-functions-2-functions-cannot-be-bound.png)

Your problem is: you would like to feed `f` with a value of `A`, but you don't have it; instead, you have a value of `Monad<A>`.  

You also know that the solution is the function `bind`.

```csharp
Monad<B> Bind<A, B>(Func<A, Monad<B>> f, Monad<A> a) => ...
```

that is, a function with the signature:

```haskell
//             f            input   =>  output
bind :: (A -> Monad<B>) -> Monad<A> -> Monad<B>
```

Here's the little new intuition you have to develop: instead of seeing to `bind` as the function to feed `f` with `input` to get an `output`, that is, as the monadic version of `Apply`, imagine surronding the last 2 elements in parenthesis:


```haskell
//             f        => (  input   -> output   )
bind :: (A -> Monad<B>) -> ( Monad<A> -> Monad<B> )
```

That is: imagine `bind` as that function that, given a function:

```haskell
f :: A -> Monad<B>
```

transforms it so its type signature becomes:

```haskell
f' :: Monad<A> -> Monad<B>
```

You like `f'` a lot! Remember your problem?

| You would like to feed `f` with a value of `A`
| but you don't have it; 
| instead, you have a value of `Monad<A>`.

That's perfect: the function `bind` gave you back wants a `Monad<A>` as input. `bind` solved your problem.

Graphically:

you start from a monadic function `f`:

![a monadic function f from A to Monad<B>](static/img/nond-for-the-rest-of-us/monadic-functions-before-bind.png)

and this is what you get applying `bind`:

![a monadic function f from Monad<A> to Monad<B>](static/img/nond-for-the-rest-of-us/monadic-functions-after-bind.png)

Here's the gist. `bind` transforms a series of not type-compatible monadic functions:

![a monadic function f from Monad<A> to Monad<B>](static/img/nond-for-the-rest-of-us/monadic-functions-series-of-functions.png)

lifting their input types so that they can be *bound* and composed together:

![a monadic function f from Monad<A> to Monad<B>](static/img/nond-for-the-rest-of-us/monadic-functions-series-of-bound-functions.png)

In other words. There are benefits in working in the monadic world. `bind` takes those function that are still with one of their legs in the ordinary world, and elevate them.

Completing this with `Return` and `Run`, it's intuitive to think that the core pattern adopted in programming functionally with monads is the following:

![return + bound functions + run](static/img/nond-for-the-rest-of-us/functional-programming-with-monads.png)

* You start by elevating your input value to the monadic world with `Return`
* You process it with a series of monadic functions, bound with `Bind`
* And finally, at the edge of your application, you descend down to the world of ordinary functions and values, with `Run`


You are ready to understand Functors.

Proceed to [Chapter 8](monads-for-the-rest-of-us-8).

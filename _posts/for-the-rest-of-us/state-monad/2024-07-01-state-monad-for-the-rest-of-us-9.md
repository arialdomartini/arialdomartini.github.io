---
layout: post
title: "State Monad For The Rest Of Us - Part 9"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- F#
- Functional Programming
include_in_index: false
---
[Index](state-monad-for-the-rest-of-us)  
Source code:
[github.com/arialdomartini/state-monad-for-the-rest-of-us][source-code].

We would like the `Leaf` branch to look more or less like this:

```fsharp
    let leaf = Leaf (v, count)
    incrementCount
    leaf
```

The challenge is that `incrementCount`. Remember that this is the
starting point we obtained in [Chapter
8](state-monad-for-the-rest-of-us-8):

```fsharp
let rec index =
    function
    | Leaf v ->
        WithCount (fun count -> (Leaf (v, count), count + 1))
    | Node (l, r) ->
        pure' build <*> index l <*> index r
```

It is worth to repeat that the `Leaf` branch does not directly return
a `Leaf` instance: instead, it returns a `Int -> (a, Int)` function
wrapped in a `WithCount`. Most likely, also `incrementCount` needs to
deal with a `WithCount`.


## Manipulating the state
Remember how we tackled this with the `Node` branch? We did 2
observations that lead us untangle the yarn:

1. We made more apparent that the branch was invoking a function,
   creating a function `buildNode`.
2. We immediately put `buildNode` in a `WithCount`.

Let's do the same. Make explicit that you are invoking a function:

```fsharp
// v -> Int -> Leaf (v, Int)
let buildLeaf v count = Leaf(v, count)
```

Then, start with moving it inside a `WithCount`. You can reuse the
`pure'` function:

```fsharp
let rec index<'a> =
    function
    | Leaf v ->
        pure' buildLeaf ...
```

As it happened before, the signature of the function is now surrounded
by a `WithCount` and cannot be invoked directly anymore &mdash; in
this case, `pure' buildLeaf` has the signature `WithCount (v -> Leaf
(v, Int))`. As before, you can use the super-function application
`<*>`. Passing `v` is simple, as soon as it is *lifted* in a
`WithCount`:

```fsharp
let rec index<'a> =
    function
    | Leaf v ->
        pure' buildLeaf <*> pure' v ...
```

Second, you need to pass a value of `count`. But, wait... There is no
`count` around to be seen. You cannot pull it out of a hat, right?  
Yes, you can! Because your code *is* running inside a hat. Let me
repeat it the last time: the `Leaf` branch does not directly return a
`Leaf` instance: instead, it returns a `Int -> (a, Int)` function
wrapped in a `WithCount`. Your hat is `WithCount`.

Here I ask you to wide open your eyes and your mind, because that
could be the inflection point where you build an intuition about the
State Monad. Compare:


| Function |
|-----------------------------------|
| `buildLeaf v ...` |
| `pure' buildLeaf <*> pure' v ...` |


`buildLeaf` is an ordinary function, and when you invoke it, you pass
it an ordinary value `v`.  
`pure' buildLeaf` is wrapped in a `WithCount`. And you remember that a
`WithCount` is like a promise. It contains a `Int -> (v, Int)`
function. It is not a value: it *will eventually* result in a value,
as soon as someone provides it with a value of `count`, through the
`run` function. The mechanism revolves around deferring an execution,
waiting for returning a value to the moment a `count` is available.

So, if you can directly provide a value of `v` to `buildLeaf`, what
can you provide to `pure' buildLeaf`? A `pure' v`. Which, in turn, *is
not* a value. It is itself a promise, a *function* eventually
returning a `v`, as soon as `count` is provided. In fact, `pure' v`
is:

```fsharp
let pure' v = WithCount (fun count -> (v, count))
```

Here's the trick: wherever you see a `WithCount` or a `pure'`, don't
think to values, think to functions from `count` to something.

## Pull state out of a hat
Back to your problem:

```fsharp
let rec index<'a> =
    function
    | Leaf v ->
        pure' buildLeaf <*> pure' v ...
```

You don't need to pass `count` to `pure' buildLeaf`: you need to pass
*a function that eventually returns `count`*. As, usual, this function
needs to be wrapped in the ordinary hat `WithCount`. Let's call it
`getCount`:

```fsharp
// WithCount Int
let getCount = WithCount (fun count -> (???, ???))
```

The first `???` is the value you want to return &mdash; that is,
`count`. The second `???` is the updated value of `count`, if you want
to change it. Do you want to change it? Of course no, there is no
need. So:

```fsharp
let getCount = WithCount (fun count -> (count, count))
```

Look! This perfectly matches the `get` function of the Haskell's `get`
function:

```haskell
get = state $ \ s -> (s, s)
```

Observe it again: it's a function (inside a `WithCount`), but, in
applicative and monadic contexts, you pratically use it as a value.
Playing with functors, applicative and monads is often akin to playing
with Quantum Mechanics: things have a dual nature of values and
functions. In an Applicative context, you can treat `getCount` as *the
value of `count`*:


```fsharp
let rec index<'a> =
    function
    | Leaf v ->
        pure' buildLeaf <*> pure' v <*> getCount
```

Cool. This looks like reading `count` from a global variable, like a
horrible Service Locator. Instead, it is purely functional, completely
immutable, absolutely referential tranparent, perfectly constant.

## Discarding values

You are almost done. This line is returning an indexed `Leaf`,
alongside with the original value of `count`. Afterall, you haven't
incremented it yet. Now it's timee to distill a functional version of
`count = count + 1`.  
Along the lines of `getCount`:

```fsharp
let getCount = WithCount (fun count -> (count, count))
```

could you imagine a similar `incrementCount`? It could be something
like:


```fsharp
// WithCount ???
let incrementCount = WithCount (fun count -> (???, count + 1))
```

Fine. This increments the current value of `count`. But what should it
return? One reasonable approach is to return the previous value:

```fsharp
// WithCount Int
let incrementCount = WithCount (fun count -> (count, count + 1))
```

or even the last updated one:

```fsharp
// WithCount Int
let incrementCount = WithCount (fun count -> (count + 1, count + 1))
```

A better approach is to apply Command Query Separation ([CQS][cqs]):
every function should either be a command that performs an action, or
a query that returns data to the caller, but not both.
`incrementCount` is performing an increment: better not returning any
value back.  
The standard value, transporting usable information, you can return is
unit, the empty tuple `()`:


```fsharp
// WithCount ()
let incrementCount = WithCount (fun count -> ((), count + 1))
```

OK. Just put it after the other arguments:

```fsharp
let rec index<'a> =
    function
    | Leaf v ->
        pure' buildLeaf <*> pure' v <*> getCount <*> incrementCount
```

Does it make sense? Almost, but not quite. In fact, it does not even
compile. The problem is, we are providing a 3rd argument to a
2-parameter function. Look. `buildLeaf` takes 2 values:

```fsharp
// 1    2      returned value
// v -> Int -> Leaf (v, Int)
let buildLeaf v count = Leaf(v, count)
```

Analogously, `pure' buildLeaf` does:

```fsharp
//                               1    2      returned value
// pure' buildLeaf :: WithCount (v -> Int -> Leaf (v, Int))
```

By now, you should have learnt to see `<*>` as a fancy way to provide
a `WithCount`-wrapped argument to a `WithCount`-wrapped function. The
3rd parameter is just not there.  
There are 2 possible approaches:

1. You intentionally add a 3rd parameter to `buildLeaf`, only to make
   the compiler happy. Then you ignore it.
2. You create a different version of `<*>` which still takes care of
   the `count`-handling logic, but does not try to apply an argument
   to the function.

Let's see both.

### Unused parameter
Instead of:


```fsharp
// v -> Int -> Leaf (v, Int)
let buildLeaf v count = Leaf(v, count)
```

you can conceive:


```fsharp
// v -> Int -> () -> Leaf (v, Int)
let buildLeaf v count _ = Leaf(v, count)
```

The extra `()` ignored parameter does not alter in any way the logic,
but it forces callers to provide an extra parameter. Try this and
convince yourself that it does the trick:

```fsharp
let rec index<'a> =
    function
    | Leaf v ->
        pure' buildLeaf' <*> pure' v <*> getCount <*> incrementCount

let rec index'<'a> =
    function
    | Leaf v -> pure' buildLeaf' <*> pure' v <*> getCount <*> incrementCount
    | Node(l, r) -> pure' buildNode <*> index l <*> index r

```

### Apply and Discard Right
The second option is to create a version of `<*>` that does not pass
its value to a function, thus avoiding the consumption of 1 function
parameter. Reviewing the `<*>` implementation:

// WithCount (a -> b) -> WithCount a -> WithCount b
```fsharp
let (<*>) f a =
    WithCount(fun count ->
        let fv, fc = run f count
        let av, ac = run a fc
        let b = fv av
        (b, ac))
```

it should be apparent that what it performs can be summarized as:

1. It runs its 1st parameter, the wrapped function.
2. It runs its 2nd parameter, the wrapped original value.
3. It applies the 2nd unwrapped-result to the 1st unwrapped-result.
4. It returns the obtained result, with the updated count.

A version of `<*>` not consuming a function parameter should just skip
the step `3`, and most likely returning the original function. Let's
call it `<*`:


```fsharp
let (<*) f v =
    WithCount(fun count ->
        let fv, fc = run f count
        let _, newCount = run v fc
		// No need to invoke fv here
        (fv, newCount))  // returning the original function, unchanged
```

Notice that in `4`, `<*>` returns the original function `f` partially
applied, with `1` parameter consumed. On the contrary, `<*` returns
the function `f` unchanged: it does not consume its parameters.

Keeping all together:


```fsharp
let rec index<'a> =
    function
    | Leaf v ->
        pure' buildLeaf <*> pure' v <*> getCount <* incrementCount
    | Node(l, r) -> 
	    pure' buildNode <*> index l <*> index r
```

You can read the `... <* incrementCount` as "also execute
`incrementCount`, but discard its returned value". It discards the
right value. As a mnemonic: the symbol we chose, `<*` lacks the right
`>`, signaling that it discards whatever value is returned by the
right expression.

Try it: the types match, the compiler pleased nods, the test is so
green. Cool.

(If you think that there is some duplication in the the implementation
of `<*` and `<*>`, you are right: in fact, `<*` can be derived
directly from `<*>`).

## Wait, discard what?
The first time I stumbled upon `<*` I was so confused. What does
*discarding* a value really means?

In a context where things are not pure, invoking a function and
ignoring the result means being interested in the side effects only:

```fsharp
let _ = apiClient.LaunchTheMissiles()
```

In a context where functions are completely devoid of side effect,
where all they can do is returning a value, ignoring a value really
means doing nothing:

```fsharp
let _ = add 2 3
```

So, isn't:

```fsharp
pure' buildLeaf <*> pure' v <*> getCount <* incrementCount
```

completely equivalent to:

```fsharp
pure' buildLeaf <*> pure' v <*> getCount
```

where `incrementCount` is removed? After all, its value is ignored.

The answer is: no, they are not equivalent at all. The fact is, you
are not playing with the ordinary function application, but with a
special version of it, `<*>`, which does 2 things:

1. It passes a value as an argument to a function.
2. It keeps track of the extra, implicit `count` argument.

Read again the implementation of `<*`: it skips `1`, but it keeps
perfoming `2`:

```fsharp
let (<*) f v =
    WithCount(fun count ->
        let fv, fc = run f count
        let _, newCount = run v fc
        (fv, newCount))  // <- it takes care of passing the
                         //    updated value of count
```

Using `<*` is similar to discarding a value in an impure world: it
means being interested only in the side-effects of the functor &mdash;
in this case, in the `count`-tracking logic &mdash; not in the pure
calculation it performs.


## OK, and the monad?
Congrats! You created a complete State Applicative Functor. You are
just that close to the State Monad, and the last mile should not be
challenging at all.

Let's take a last, little detour to speculate on the signature, which
will bring us even closer. Then, we will develop the final State
Monad. See you in [Chapter 10](state-monad-for-the-rest-of-us-10).

# References
* [State Monad For The Rest Of Us - source code][source-code]
* [Command Query Separation - Wikipedia][cqs]
  
# Comments
[GitHub Discussions][discussions]

[source-code]: https://github.com/arialdomartini/state-monad-for-the-rest-of-us
[cqs]: https://en.wikipedia.org/wiki/Command%E2%80%93query_separation
[discussions]: https://github.com/arialdomartini/arialdomartini.github.io/discussions/30



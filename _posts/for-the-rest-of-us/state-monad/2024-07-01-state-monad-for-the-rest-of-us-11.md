---
layout: post
title: "State Monad For The Rest Of Us - Part 7"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- F#
- Functional Programming
include_in_index: false
---
Source code:
[github.com/arialdomartini/state-monad-for-the-rest-of-us][source-code].

[Two chapters ago](state-monad-for-the-rest-of-us-9), you wrote the
applicative version of `index` using `<*>` and `<*`. If you remember,
in [chapter 8](state-monad-for-the-rest-of-us-8) we said that there
are 2 different series of moves:

* One leading to Applicative Functors.
* One leading to Monads.

You performed the former, which were:

1. Make the fact that each branch is invoking a function explicit.
2. Start by moving that function inside a `WithCount`.

Now we try another approach:

1. As before, make the fact each branch is invoking a function
   explicit.
2. Start by letting that function return a `WithCount` value.

Let's follow this path.

## Make Monads, not War
This is the starting point:

```fsharp
let rec index =
    function
    | Leaf v ->
        WithCount(fun count ->
            (buildLeaf v count, count + 1))
    | Node(l, r) ->
        WithCount(fun count ->
            let li, lc = run (index l) count
            let ri, rc = run (index r) lc
            (buildNode li ri), rc)
```

Focusing on the `Node` branch, the function we want to modify is
`buildNode`. So far, it returns a `Node`: let it return a `WithCount`
instead:

```fsharp

let rec index =
    function
    | Node(l, r) ->
	    // Tree a -> Tree a -> WithCount (Tree a)
        let buildNode' l r = pure' (buildNode l r)
```

The problem now is how to feed it with `WithCount` values. In fact,
once recursing on `index` on `l` and `r` you get:

```fsharp
    | Node(l, r) ->
        // WithCount (Tree a)
        let li = index l

        // WithCount (Tree a)
        let ri = index r
        
        // Tree a -> Tree a -> WithCount (Tree a)
        let buildNode' l r = pure' (buildNode l r)
```

You cannot just pass `li` and `ri` to `buildNode'`: 

```fsharp
        // Tree a -> Tree a -> WithCount (Tree a)
        let buildNode' l r = pure' (buildNode l r)

        buildNode' li ri
```

the compiler would complain because `buildNode'` expects naked `Tree`
values, and you are providing promises of them. F#'s built-in function
application is not smart enough to handle `WithCount` instances. But
you can easily teach it.  
Just like you did before with the Applicative Functor, it's a matter
of implementing another on-steroids function application, one which is
able to provide `WithCount` arguments to functions expecting naked values.

## Bind
Here's the general challenge:

* You have a value `a :: Tree a`.
* And a function `f :: Tree a -> WithCount (Tree b)`.
* You cannot invoke `f a`.
* You need a `f ??? a` able to do that.

Yes, in the `Node` branch you have a `2`-parameter function, but as
usual you will find that solving the problem for `1`-parameter
functions scales to any `n`-parameter ones. The power of currying...

Implementing this on-steroids function application, that we will call
`=<<` (pronunced "reversed bind") is actually simple. It is similar to
what you have alread done in [Chapter
3](state-monad-for-the-rest-of-us-3) to implement `map`, using the box
metaphor. Be guided by types: they will lead you to the correct
implementation

```fsharp
// (a -> WithCount b) -> WithCount a -> WithCount b
let (=<<) f a =
    ???
```

If only `a` was a naked instance of `a`, you could feed it directly to
`f`. But it is not: it is a (`WithCount` wrapped) function, waiting
for a `count` to return a naked `a` instance. You could pass it a
`count` instance. Sure: but where to find it? Can you produce it one
out of thin air?  
Actually you can. Remember my trick: when you don't have a value,
defer the problem and return a function asking for it.  
Another way to see this is: consider that you will finally have to
return another `WithCount` value. So it makes sense to start building
one:

```fsharp
// (a -> WithCount b) -> WithCount a -> WithCount b
let (=<<) f a =
    WithCount ???
```

And you know that a `WithCount` contains a function from `count`:

```fsharp
// (a -> WithCount b) -> WithCount a -> WithCount b
let (=<<) f a =
    WithCount (fun count -> ???)
```

Here's the `count` value you needed. With it, you can ask `a` to
generate a naked value. How? But with `run`, the function you have
already created to run a `WithCount`:

```fsharp
// WithCount v -> Int -> v
let run (WithCount f) (count: int) = f count

// (a -> WithCount b) -> WithCount a -> WithCount b
let (=<<) f a =
    WithCount (fun count ->
	    // (a, Int)
        let va, ca = run v count
		???)
```

`va` is the naked `a` value; `ca` is the count. Perfect: you can
finally feed `f`:

```fsharp
// (a -> WithCount b) -> WithCount a -> WithCount b
let (=<<) f a =
    WithCount(fun count ->
        let va, ca = run a count
        let result = f vv
```

Which type is `result`? It matches the return value of the `f`
signature, so it is a `WithCount b`. Your code already lives inside a
`WithCount`, you don't need to nest another one. You know that in
order to extract a naked value from a `WithCount`, you can run it
providing it a value of `count`. In this case, you have a freshly
calculated, unused `ca`:

```fsharp
// (a -> WithCount b) -> WithCount a -> WithCount b
let (=<<) f a =
    WithCount(fun count ->
        let va, ca = run a count
        let result = f vv
		run result ca)
```

Since the name of `=<<` is "reversed bind", you might have guessed
that a forward-version also exist. In fact, as you will soon find out,
is convenient to flip the arguments. Let's define a specular `>>=`
operator, called "bind". Just flip the parameters, re-using the very
same implementation:

```fsharp
// WithCount a -> (a -> WithCount b) -> WithCount b
let (>>=) a f =
    WithCount(fun count ->
        let va, ca = run a count
        let result = f va
        run result ca)
```

Of course, since you abhor copy-pasting, you can re-define `=<<` in
terms of `>>=` just flippling the parameters:

```fsharp
// (a -> WithCount b) -> WithCount a -> WithCount b
let (=<<) a b = b (>>=) a
```

Let's put this bind operator at work!

## Node branch with `>>=`
You have all the ingredients: `li`, the indexed left branch, which is
a `WithCount Tree a` value, and `buildNode'`, which expects a naked
`Tree a` value. Feed it with `>>=`:

```fsharp
let rec index =
    function
    | Node(l, r) ->
        // WithCount (Tree a)
        let li = index l

        // WithCount (Tree a)
        let ri = index r
        
        // Tree a -> Tree a -> WithCount (Tree a)
        let buildNode' l r = pure' (buildNode l r)

        li >>= ???
```

Read the `>>=` signature again:

```haskell
// WithCount a -> (a -> WithCount b) -> WithCount b
let (>>=) a f = ...
```

On the right, `>>=` takes a function from `Tree a`.


```fsharp
let rec index =
    function
    | Node(l, r) ->
        // WithCount (Tree a)
        let li = index l
        
        ...

        li >>= (fun ll -> ???)
```

`ll` is the naked value of the indexed left branch.

Here's the trick to master `>>=`. Focus on the types:

```fsharp
    //  WithCount Tree a           Tree a  -> ...
        li                >>= (fun ll      -> ???)
```

Interpret it as follows: 

* You start from a `WithCount` value.
* After `fun` you continue writing your code as `WithCount` didn't
exist. Your lambda operates on a naked value.

This makes sense: `>>=` is the on-steroids function application that
lets you think in terms of simple values, taking care of performing
all the logic related to the `count`-handling. Since the
`count`-handling logic is modeled with a `WithCount`, it makes sense
that `>>=` lets you ignore it.

In other words, `>>=` *unwraps* any `WithCount` value. Let me rephrase
the trick:

* Every time you have a `WithCount a` value, feed it to `>>=`.
* Then keep coding a lambda operating on `a` only.

Given this rule of thumb, implementing the `Node` branch should be
trivial. You just have to continue unwrapping `ri = index r` too:

```fsharp
let rec index =
    function
    | Node(l, r) ->
        let li = index l
        let ri = index r
        let buildNode' l r = pure' (buildNode l r)

        li >>= (fun ll -> ri >>= (fun rr -> ???))
```

Cool. You have `ll` and `rr` to feed `buildNode'` with:


```fsharp
let rec index =
    function
    | Node(l, r) ->
        let li = index l
        let ri = index r
        let buildNode' l r = pure' (buildNode l r)

        li >>= (fun ll -> ri >>= (fun rr -> buildNode' ll rr))
```

Believe it or not, you are done.

## Leaf branch
For the Leaf branch, you need to perform 3 actions:

* Read the current value of `count` using `getCount`.
* Return a `Leaf v, count`.
* Update `count` with an incremented value, using `putCount`.

Both `getCount` and `putCount` operate in a `WithCount` context:

```fsharp
// WithCount (Int, Int)
let getCount = WithCount(fun c -> (c, c))

// Int -> WithCount ((), Int)
let putCount c = WithCount(fun _ -> ((), c))
```

That should not be a problem by now: you know the trick now, pass
`WithCount` values to `>>=` and you get the naked value.

So, let's start from retrieving `count`:

```fsharp
let rec index =
    function
    | Leaf (v: string) ->
        getCount
        >>= (fun count -> ...
```

You have both `v` and `count`. You can already build the return value:

```fsharp
let rec index =
    function
    | Leaf(v: string) ->
        getCount
        >>= (fun count ->
            let leaf = Leaf(v, count)
```

Don't return it just yet. You have to increment `count`.

```fsharp
let rec index =
    function
    | Leaf(v: string) ->
        getCount
        >>= (fun count ->
            let leaf = Leaf(v, count)
            let newCount = count + 1
            putCount newCount
            ...
```

Now, `putCount` returns a `WithCount ()`. Again, continue with `>>=`
and a lambda, and finally return the indexed leaf, inside a `WithCount`:

```fsharp
let rec index =
    function
    | Leaf(v: string) ->
        getCount
        >>= (fun count ->
            let leaf = Leaf(v, count)
            let newCount = count + 1
            putCount newCount
            >>= (fun unit -> pure' leaf))
```

Notice that as expected the `unit` returned by `putCount` is never
used. You can safely ignore replacing it with `_`. Let's put all
together:


```fsharp
let rec index =
    function
    | Leaf(v: string) ->
        getCount
        >>= (fun count ->
            let leaf = Leaf(v, count)
            putCount (count + 1)
            >>= (fun _ -> pure' leaf))
    | Node(l, r) ->
        index l >>= (fun ll ->
            index r
             >>= (fun rr ->
                 pure' (buildNode ll rr)))
```

Run the test and see it pass.

The type `WithCount`, together with `>>=` and `pure'` is what we call
a State Monad. In this implementation, it only accounts for an integer
`count`, but nothing prevents you from making it more generic.

## Do you like it?
I know you are not excited. How could you be? That series of `>>=` and
`fun s ->` makes head spin, don't they? Yes, it's true: it's stateful
code with an imperative scent. See how:

```fsharp
putCount (count + 1)
```

seems to imperatively update a global variable. I'm sure you can
follow the flow and understand it, and reading between the lines you
can see how how it looks like imperative code. Yet, I bet you would
call it all but fluent and readable.

If you are only partially satisfied with the result and you expected
the State Monad to be more magic, you are not alone. The designers of
Haskell also felt that expressions like the above could be improved
with some little syntactic sugar.

So they invented the *do notation*. Which is what you are going to
implement too in the [next
chapter](state-monad-for-the-rest-of-us-12), and which will make you
see the true nature of LINQ.


# References
* [State Monad For The Rest Of Us - source code][source-code]
  

# Comments
[GitHub Discussions][discussions]


[discussions]: https://github.com/arialdomartini/arialdomartini.github.io/discussions/30
[source-code]: https://github.com/arialdomartini/state-monad-for-the-rest-of-us

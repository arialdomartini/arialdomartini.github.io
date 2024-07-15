---
layout: post
title: "State Monad For The Rest Of Us - Part 8"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- F#
- Functional Programming
include_in_index: false
---
Source code:
[github.com/arialdomartini/state-monad-for-the-rest-of-us][source-code].

## Function application, on steroids
Here's our last working version:

```fsharp
type WithCount<'v> = WithCount of (int -> 'v * int)

let run (WithCount f) count = f count

let rec index =
    function
    | Leaf v ->
        WithCount (fun count -> (Leaf (v, count), count + 1))
    | Node (l, r) ->
         WithCount (
          fun count ->
              let li, lc = run (index l) count
              let ri, rc = run (index r) lc
              Node (li, ri), rc)
```

Focus on the `Node` branch:

```fsharp
WithCount (
 fun count ->
     let li, lc = run (index l) count
     let ri, rc = run (index r) lc
     Node (li, ri), rc)
```

Look the very last line, which creates an instance of `Node`: it might
not be immediately apparent that it is invoking a function. Let's make
it more explicit:

```fsharp
let build l r = Node(l, r)

...
WithCount(fun count ->
   let li, lc = run (index l) count
   let ri, rc = run (index r) lc
   (build li ri), rc)
```

Now, the goal is to get rid of all the count-handling code, and to be
able to just write the domain logic code:

```fsharp
let li = index l
let ri = index r
build li ri
```

or

```fsharp
build (index l) (inder r)
```

along the lines of what you did in the very first chapters. Unfortunately, this does not work as expected  
The problem is that this does not compile. The types don't match:

| Value     | Type                         |
|-----------|------------------------------|
| `build`   | `Tree a -> Tree a -> Tree a` |
| `index l` | `WithCounter (Tree a)`       |
| `index r` | `WithCounter (Tree a)`       |


You cannot apply a `WithCounter (Tree a)` value to a function
expecting `Tree a`. Function Application in F# is not compatible with
the types you are providing.

## Reimplementing the built-in Function Application
If F# Function Application is not compatible with `WithCounter`
parameters, let's reinvent Function Application.  
You can start from reimplementing the standard F# Function
Application. When you write:

```fsharp
let add a b = a + b
let result = add 2 3
```

you can imagine that the space between `add` and `2` is an elusive and
invisible operator that applies `2` to the function `add`. What would
its type be?

```haskell
(a -> b) -> a -> b
```

It takes a function `a -> b` and a parameter `a`; it applies `a` to
the function, getting back a `b`. If you wanted to implement it giving
it a more visible symbol, you could write:

```fsharp
let (<|) f a = f a 
```

so that:

```fsharp
let add a b = a + b
let result = add 2 3
```

would become:

```fsharp
let add a b = a + b
let result = add <| 2 <| 3
```

Well, you don't even have to implement `<|`, because it's natively
defined in F#.  
Getting back to:


```fsharp
build (index l) (inder r)
```

Maybe what you need is a function application on steroids able to
apply `WithCounter a` arguments to functions expecting just `a`
values:

```fsharp
build <???> index l <???> index r
```

This extended, hypothetical super-function application should take
care of handling the extra-parameter `count`, letting you focus on the
domain logic only.

Maybe you need to fill the first `<???>` and the second `<???>` with 2
different operators. Maybe there are alternative, similar forms to get
to the same result. Whatever the case, I hope you see that those
operator must have an implementation independent from the domain
logic. Therefore, it is very likely that you can distill them focusing
on the type
signatures only.  
Which is exactly what we are going to do in the next paragraph.

## `ap`, an on-steroids Function Application
There are 2 series of moves that you can do at this point.

* One leads you to Applicative Functors.
* The other, to Monads.

Let's focus on the former. Read again:

```fsharp
type WithCount<'v> = WithCount of (int -> 'v * int)

let run (WithCount f) count = f count

let rec index =
    function
    | Leaf v ->
        WithCount (fun count -> (Leaf (v, count), count + 1))
    | Node (l, r) ->
         WithCount (
          fun count ->
              let li, lc = run (index l) count
              let ri, rc = run (index r) lc
              (build li ri), rc)
```

and focus on the `Node` branch. Make an attempt to mentally remove all
the count-handling logic and focus on the domain logic only. See how
this branch returns the result of `build li ri` somehow enclosed in a
`WithCount` instance. Since you want to isolate the domain logic, we
can try to extract `build` and to put it in a `WithCount`:

```fsharp
let rec index =
    function
    | Leaf v ->
        WithCount (fun count -> (Leaf (v, count), count + 1))
    | Node (l, r) ->
        let build' = putInWithCount build
        ...
```

What does it mean to *put something* in a `WithCount`? How to
implement that `putInWithCount`?


```fsharp
let putInWithCount v = ???
```

Well, that's easy once you recall that:

```fsharp
type WithCount<'v> = WithCount of (int -> 'v * int)
```

First things first, `putInWithCount` must return a `WithCount`
instance, and `WithCount` instances are created with the `WithCount` constructor:

```fsharp
let putInWithCount v = WithCount ??? 
```

Then, look at the `WithCount` data contstuctor: it need to be fed with
a function from `Int` &mdash; the count &mdash; to a tuple `(v, Int)`,
where `v` is your value. So, you need to build an instance of a
function:


```fsharp
let putInWithCount v = WithCount (fun count -> ???)
```

Now, you have to return a tuple with your value, plus a `count`
value:

```fsharp
let putInWithCount v = WithCount (fun count -> (v, ???))
```

The lambda receives a value for `count`: which value shall it return
back? Shall it increment it? In fact, there is no reason to. It's
safer to keep it unmodified:

```fsharp
let putInWithCount v = WithCount (fun count -> (v, count))
```

Here we go! Traditionally, this function is called `pure`:

```fsharp
let pure' v = WithCount (fun count -> (v, count))

let rec index =
    function
    | Leaf v -> WithCount(fun count -> (Leaf(v, count), count + 1))
    | Node(l, r) ->
        let build' = pure' build
        ...
```

F# flags `pure` as reserved for future uses &mdash; a good sign, I
guess! &mdash; so I'm opting for `pure'`.  
What's `build'` signature? It's the signature of a `build` inside a
`WithCount`:

| Function      | Signature                                |
|---------------|------------------------------------------|
| `build`       | `Tree a -> Tree a -> Tree a`             |
| `pure' build` | `WithCount (Tree a -> Tree a -> Tree a)` |


Let's continue along this line, focus on the domain logic only:

```fsharp
let rec index =
    function
    | Leaf v -> WithCount(fun count -> (Leaf(v, count), count + 1))
    | Node(l, r) ->
        // `WithCount (Tree a -> Tree a -> Tree a)`
        let build' = pure' build
        
        // WithCount (Tree (string,int))
        let li = index l
        
        // WithCount (Tree (string,int))
        let ri = index r
```

It is simpler that it sounds:

* You have a function inside a `WithCount`.
* Its first argument, also inside a `WithCount`.
* And so its second argument.

If it wasn't for the surrounding `WithCount`, you could just apply the
2 arguments to the function, using the ordinary Function
Application. Instead, it seems that you need a special function
application, which we will call `ap` or `<*>`, matching the signature:

|           | Function Application          | ap                                                 |
|-----------|-------------------------------|----------------------------------------------------|
| Symbol    | `<|`<br/>or just a space      | `<*>`                                              |
| Signature | `(a -> b) -> a -> b`          | `WithCount (a -> b) -> WithCount a -> WithCount b` |
| Example   | `add 2 3`<br/>`add <| 2 <| 3` | `build' <*> li <*> ri`                             |

Wait a sec: isn't this the signature of a function of `1` parameter
only? Yes: but you know that multi-parameters functions are an
illusion. A function like:

```fsharp
(a -> b) -> a -> b
```

has 2 parameters as soon as `b` is itself a 1-parameter function:

```fsharp
(a -> b) -> a -> b

if b = x -> y

(a -> (x -> y)) -> a -> (x -> y)

(a -> x -> y) -> a -> x -> y
```

Find. So, let's implement `<*>`.

## Implementing `<*>`

```fsharp
// WithCount (a -> b) -> WithCount a -> WithCount b
let (<*>) f a = ???
```

`f` is of type `WithCount (a -> b)`. What does it mean? That it is
*not* a function `a -> b`. Remember what `WithCount` is:

```fsharp
type WithCount<'v> = WithCount of (int -> 'v * int)
```

`f` is an instance of a `WithCount` holding the function you want to
run. Provide `f` with a `count`, and it will give you back that
function, plus a new value of a `count`. OK, but how to provide a
value of `count`? Where to find this `count`?  
To answer this question, notice the return type of `<*>`: it'a a
`WithCount b`. So, to begin with, you have to build an instance of a `WithCount`:

```fsharp
// WithCount (a -> b) -> WithCount a -> WithCount b
let (<*>) f a = WithCount ???
```

You should know by heart now that `WithCount` constructor needs a
function asking for a `count`:

```fsharp
let (<*>) f a = WithCount (fun count -> ???)
```

Beautiful! Here is where to get the value of `count`: use it to feed
the `WithCount ()`

```fsharp
let (<*>) f a = 
    WithCount (fun count ->
        let result = run f count
        ...
```

Which kind of result did you get? What's its type? Review the `run`
signature, or the definition of `WithCount` itself: it is a tuple with
the desired result *plus* the new value of `count`. Let's deconstruct
it:

```fsharp
let (<*>) f a =
    WithCount (fun count ->
        let ff, fc = run f count
        ...
```

Here we are calling `fv` the `f`unction `v`alue of type `a -> b`, and
with `fc` the `c`ounter, generated by running the `WithCount`
instance. Just do the same with `a`:

* `a` is of type `WithCount a`, so you cannot feed it directly to the
  `a -> b` function.
* In fact, it's not an `a` value yet. It it a function that will give
  you an `a` value, as soon as you provide it a `count`.
* As before, it will provide you an `a` value *plus* the updated value
  of `count`, all in a tuple.

```fsharp
let (<*>) f a =
    WithCount (fun count ->
        let fv, fc = run f count
        let result = run a ???
```

Which `count` value should you pass? Let's use `fc`, the most updated
one jus returned by `run f count`:

```fsharp
let (<*>) f a = 
    WithCount (fun count ->
        let fv, fc = run f count
        let result = run a fc
        ...
```

Again: `result` is a tuple containing the value `av` *plus* the
updated `count` value `ac`:

```fsharp
let (<*>) f a = 
    WithCount (fun count ->
        let fv, fc = run f count
        let av, ac = run a fc
        ...
```

You got `fv :: a -> b` and `av :: a`. You can finally apply function
to the value, obtaining a `b` value:


```fsharp
let (<*>) f a = 
    WithCount (fun count ->
        let fv, fc = run f count
        let av, ac = run a fc
        let b = fv av
        ...
```


You are almost done. You just need to return the tuple containing the
obtained value `b` plus the last updated `count`, which is `ac`:

```fsharp
let (<*>) f a = 
    WithCount (fun count ->
        let fv, fc = run f count
        let av, ac = run a fc
        let b = fv av
        b, ac)
```

Putting all together:

```fsharp
let pure' v = WithCount (fun count -> (v, count))

let (<*>) f a = 
    WithCount (fun count ->
        let fv, fc = run f count
        let av, ac = run a fc
        let b = fv av
        b, ac)

 
let rec index<'a> =
    function
    | Leaf v -> ....
    | Node(l, r) ->
        let build' = pure' build
        let li = index l
        let ri = index r
        build' <*> li <*> ri
```

Inlining `build'`, `li` and `ri` you get to:

```fsharp
let rec index<'a> =
    function
    | Leaf v -> ...
    | Node(l, r) ->
        pure' build <*> index l <*> index r
```

Which is beautiful! If you ignore the little noise produced by `pure'`
and `<*>`, it has the very shape of:

```fsharp
              build    (index l)   (index r)
```

Please focus again on the `Node` branch and notice:

* This code properly handle the `count` argument.
* Yet, neither the `count` values nor the returned tuples are ever
  mentioned.
* Of course: their handling have been extracted and coded once for all
  in the `<*>` implementation.
* Besides a bit of noise because of `pure'` and `<*>`, this code is
  pure domain logic.
  
`WithCount` together with `pure'` and `ap` / `<*>` is an example of an
Applicative Functor. It's like the Functor you built in [chapter
3](state-monad-for-the-rest-of-us-3) but, as you see, more powerful.


## And the Leaf branch?
Surprisingly, this applicative style is actually trickier to implement
on the leaf branch.  
Or maybe not surprisingly: after all, we already know from [chapter
2](state-monad-for-the-rest-of-us-2), while implementing the 2 initial
algorithms:

```fsharp
let baseCase _ = 1
let baseCase' v = Leaf(String.length v)
let (^+) l r = Node (l, r)

let rec numberOfLeaves =
    function
    | Leaf v     -> baseCase v
    | Node(l, r) -> numberOfLeaves l + numberOfLeaves r

let rec lengths =
    function
    | Leaf v     -> baseCase' v
    | Node(l, r) -> lengths l ^+ lengths r
```

that

* The `Node` branch's only purpose is to perform a double recursion
  and to compose the result.
* The `Leaf` node performs the actual business logic.

Now, the actual business logic is about indexing a leaf, that is:

* Somehow acquiring the value of `count`.
* Creating a indexed `Leaf`.
* Incrementing `count`.

The `Node` branch is completely devoid of any count-handling logic:

```fsharp
| Node(l, r) ->
    pure' build <*> index l <*> index r
```

Not only does not it mention `count` in any way, but it does not even
know it is running in a context where `count` is handled. From the
`Node` branch perspective, it could be run in another monadic
context. And, as an aside, this is the beauty of monads: they all
provide the same basic interface and they completely abstract away the
effect-handling logic.

The `Leaf` branch is inherently different: although the count-handling
logic is still taken care of by `<*>`, the code must somehow
manipulate `count`. That's a novel kind of challenge. Fortunately, an
easy one, which will bring you closer to the State Monad. Something
deserving a little break and a [new chapter](state-monad-for-the-rest-of-us-9).




# References
* [State Monad For The Rest Of Us - source code][source-code]
  
[source-code]: https://github.com/arialdomartini/state-monad-for-the-rest-of-us


# Comments
[GitHub Discussions][discussions]


[discussions]: https://github.com/arialdomartini/arialdomartini.github.io/discussions/30


{% include fp-newsletter.html %}

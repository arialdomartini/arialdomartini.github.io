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

## Types are always a good idea

Enjoyed the sorbet? Gather your strengths, you will need them! These
chapters will be a bit more challenging than the ones in Part I but,
hopefully, also more rewarding.

So, we closed [chapter 6](state-monad-for-the-rest-of-us-6) with this
code:

```fsharp
// Tree a -> (Int -> (Tree (a, Int), Int))
let rec index =
    function
    | Leaf v -> fun count -> (Leaf (v, count), count + 1)
    | Node (l, r) ->
        fun count ->
            let li, lc = index l count
            let ri, rc = index r lc
            Node (li, ri), rc

[<Fact>]
let ``indexes a tree`` () =
    let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))

    let indexed, _ = index tree 1

    test <@ indexed = Node(Leaf ("one", 1), Node(Leaf ("two", 2), Leaf ("three", 3))) @>
```

and we commented that it is a pity that the code for handling the
state (the value of `count`) is interleaved with the domain logic
code. It is also a pity that the function signature &mdash;its type
&mdash; got so complicated.

Indeed, types are exactly where the key to the next step lies. You
surely remember when we enumerated the possible placements for the
`count` parameter:

| Position                            | Signature                                                                                                               |
|-------------------------------------|-------------------------------------------------------------------------------------------------------------------------|
| As the first `index` parameter      | `let rec index count tree = ...`                                                                                             |
| As the second `index` parameter     | `let rec index tree count = ...`                                                                                             |
| As the parameter of a nested lambda | `let rec index tree =` <br/>&nbsp;&nbsp;&nbsp;&nbsp;`match tree with`<br/>&nbsp;&nbsp;&nbsp;&nbsp;  `| Leaf v -> fun count -> ....` |

and you remember that we went with the 3rd option. You might have
already noticed that the last 2 are equivalent. Compare their
signatures:

| Position                            | Signature                                 |
|-------------------------------------|-------------------------------------------|
| As the second `index` parameter     | `Tree a ->  Int -> (Tree (a, Int), Int)`  |
| As the parameter of a nested lambda | `Tree a -> (Int -> (Tree (a, Int), Int))` |

The latter has a couple of extra parenthesis, which are anyway
implicit in the former.  
We preferred the latter because it makes it clear that the
function `index` is returning a kind of a promise: it does not
directly return an indexed tree `Tree (a, Int)`; instead, it returns a
function that, once fed with the value of `count`, will carry its job
out. Not only: it will return an indexed tree *plus* a new `count`
value, all in a tuple.

Wow. That's a mouthful. Even attempting to explain the signature is
utterly exhausting. It is definitely worth to simplify it.

As it often happens in Functional Programming, the first step to
elaborating an idea &mdash; to simplify it &mdash; is to give it a
name. *Giving an idea a name* rally means *to define a type* to
represent it. The more you will apply Functional Programming, the more
you will find yourself modeling your applications through types.

There are 2 ways I can suggest you to tackle this step.

* Just add another layer of indirection, and follow where it leads
  you.
* Manipulate the signature, separating the state-management and domain
  logic on the type level.
  
They are 2 sides of the same coin.

## Add another layer of indirection
Observe again `index`:

```fsharp
let rec index =
    function
    | Leaf v -> fun count -> (Leaf (v, count), count + 1)
    | Node (l, r) ->
        fun count ->
            let li, lc = index l count
            let ri, rc = index r lc
            Node (li, ri), rc
```

and notice how both the branches return a naked `fun count ->
...`. Let's pack it inside a container, which we will call
`WithCount`:

```fsharp
let rec index =
    function
    | Leaf v -> WithCount (fun count -> (Leaf (v, count), count + 1))
    | Node (l, r) ->
        WithCount (
          fun count ->
              let li, lc = index l count
              let ri, rc = index r lc
              Node (li, ri), rc)
```

Spoiler alert: `WithCount` is the type around which you will develop
the State Monad. Keep an eye on it.

`WithCount` does nothing but holding that convoluted function. In a
sense, it decorates it, it encapsulates the function and hides its
complexity. More importantly, having a name, gives us the
chance to define dedicated functions to operate on it.  
Which functions? Well, first of all, the ones we need to fix the
compilation errors. There are severals, and they revolve around 2
problems:

1. `WithCount` is not even defined.
2. `index` does not return a function anymore: its returned value
cannot be directly executed.

Let's proceed in order.

### Definging a new type
`WithCount` can be defined as:

```fsharp
type WithCount = WithCount of ???
```

Remember: the `WithCount` on the left is the type. The `WithCount` on
the right is not: it is the data constructor that can be used to build
an instance of `WithCount`. Think of it as the C# class constructor.

What to replace `???` with? Let's be lazy and let's ask F# itself. If
you extract the argument of `WithCount` to a variable, if can use the
F# type inference to ask the F# compiler its opinion:

```fsharp
let rec index =
    function
    | Leaf v ->
        let f: int -> Tree<'a * int> * int = (fun count -> (Leaf (v, count), count + 1))
        WithCount f
    ...
```

So, F# claims that the function encapsulated by `WithCount` has the type:

```haskell
Int -> (Tree (a, Int>), Int)
```

This matches our experience:

| `Int`                          | `Tree (a, Int>)` | `Int`                      |
|--------------------------------|------------------|----------------------------|
| The previous value for `count` | The indexed tree | The next value for `count` |


In fact, we could be way more generic. For the time being, let's
settle for this little generalization:

```fsharp
type WithCount<'v> = WithCount of (int -> 'v * int)
```

so `WithCount` is not specific to trees. Afterall, the logic for
traversing trees is already implemented in `index`. It might not be
immediately evident, but what this `WithCount` definition does, it to
isolate the pure, domain logic from the count-handling logic. We will
discuss this more thoroughly in [chapter xxx](state-monad-for-the-rest-of-us-xxx).

### Running `WithCount`

Your code is full of invocations of that function that now fail to
even compile, because the function is hidden behind a `WithCount`
type.  
Take the test, for example:

```fsharp
[<Fact>]
let ``indexes a tree`` () =
    let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))

    let indexed, _ = index tree 1

    test <@ indexed = Node(Leaf ("one", 1), Node(Leaf ("two", 2), Leaf ("three", 3))) @>
```

The:

```fsharp
index tree 1
```

fails to compile. You could read it as:

```fsharp
(index tree) 1
```

Before you introduced `WithCount`, this was equivalent to:

```fsharp
// f :: Int -> (Tree (String, Int), Int)
let f = index tree  // invoking index
f 1                 // invoking the returned function
```

In this version, `f` is a function ready to be invoked. After the last change, what
you get is instead:

```fsharp
// WithCount Tree (string, int)
let withCount = index tree  // invoking index
withCount 1                 // this fails to compile
```

By the way: notice the simplification you already gained. What you get
back from invoking `index` is an indexed tree, `Tree (string, int)`,
surrounded by a `WithCount`. In this signature, there is no mention at
all to the count-handling logic. As I commented before, `WithCount` lets you
liberate your domain logic from the state handling. We will see this
better shortly.

Let's fix the compilation errors, first. F#'s function application is
not smart enough to pass an argument to a function when it is inside a
`WithCount`. You need a special version of function application. Let's
call it `run`:

```fsharp
let run (withCount: WithCount) count =
  let f = <get the function held by withCount>
  f count
```

Getting the function held by `WithCount` is idiomatically done in F#
with Pattern Matching. You remember that in [chapter
1](state-monad-for-the-rest-of-us-1) we mentioned that an instance
*remembers* which constructor was used to create it, and that you can
*deconstruct* the instance getting to the original arguments. It's way
easier done than said:

```fsharp
let run (WithCount f) count = f count
```

Cool. `run` is a super-function application that works for functions
held by a `WithCount` shell. If you have read [Monads for the Rest of
Us][monads-for-the-rest-of-us] you remember that I insist that monads
are not *a thing*. In a sense, they don't exist; *monadic functions*
do, and the key to understanding monads is to implement versions of
function application and function composition able to work with this
extended notion of functions. Here you are doing something very
similar: it is like you invented a novel notion of function. `index`
is a function that returns an indexed tree, *plus* an indication of
an extra-effect: depending on an input `count`, and returning back an
updated version of `count` itself. In other words, `index` is a
function that, besides doing its pure work, also lives in a context
where some *state* must be considered.


Instead of functions:

```haskell
f :: a -> b
```

you are now dealing with functions:

```haskell
f :: a -> WithCount b
```

which are a bit harder to concatenate, unless you extend some F#'s
native functionalities.

## Let it compile
Back to fixing the compilation errors. Everywhere there used to be a
native F#'s function application, you need to use your novel `run`
function. Here's the working result:

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

[<Fact>]
let ``indexes a tree`` () =
    let withCount: WithCount<string> = index tree
    let indexed, _ = run withCount 1

    test <@ indexed = Node(Leaf ("one", 1), Node(Leaf ("two", 2), Leaf ("three", 3))) @>
```

So far, you got a bit on improvement on the signature, but the
implementation code is still convoluted, and domain logic and
count-handling logic are still interwoven.  
It's time to sort out the tangle, and to invent the Applicative
Functor. Have a coffee and see you in a minute in [Chapter
8](state-monad-for-the-rest-of-us-8).


# References
* [State Monad For The Rest Of Us - source code][source-code]
* [Arialdo Martini - Monads for the Rest of
  Us][monads-for-the-rest-of-us]
  
[source-code]: https://github.com/arialdomartini/state-monad-for-the-rest-of-us
[monads-for-the-rest-of-us]: https://arialdomartini.github.io/monads-for-the-rest-of-us


# Comments
[GitHub Discussions][discussions]


[discussions]: https://github.com/arialdomartini/arialdomartini.github.io/discussions/30

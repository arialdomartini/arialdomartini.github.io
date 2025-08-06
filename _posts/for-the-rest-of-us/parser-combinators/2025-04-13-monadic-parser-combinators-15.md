---
layout: post
title: "Monadic Parser Combinators in F# - One Combinator to Rule Them All"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
include_in_index: false
---
We will dedicate the next couple of chapters to giving ourselves a pat
on the back to celebrate this little `bind` function, and how it is
our most outstanding invention since chapter 1.  
Here's the plan:

- We will see how Monads entail and incorporate Functors and
  Applicative Functors, and how `bind` can be used to reimplement
  `map` and `<*>`. Basically, we will find out that `bind` was the
  missing Swiss Army Knife to implement everything and the kitchen
  sink.

- Then, we will see how F#'s Computation Expressions can be used to
  simplify the syntax of parsers. We will discover an interesting
  imperative style that, despite apparences, is perfectly pure
  functional.

- Finally, we will revisit the first goal using this new syntactic
  style. Hopefully, at last!, the whole Monadic Parser Combinators
  topic will make click.
  
  
## Universal Laws
There is something deeply rewarding in Functional Programming: every
so often, when you stumble upon some discovery, you find that it is so
profoundly true and powerful that you can use it to express a wide
range of seemingly unrelated ideas, most likely that not more
concisely and elegantly than before.  

Graham Hutton wrote a famous paper on one of those cases: [A tutorial
on the universality and expressiveness of fold][hutton]. Indeed,
`fold` (or `Aggregate` in LINQ lingo) is so powerful that if you
stripped `Select`, `Where`, `Sum`, `First`, `Zip`, `CountBy` and other
functions away from LINQ, only saving `Aggregate`, believe it or not,
you could reimplement all of them entirely in terms of `Aggregate`
only. I cannot recommend to challenge you with this enlighting and fun
exercise more.

`bind` plays in the same league. Indeed, I truly hope that, in the
sections ahead, I will be able to show you what a tremendous power you
uncovered with `>>=`.

## Look Ma, Functors!
Do you remember when in [Chapter
7](/monadic-parser-combinators-7) we defined `map`?

```fsharp
let map (f: 'a -> 'b) (ap: 'a Parser) : 'b Parser =
    Parser (fun input ->
        let ar : 'a ParseResult = run ap input
        match ar with
        | Success (rest, a) -> Success (rest, f a)
        | Failure s -> Failure s )
```

Initially, we named it `<<|`, because we wanted to see it as the
on-steroid sibling of the reverse-pipe operator `<|`. Then we started
interpreting it as a way to map a function to the *content* of a
parser, and we gave it the alias `<!>`. Finally, in [Chapter
10](/monadic-parser-combinators-10) we discovered Applicative
Functors, and we found out that `map` could be expressed in terms of
`<*>` and `pure'`:

```fsharp
let map = (<<|)
let map (f: 'a -> 'b) (a: 'a Parser) : 'b Parser =
    pure' f <*> a
```

We can do the same with Monads. Try yourself before reading the
solution: how to write `map` only using `>>=` and `return`? It's not
an easy exercise but it is worth trying. Anyway, fear not: later I
will show you an easy approach that should be really easy to grasp.

Here is how I would do this. I would start by analyzing the
signatures:

```fsharp
map : ('a -> 'b) -> 'a Parser -> 'b Parser 
bind : 'a Parser -> ('a -> 'b Parser) -> 'b Parser
return : 'a -> 'a Parser
```

We want to build `map`, so we want to complete this implemenentation:

```fsharp
let map (f: 'a -> 'b) (ap: 'a Parser) = 
    ...
```

Both `map` and `bind` return a `'b Parser`, so the only challenge is
with the input parameters.  
As input, we have `f` and `ap`. Can we just pass them as they are to `return'` and `bind`?  
Well, `ap` has already the right type for `bind`, as it matches the
1st parameter. The second parameter, though, should be `'a -> 'b
Parser`, while we have `'a -> 'b`. But we know that `return'` can help
lifting a `'b` to `'b Parser`:

```fsharp
let f' = fun a -> 
    let b:  'b        = f a
    let bp: 'b Parser = return' b
    b
```

Good, that's it! We just have to invoke `bind` now:

```fsharp
let map (f: 'a -> 'b) (ap: `a Parser) = 
    let f' = fun a -> 
        let b = f a
        let bp = return' b
        b
        
    bind ap f'
```

We can make it way shorter inlining the variables:

```fsharp
let map (f: 'a -> 'b) (ap: `a Parser) = 
    let f' = fun a -> return' f a
        
    bind ap f'
```

and then observing that:

```fsharp
    let f' = fun a -> return' f a
```

can be written in Point-Free style with the `>>` operator:

```fsharp
let map (f: 'a -> 'b) (ap: `a Parser) = 
    let f' = f >> return'
        
    bind ap f'
```

It helps me to read `>>` as "*and then*", so that the expression:

```fsharp
f >> return'
```

reads as:

```
apply f, and then return'
```

which is exactly what the meaning of the original:

```fsharp
fun a -> return' f a
```

This gets us to:

```fsharp
let map (f: 'a -> 'b) (ap: `a Parser) = 
    bind ap (f >> return')
```

or, using the infix alias `>>=`:

```fsharp
let map f ap =
    ap >>= (f >> return')
```

Wow! How concise! The compiler is happy with the signature and every,
every single test is still green. [This is heavy][doc]! It's actually
quite something!

I guess that the result might appear cryptic and magic. I swear that,
after playing enough with FP, you will find it understandable. And I
promise that when we will eventually distill the *do notation*, you
will even find it familiar.

## Look Ma, Applicative Functors Too!
Writing `map` in terms of `>>=` was cool. But we already wrote it
in terms of `<*>`, so shall we be so impressed?

What if we killed `<*>`'s implementation and redefined it in terms
`>>=` and `return'`? That would be similar to the case of `Aggregate`
and LINQ: `>>=` would really be all we ever needed, the
one-size-fits-all tool, the mythical silver-bullet operator.

In [Chapter 10](/monadic-parser-combinators-10) we wrote:

```fsharp
let ap fP aP = Parser (fun input ->
    match run fP input with
    | Failure e ->  Failure e
    | Success (rf, f) ->
        match run aP rf with
        | Failure s -> Failure s
        | Success (ra, a) -> Success (ra, f a))

let (<*>) = ap
```

How can we write this in terms of `>>=`? OK, this is tought. I have no
idea where to start from. Shall we try analyzing the
signatures, like we did with `map`?

```fsharp
ap :  ('a -> 'b) Parser -> 'a Parser -> 'b Parser 
bind : 'a Parser -> ('a -> 'b Parser) -> 'b Parser
return : 'a -> 'a Parser
```

Honestly, I don't see any easy combination. I can't help but feeling
lost. It's just beyond what my brain can process. What can help my
poor limited understanding is the following trick.  
Whenever I see the `>>=` operator in an expression like:

```fsharp
foo >>= fun (bar -> baz)
```

I interpret it like:


```fsharp
someParser >>= fun (theValueItParsed -> whatIWantToDoWithThatValue)
```

This matches 1:1 the signature:

```fsharp
bind : 'a Parser -> ('a -> 'b Parser) -> 'b Parser
```

The rule of thumb I keep in mind is:

* Whenever I find a Parser
* I can apply `>>=`.
* What follows is a function that simply receives the parsed value.
* So I can just operate on that value, ignoring that I am in the
  context of parsers.
* The only caveat I have to remember: at the end, I have to return a
Parser, not a bare value.

Basically, I often use this metaphor: `>>=` is a lens that lets me look
*inside* the parser box, so I can completely forget about parsers and
deal directly with values:

```fsharp
parser >>= (fun parsedValue -> ...)
```

Fine. Going back to rewriting `ap`:

```fsharp
// ('a -> 'b) Parser -> 'a Parser -> 'b Parser 
let ap (fP: ('a -> 'b) Parser) (aP: 'a Parser) =
    ...
```

`fP` is a function, and `aP` is the value to feed it
with. Unfortunately, they are both inside a parser. No problem: we'll
use the `>>=` lens to extract their values. We will have to apply
`>>=` twice, one time to look insie `fP`, one time for `aP`. Let's
start with accessing `f` inside `fP`:

```fsharp
let ap (fP: ('a -> 'b) Parser) (aP: 'a Parser) =
    fp >>= (fun f ->
        ...)
```

Let's do the same with `aP`:


```fsharp
let ap fP (aP: 'a Parser) =
    fp >>= (fun f ->
        aP >>= (fun a ->
            ...))
```

Good. We have `f` and its argument `a`. That's easy! Applying `f` to
`a` will get us back `b`:


```fsharp
let ap fP (aP: 'a Parser) =
    fp >>= (fun f ->
        aP >>= (fun a ->
            let b = f a
            ...))
```

Can we just return `b`? No, both the `>>=` signature and the signature
of `ap` itself claim we should return a `'b Parser`, not a `'b`. Easy!
`return'` to the resque:


```fsharp
let ap fP (aP: 'a Parser) =
    fp >>= (fun f ->
        aP >>= (fun a ->
            let b = f a
            return' b))
```

Making it shorter by inlining the temporary variable:


```fsharp
let ap fP (aP: 'a Parser) =
    fp >>= (fun f ->
        aP >>= (fun a ->
            return' f a))
```

and then, again applying `>>`:


```fsharp
let ap fP (aP: 'a Parser) =
    fp >>= (fun f -> 
        aP >>= (f >> return'))
```

The compiler is happy, tests are green, so this expression must be
correct.

## Not Really My Vibe

If you are one of those horrible developers who are proud when the
code is super-concise, magic and almost impenetrable to your
colleagues, you can stop here and celebrate.

In theory you could even keep rewriting `many`, `many1`,
`>>.`, `.>>`, `between`, `sepBy` and all the other parser combinators
we have invented in the past chapters using `>>=` only. It is
technically possible. Just know that every time you do that, a fairy
loses its wings. You should feel bad!

To me, it would make little sense. I personally find this result too
cryptic and not particularly expressive.  
So, here's my plan: I would rather get an energizing Tiramisù then I
will quickly proceed with [Chapter
16](/monadic-parser-combinators-16), in which I intend to transform
this horrible syntax into something more digestible for the rest of
us. Then, I promise, there will be a very convincing reason to rewrite
*some* of the past combinators with Monads. In the meanwhile, buon
appetito.


# References

[Graham Hutton - A tutorial on the universality and expressiveness of fold][hutton]


[hutton]: https://people.cs.nott.ac.uk/pszgmh/fold.pdf
[doc]: https://backtothefuture.fandom.com/wiki/This_is_heavy

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

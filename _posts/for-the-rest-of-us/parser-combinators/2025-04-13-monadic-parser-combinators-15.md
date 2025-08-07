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
on the back to celebrate this little `bind` function as
our most outstanding invention since chapter 1.  
Here's the plan:

- We will see how Monads entail and incorporate Functors and
  Applicative Functors, and how `bind` can be used to reimplement
  `map` and `<*>`. Basically, we will find out that `bind` was the
  missing Swiss Army Knife to implement everything and the kitchen
  sink.

- Then, we will see how F#'s Computation Expressions can be used to
  simplify the use of `bind` and, consequently, the syntax of
  parsers. We will discover an interesting imperative style that,
  despite apparences, is perfectly pure functional.

- Finally, we will revisit the first goal using this new syntactic
  style. Hopefully, at last!, the whole Monadic Parser Combinators
  topic will make click.
  
  
## Universal Laws
There is something deeply rewarding in Functional Programming: every
now and then, when you stumble upon some discovery, you find that it
is so profoundly true and powerful that you can use it to express a
wide range of seemingly unrelated ideas, often more concisely and elegantly than before.  

Graham Hutton wrote a famous paper on one of those cases: [A tutorial
on the universality and expressiveness of fold][hutton]. Indeed,
`fold` (or `Aggregate` in LINQ lingo) is so powerful that if you
stripped `Select`, `Where`, `Sum`, `First`, `Zip`, `CountBy` and other
functions away from LINQ, only saving `Aggregate`, believe it or not,
you could reimplement all of them entirely in terms of `Aggregate`. I
cannot recommend to challenge you with this enlighting and fun
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
        | Success (a, rest) -> Success (f a, rest)
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
let map f a =
    pure' f <*> a
```

We can do the same with Monads. Challenge yourself before reading the
solution: how to write `map` only using `>>=` and `return'`? It's not
an easy exercise but it is worth trying, and very rewarding.

Here is how I would do this. I would start by analyzing the
signatures:

```fsharp
map : ('a -> 'b) -> 'a Parser -> 'b Parser 
bind : 'a Parser -> ('a -> 'b Parser) -> 'b Parser
return : 'a -> 'a Parser
```

We want to build `map`, so we want to complete this implementation:

```fsharp
let map (f: 'a -> 'b) (aP: 'a Parser) = 
    ...
```

Both `map` and `bind` return a `'b Parser`, so the only challenge is
with the input parameters.  
As input, we have `f` and `aP`. Can we just pass them as they are to `return'` and `bind`?  
Well, `aP` has already the right type for `bind`, as it matches the
1st parameter. The second parameter, though, should be `'a -> 'b
Parser`, while we have `'a -> 'b`. But we know that `return'` can help
lifting a `'b` to `'b Parser`:

```fsharp
let map (f: 'a -> 'b) (aP: `a Parser) = 
    let f' = fun a -> 
        let b:  'b        = f a
        let bP: 'b Parser = return' b
        b
```

Good, that's it! We just have to invoke `bind` now:

```fsharp
let map (f: 'a -> 'b) (aP: `a Parser) = 
    let f' = fun a -> 
        let b = f a
        let bP = return' b
        b
        
    bind aP f'
```

We can make it way shorter inlining the variables:

```fsharp
let map (f: 'a -> 'b) (aP: `a Parser) = 
    let f' = fun a -> return' f a
        
    bind aP f'
```

and then observing that:

```fsharp
    let f' = fun a -> return' f a
```

can be written in Point-Free style with the `>>` operator:

```fsharp
let map (f: 'a -> 'b) (aP: `a Parser) = 
    let f' = f >> return'
        
    bind aP f'
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
let map (f: 'a -> 'b) (aP: `a Parser) = 
    bind aP (f >> return')
```

or, using the infix alias `>>=`:

```fsharp
let map f aP =
    aP >>= (f >> return')
```

Wow! How concise! The compiler is happy with the signature and every,
every single test is still green. [This is heavy][doc]! It's actually
quite something! I bet that the result appears cryptic and magic, at
first. I swear that, after playing enough with FP, you will find it
understandable. And I promise that, when we will finally introduce the
*do notation* by the means of F# Computation Expressions, everything
will get very intuitive.

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
    | Success (f, rf) ->
        match run aP rf with
        | Failure s -> Failure s
        | Success (a, ra) -> Success (f a, ra))

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
poor limited understanding is the following mental translation.  
Whenever I see the `>>=` operator in an expression like:

```fsharp
foo >>= (fun bar -> baz)
```

I interpret it like:


```fsharp
someParser >>= (fun theValueItParsed -> whatIWantToDoWithThatValue)
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
`>>=` twice, one time to look inside `fP`, the other time for
`aP`. Let's start with accessing `f` inside `fP`:

```fsharp
let ap (fP: ('a -> 'b) Parser) (aP: 'a Parser) =
    fP >>= (fun f ->
        ...)
```

Let's do the same with `aP`:


```fsharp
let ap fP (aP: 'a Parser) =
    fP >>= (fun f ->
        aP >>= (fun a ->
            ...))
```

Good. We have `f` and its argument `a`. That's easy! Applying `f` to
`a` will get us back `b`:


```fsharp
let ap fP (aP: 'a Parser) =
    fP >>= (fun f ->
        aP >>= (fun a ->
            let b = f a
            ...))
```

Can we just return `b`? No, both the `>>=` signature and the signature
of `aP` itself claim we should return a `'b Parser`, not a `'b`. Easy!
`return'` to the resque:


```fsharp
let ap fP (aP: 'a Parser) =
    fP >>= (fun f ->
        aP >>= (fun a ->
            let b = f a
            return' b))
```

Done! Let's make it shorter, now, by inlining the temporary variable:


```fsharp
let ap fP (aP: 'a Parser) =
    fP >>= (fun f ->
        aP >>= (fun a ->
            return' f a))
```

and then, again applying `>>`:


```fsharp
let ap fP (aP: 'a Parser) =
    fP >>= (fun f -> 
        aP >>= (f >> return'))
```

The compiler is happy, tests are green, so this expression must be
correct.

## Not Really My Vibe

If you are one of those horrible developers who are proud when the
code is super-concise, magic and almost impenetrable to your
colleagues, you can stop here and praise yourself. In theory you could
even keep rewriting `many`, `many1`, `>>.`, `.>>`, `between`, `sepBy`
and all the other parser combinators we have invented in the past
chapters using `>>=` only. It is technically possible. Just know that
every time you do that, a fairy loses its wings.  
To me, in most of the cases, it makes little sense. I personally find
this result too cryptic and not particularly expressive.

So, here's my alternative plan: I would rather get an energizing
Tiramisù; then I will quickly proceed with [Chapter
16](/monadic-parser-combinators-16), in which I intend to transform
this horrible syntax into something more digestible for the rest of
us. Then, I promise, there will be a very convincing reason to rewrite
*some* of the past combinators with Monads. In the meanwhile, buon
appetito.


[Previous - Mind the Context](/monadic-parser-combinators-14) ⁓
[Next - A Programmable Semicolon](/monadic-parser-combinators-16)


# References

* [Graham Hutton - A tutorial on the universality and expressiveness of fold][hutton]


[hutton]: https://people.cs.nott.ac.uk/pszgmh/fold.pdf
[doc]: https://backtothefuture.fandom.com/wiki/This_is_heavy

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

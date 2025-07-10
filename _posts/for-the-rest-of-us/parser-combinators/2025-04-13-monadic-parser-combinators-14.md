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
on the back celebrating the fact that the little `bind` function we
have just distilled is our most outstanding invention since
chapter 1. Here's the plan:

- We will see how monads entail and incorporate functors and
  applicative functors, and how `bind` can be used to reimplement
  `map` and `<*>`. Basically, we will discover the `bind` was the
  missing Swiss Army Knife to implement everything and the kitchen
  sink.

- We will see how F#'s Computation Expressions can be used to simplify
  the syntax of parsers. We will discover an interesting imperative
  style which, under the hood, keeps being perfectly pure functional.

- Finally, we will revisit the first goal using this new syntactic
  style. Hopefully, at last!, the whole Monadic Parser Combinators
  topic will make click.
  
  
## Universal Laws
There is something deeply rewarding in Functional Programming: every
so often, you stumble upon some discovery, and you find that it is so
profoundly true and powerful that you can you can use it to express a
wide range of seemingly unrelated ideas, and often way more concisely
and elegantly than before.  

Graham Hutton wrote a famous paper on one of those case: [A tutorial
on the universality and expressiveness of fold][hutton]. `fold`
(`Aggregate` in LINQ) is so powerful that if you stripped `Select`,
`Where`, `Sum`, `First`, `Zip`, `CountBy` and other functions away
from LINQ, only saving `Aggregate`, believe it or not, you could
reimplement each of them entirely in terms of `Aggregate` only. I
cannot recommend to challenge you with this enlighting and fun
exercise more.

## Look ma, functors!
`bind` plays in the same league. Do you remember when in [Chapter
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
We can do the same with `>>=`. Try yourself before reading the
solution. It's not an easy exercise, but it is worth trying. Anyway,
fear not: later I will show you an easy approach that should be really
easy to grasp.

So, let's analyze the signatures:

```fsharp
map : ('a -> 'b) -> 'a Parser -> 'b Parser 
bind : 'a Parser -> ('a -> 'b Parser) -> 'b Parser
return : 'a -> 'a Parser
```

We want to build `map`, so we want to complete this implemenentation:

```fsharp
let map (f: 'a -> 'b) (ap: `a Parser) = 
    ...
```

Both `map` and `bind` return a `'b Parser`, so the only challenge is
with the input parameters.  
As input, we have `f` and `ap`, and all we can do is to use them as
arguments for `return'` and `bind`.  
`ap` has already the right type for `bind`, as it matches the 1st
parameter. The second parameter, though, should be `'a -> 'b Parser`,
while we have `'a -> 'b`. But we know how to lift a `'b` to `'b
Parser`: with `return'`:

```fsharp
let f' = fun a -> 
    let b = f a
    let bp = return' b
    b
```

Good, we have all the ingredients:

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

and then realizing that:

```fsharp
    let f' = fun a -> return' f a
```

can be written in Point-Free style with the `>>` operator:

```fsharp
let map (f: 'a -> 'b) (ap: `a Parser) = 
    let f' = f >> return'
        
    bind ap f'
```

That gets us to:

```fsharp
let map (f: 'a -> 'b) (ap: `a Parser) = 
    bind ap (f >> return')
```

or, using the infix alias `>>=`:

```fsharp
let map f ap =
    ap >>= (f >> return')
```

Wow! That's quite something! The compiler is happy with the signature
and every, every single test is still green. [This is heavy][doc]!

I guess the result might appear cryptic and magic. I swear that after playing
enough with FP you will find it understandable. And I promise that
when we will eventually distill the do-notation of the very same
formula, you will find reading it just familiar.

* `map` takes `f: 'a -> 'b` and applies it to the value parsed by `'a
  Parser`. It maps functions to parsers.
* *Mapping* a function `f` is equivalent to *binding* the function `f
  >> return'`, because `f >> return'` simply means "run `f`, then lift
  the result to the Parser world, to make the `bind` signature happy".

I know, I know, it's hairy. Bear with me, read this section again when
we have introduce the do-notation.

## Look ma, applicative functors either!
Writing `map` in terms of `>>=` is cool. But we already wrote it
in terms of `<*>`, so what's the reason to be that impressed?

What if we could also kill `<*>`'s implementation, and redefine it in
terms `>>=` and `return'`? It would be just like with `Aggregate` and
LINQ: `>>=` would really be all we ever needed, the one-size-fits-all,
the one-stop, the mythical silver-bullet operator.

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

OK, this is tought. I have no idea where to start from and, honestly, if
I try to analyze the signatures like we did with `map`:


```fsharp
ap :  ('a -> 'b) Parser -> 'a Parser -> 'b Parser 
bind : 'a Parser -> ('a -> 'b Parser) -> 'b Parser
return : 'a -> 'a Parser
```


I don't see any easy combination. I can't help but feeling lost. It's
just beyond what my brain can process.  
What can help my poor limited understanding is this trick
here. Whenever I see the `>>=` operator in an expression like:

```fsharp
foo >>= fun (bar -> baz)
```

I interpret it like:


```fsharp
someParser >>= fun (theValueItParsed -> howYouWantToProcessThatValue)
```

This matches 1:1 the signature:

```fsharp
bind : 'a Parser -> ('a -> 'b Parser) -> 'b Parser
```

The rule of thumb I keep in mind is:

* Whenever I find a Parser
* I can apply `>>=`.
* What follows is a function that will receive the parsed value
* Then, I have to remember, I still have to return a Parser, not a
bare value.

I admit I always mentally use this metaphore: `>>=` is a lens that
lets me see *inside* the parser box, getting to its parsed value:

```fsharp
parser >>= (fun parsedValue -> ...)
```

Fine. Going back to:

```fsharp
// ('a -> 'b) Parser -> 'a Parser -> 'b Parser 
let ap (fP: ('a -> 'b) Parser) (aP: 'a Parser) =
    ...
```

`fP` is a function, and `aP` is the value to feed it
with. Unfortunately, they are both inside a parser. No problem: we'll
use the `>>=` lens to extract the value. Let's start with accessing
`f` inside `fP`:

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

Good. We have `f` and its argument `a`:


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

Making it shorter:


```fsharp
let ap fP (aP: 'a Parser) =
    fp >>= (fun f ->
        aP >>= (fun a ->
            return' f a))
```

and then:


```fsharp
let ap fP (aP: 'a Parser) =
    fp >>= (fun f -> 
        aP >>= (f >> return'))
```

So, compiler is happy, tests are green, so this expression must be
correct. If you are one of those devs who are proud when the code is
super-concise, magic and almost impenetrable to other devs
comprehension, you can stop here and celebrate.

I personally find this result too cryptic and poorly expressive. So,
here's my plan: I will get an energizing Tiramisù and I will quickly
proceed with [Chapter 15](/monadic-parser-combinators-15), in which I
intend to transform this horrible syntax into something more
digestible for the rest of us. Buon appetito.

# References

[Graham Hutton - A tutorial on the universality and expressiveness of fold][hutton]


[hutton]: https://people.cs.nott.ac.uk/pszgmh/fold.pdf
[doc]: https://backtothefuture.fandom.com/wiki/This_is_heavy

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

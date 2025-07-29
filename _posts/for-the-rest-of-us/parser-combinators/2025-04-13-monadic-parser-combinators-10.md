---
layout: post
title: "Monadic Parser Combinators in F# - Applying Functions, Ad Nauseam"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
include_in_index: false
---
There is something magic about the native F# function application:
once you apply a function to an argument, if the result is another
function you can just run function application again. And if you get
yet another function, you can do the same, ad nauseam.  
See this example:

```fsharp
[<Fact>]
let ``function application with 2 parameters`` () =

    let fa: int  -> (bool -> string) = fun a -> fun b -> $"{a}, {b}"
    let fb:          bool -> string  = fa 42
    let c:                   string  = fb true

    test <@ c = "42, True" @>
```

- `fa` is a function that takes `a`, an `int`, and that returns
  another function, `bool -> string`.
- If you use the native function application to apply `f` to `42`, you
  get `fb: bool -> string` back.
- Now, thare's nothing special in that `bool -> string` function: it
  is just another function. So, you can keep using the F# native
  function application to pass it the next argument, a `true` value,
  which finally gets you back a `string`.
  
It's easy to see how this does not stop with 2-parameter functions.
Here's a test for a 3-parameter function:

```fsharp
[<Fact>]
let ``function application with 3 parameters`` () =

    let fa: int -> (bool -> (string -> string)) =
        fun a -> fun b -> fun c -> $"{a}, {b}, {c}"

    let fb: bool -> (string -> string) = fa 42
    let fc: string -> string = fb true
    let d: string = fc "foobar"

    test <@ d = "42, True, foobar" @>
```


Now:

```fsharp
let f:  int  -> (bool -> (string -> string)) = 
    fun a -> fun b -> fun c -> $"{a}, {b}, {c}"
```

is a very verbose way to define a function returning a function, in
turn, returning a function. F# lets you:

- remove the parenthesis from the signature, since function
  application associates to the right:


```fsharp
let f:  int  -> bool -> string -> string =
    fun a -> fun b -> fun c -> $"{a}, {b}, {c}"
```

- skip that `fun a -> fun b -> fun c` boiler plate and just write `f a b
c`, since all the functions are automatically curried:


```fsharp
let f a b c = $"{a}, {b}, {c}"
```

- mentally see this as a function of 3 parameters.

- apply it to multiple parameters in a single shot, with:

```fsharp
let d = f 42 true "foobar"
```

Ah, much better! But, note: it's just syntactic sugar. This is still a
function returning a function &mdash; in turn, returning a function.

## A Crocked Function Application
What about the Parser-Powered Function Application `<<|` / `map` that
we have so proudly distilled in [Chapter
7](/monadic-parser-combinators-7)?  Can we also apply it *ad nauseam*?

Let's see. We start from a generic 3-parameter function `'a -> 'b ->
'c -> 'd`. In this context, we are not concerned how it is
implemented, we can just focus on its signature:


```fsharp

[<Fact>]
let ``Parser-powered function application with 3 parameters`` () =

    let f (a: int) (b: 'b) (c: 'c) = __
    ...
```

Then, we apply it to an argument `'a Parser`, of course using `map`.
Let me use the symbol `<!>` instead of `<<|` or `map`; after all, we
mentioned that they are all synonyms:

```fsharp
let (<!>) = map

[<Fact>]
let ``Parser-powered function application with 3 parameters`` () =

    let a: 'a Parser = __

    let f (a: int) (b: 'b) (c: 'c) = __
    
    let fa: ('b -> 'c -> 'd) Parser = 
        f <!> a
    
    ...
```

Oh no! Look at `fa`'s signature! The result is not just another
function with 1 parameter less, like it happened before . It's not
even a function anymore: it's a function wrapped inside a Parser. if
you think what's `<!>` purpose, this makes sense. If you apply `<!>`
to an `'a -> 'b` function, you get this:

<p align="center">
  <img src="static/img/parser-combinators-for-the-rest-of-us/map-ap-part-1.png"
  alt="" height="350px">
</p>

If you think to a 2-parameter function `'a -> 'b -> 'c` as a
1-parameter function `'a -> ('b -> 'c)` &mdash; so as a function which
just happens to return another function &mdash; then applying `<!>`
gets you this:

<p align="center">
  <img src="static/img/parser-combinators-for-the-rest-of-us/map-ap-part-2.png"
  alt="" height="350px">
</p>


This means that we cannot apply `<!>` again... Does it mean we need a
different operator? Yes, we do! It could be demonstrated that, for
such cases, Functor's `map` is of little help. It's time to invent a
more powerful version of Functors: enter Applicative Functors.


## Beyond Functors

You already guessed the next steps: we will implement a new operator,
dedicating it yet another symbol, and letting its signature lead the
way. Then, hopefully, we will manage to use the new operator to
express, in a smarter and more concise way, some of the things we have
distilled so far.  
And you guessed it right! We are going to distill `<*>` which, with a
burst of creativity, we are going to call "apply" or `ap`.

Let's recover from where we left:

```fsharp
[<Fact>]
let ``Parser-powered function application with 3 parameters`` () =

    let a: 'a Parser = __
    let b: 'b Parser = __
    let c: 'c Parser = __

    let f (a: int) (b: 'b) (c: 'c) = __
    
    let fa: ('b -> 'c -> 'd) Parser = f <!> a
    ...
```

We want to apply `fa`, a 2-parameter function inside a Parser, to the
next argument, a `'b Parser`. In order to proceed, let me use a little
syntax maneuver, so that the result will resemble the native F#
function application: hopefully, this will let us see what's going on
in a more streamlined way.

With the native F# function application, when you have a
multiparameter function:

```fharp
let f: 'a -> 'b -> 'c -> 'd = __
```

you can apply it to arguments just by separating them with white
spaces:


```fsharp
let d = f a b c
```

With a bit of imagination, you can think to those white spaces as an
F# native pseudo-operator. We did this exercise with `map` already. In
the Parser world, you have to use `<!>` for the first argument, which
gives you back a Parser-wrapped function. The idea is to use an
equivalent to the native white-space pseudo-operator, this
yet-to-be-implemented `<*>` operator:

```fsharp
let dP = f <!> aP <*> bP <*> cP
```

You see the equivalence?


```fsharp
let d:  'd         = f     a      b      c
let dP: 'd Parser  = f <!> aP <*> bP <*> cP
```


Basically, we are writing an enhanced version of whitespace.


<p align="center">
  <img src="static/img/parser-combinators-for-the-rest-of-us/map-ap-part-3.png"
  alt="" width="100%">
</p>


Implementing `<*>` is not hard. You just have to be driven by the type
signature. Let's start with the simplest case of 1-parameter
functions. `ap` / `<*>` is that operator that given a 1-parameter
function wrapped in a Parser:

```fsharp
val ap : ('a -> 'b) Parser -> ...

let (<*>) = ap
```

lets us apply the wrapped `'a -> 'b` to an `'a Parser` argument:


```fsharp
val ap : ('a -> 'b) Parser -> 'a Parser -> ...
```

What will this give us back? Let's think about it. We cannot get back
a `'b` value, right? A Parser is a promise of a value, so if we give
parsers it's fair to be paid back with other parsers. It's legitimate
to assume we get back a `'b Parser`:


```fsharp
val ap : ('a -> 'b) Parser -> 'a Parser -> 'b Parser
```

Let's implement it:


```fsharp
let ap fP aP = __

let (<*>) = ap

[<Fact>]
let ``ap with a 1-parameter function`` () =
    let aP = Parser (fun input -> Success(42, input))

    let fP = Parser (fun input -> Success (f, input))

    test <@ run (fP <*> aP) "some input" = Success (84, "some input")@>
```

Not how, to keep things simple, we are using 2 trivial parsers, which
do not even consume the input: `aP` just returns a Parser-wrapped
`42`, `fP` a Parser-wrapped `fun i -> i * 2`. For cases like these, we
can have a convenience function `pure'`, which takes whatever value
you give it in and wraps it into a doing-nothing Parser:

```fsharp
let pure' a = Parser (fun input -> Success (a, input))

// ('a -> 'b) Parser -> 'a Parser -> 'b Parser
let ap fP aP = __

[<Fact>]
let ``ap with a 1-parameter function`` () =
    let aP = pure' 42

    let fP = pure' (fun a -> a * 2)

    test <@ run (fP <*> aP) "some input" = Success (84, "some input")@>
```

You know how to proceed. The signature of `ap` tells you to return a
`'b Parser`. Just build one:

```fsharp
let ap fP aP = Parser (fun input ->
    ...)
```

Now, have a function `f` inside a Parser (`fP`) and the argument `a`
also inside a Parser (`aP`). You also have an `input`. Using the box
analogy, it seems that solving this riddle is a matter of:

- opening both the boxes;
- extracting the contained `f` and `a`;
- applying `f` to `a`;
- possibly giving up in case of failure;
- and passing the unconsumed input around;
- then, successfully return the result.

Conventionally, the first box to open is the one containing the
function:

```fsharp
let ap fP aP = Parser (fun input ->
    match run fP input with
    | Failure e ->  Failure e
    | Success (f, rf) ->
        ...
```

Of course, if we get an error, we give up. If we are successful, we
get the inner function `f` the unconsumed input `rf` ("rest of `f`").
Fine, we have all the ingredients to open the `a` box:

```fsharp
let ap fP aP = Parser (fun input ->
    match run fP input with
    | Failure e ->  Failure e
    | Success (f, rf) ->
        match run aP rf with
        | Failure s -> Failure s
        | Success (a, ra) -> ...
```

Cool. We have `f`, we have `a` and the unconsumed input `ra`.
Time to finally apply `f` to `a`, and to wrap the result in a
`Success`:

```fsharp
let ap fP aP = Parser (fun input ->
    match run fP input with
    | Failure e ->  Failure e
    | Success (f, rf) ->
        match run aP rf with
        | Failure s -> Failure s
        | Success (a, ra) -> Success (f a, ra))
```

Green tests.

## Dealing with Details, Again?
Wait a second: why did we have to pattern match and to pass unconsumed
input around? Didn't we say that we could always build on top of the
previous building blocks?  
It turns our that the Applicative Functor's `<*>` operator you just
invented cannot be built in terms of humble Functor's `map`. We could
even demonstrate it mathematically. On the contrary: we could
demonstrate that, since Applicative Functors are more powerful than
Functors, we can rewrite `map` in terms of `pure'` and `ap`:

```fsharp
let map (f: 'a -> 'b) (a: 'a Parser) : 'b Parser =
    pure' f <*> a
    
let (<<|) = map
let (<!>) = map
```

`map` implementation may look obscure, but it is in fact very logic.
If you compare the signatures of `map` and `<*>` you see that the only
difference is that in `<!>` the parameter `f` is not wrapped inside a
Parser. So, if you lift `f` inside a parser with `pure' f`, you would
get to `<*>` signature; that is, you can proceed applying `<*>` to the
`'a Parser` argument.

Try running all the past tests you wrote so far. Woah! What a
beautiful display of green! It seems that with `pure'` and `<*>` you
really discovered something deep.

## Apply, Apply, Apply!
We opened this chapter claiming that there is something magic about
the native F# function application, because you can apply a function
returning a function returning a function &mdash; ad nauseam &mdash;
to arguments, and because the unsuspectedly powerful whitespace
pseudo-operator will happily do its work.

Does your brand new `<*>` have the same super-power? Let's see. We
want to have the equivalent of this test:

```fsharp
[<Fact>]
let ``function application with 3 parameters, inlined`` () =

    let f a b c = $"{a}, {b}, {c}"

    let a = 42
    let b = true
    let c = "foobar"

    let d = f a b c

    test <@ d = "42, True, foobar" @>
```

but using Parser arguments:

```fsharp
[<Fact>]
let ``ap with a 3-parameter function`` () =
    let f a b c = $"{a}, {b}, {c}"

    let aP = pure' 42
    let bP = pure' true
    let cP = pure' "foobar"

    let dP = f <!> aP <*> bP <*> cP

    test <@ run dP "some input" = Success ("some input", "42, True, foobar") @>
```

Amazing! It works! You have actually invented a very generic
Parser-powered function application, haven't you?  
I did not introduce `pure'` as a coincidence: in fact, the combination
of `pure'` and `<*>` is what conventionally defines Applicative
Functors.

Let's get our hands dirty building some real parsers with `<*>` and
`pure'`. Treat yourself with a rösti and get prepared to [Chapter 11](/monadic-parser-combinators-11).

[Previous - Things You Don't Care About](/monadic-parser-combinators-9) ⁓
[Next - Lifting Functions](/monadic-parser-combinators-11)


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

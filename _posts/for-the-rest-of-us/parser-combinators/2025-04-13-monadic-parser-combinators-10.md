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
function, you can just run function application again. And if you get
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
- Now, thare's nothing special in that `bool -> string` returned
  value: it is just another function. So, you can keep using the F#
  native function application to pass it the next argument, a `true`
  value, which finally gets you back a `string`.
  
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

- skip that `fun a -> fun b -> fun c` boiler plate and just write `f a
b c`. This can be expressed saying that in F# all the functions are
automatically curried:


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

`<!>` allows passing an `'a Parser` to a function expecting an
`'a`. But, darnit! Look at the resulting `fa`'s signature! The result
is not just another function with 1 parameter less, like it happened
before . It's not even a function anymore: it's a function wrapped
inside a Parser. If you think what `<!>`'s purpose is, this makes
sense. If you apply `<!>` to an `'a -> 'b` function, you get this:

<p align="center">
  <img src="static/img/parser-combinators-for-the-rest-of-us/map-ap-part-1.png"
  alt="" height="350px">
</p>

Focus on the returned value, in this case `'b`: it ends up being
wrapped in a `Parser`.  
Now, if you think to a 2-parameter function `'a -> 'b -> 'c` as a
1-parameter function `'a -> ('b -> 'c)` &mdash; so as a function which
just happens to return another function &mdash; then applying `<!>`
gets you this:

<p align="center">
  <img src="static/img/parser-combinators-for-the-rest-of-us/map-ap-part-2.png"
  alt="" height="350px">
</p>


This means that we cannot apply `<!>` again, ad nauseam...

Does it mean that we need a different operator? Yes, that's exactly
the point! It could be demonstrated that for such cases Functor's
`map` is of little help: there is no possible way to perform the next
function application only using `map`'s capabilities. It's time to
invent a more powerful version of Functors: enter Applicative
Functors.


## Beyond Functors

You already guessed the next steps: we will implement a new operator,
dedicating it yet another symbol, and letting its signature lead the
way. Then, hopefully, we will manage to use the new operator to
express, in a smarter and more concise way, some of the things we have
distilled so far. Ideally, we could discover that the new operator is
so powerful to incorporate `map` itself.  
Without further ado, let's distill `<*>`. With a burst of creativity,
we will call it "apply" or `ap`.

Let's recover from where we left:

```fsharp
[<Fact>]
let ``Parser-powered function application with 3 parameters`` () =

    let aP: 'a Parser = __
    let bP: 'b Parser = __
    let cP: 'c Parser = __

    let f (a: int) (b: 'b) (c: 'c) = __
    
    let fa: ('b -> 'c -> 'd) Parser = f <!> aP
    ...
```

We want to apply `fa`, a 2-parameter function inside a Parser, to the
next argument, a `'b Parser`. In order to proceed, let me use a little
syntax maneuver, so that the result will resemble the native F#
function application: hopefully, this will let us see what's going on
in a more streamlined way. With the native F# function application,
when you have a multiparameter function:

```fharp
let f: 'a -> 'b -> 'c -> 'd = __
```

you can apply it to arguments just by separating them with white
spaces:


```fsharp
let d = f a b c
```

With a bit of imagination, you can think to those white spaces as an
native F# pseudo-operator, as we did with `map`. We got to:


```fsharp
let dP = f <!> aP ...
```

The idea is to keep running function application using an improved,
Parser-powered `<*>` operator:

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


 If you don't like the fact that `<*>` is used for all the arguments
but the first one, you might prefer this alternative syntax:

```fsharp
let dP: 'd Parser  = pure' f <*> aP <*> bP <*> cP
```

It's not hard to verify that it's completely equivalent. Anyway,
implementing `<*>` is not hard at all. You just have to be driven by
the type signature. Let's start with the simplest case of 1-parameter
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

What will this give us back? Let's think about it. Naturally, we
cannot get back a `'b` value: a Parser is a promise of a value, so if
we give parsers it's fair to be paid back with other parsers. It's
legitimate to assume we get back a `'b Parser`:


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
As I anticipated, it turns our that the Applicative Functor's `<*>`
operator cannot be built in terms of humble Functor's `map`. This
could even be demonstrated mathematically. Even further: we can easily
show that, since Applicative Functors are more powerful than Functors,
we can rewrite `map` in terms of `pure'` and `ap`:

```fsharp
let map f aP = pure' f <*> aP
```

This `map` implementation may look obscure, but it is in fact very
logic.  If you compare the signatures of `map` and `ap`:


```fsharp
val map:  ('a -> 'b)        -> 'a Parser -> 'b Parser
val ap:   ('a -> 'b) Parser -> 'a Parser -> 'b Parser
```

you see that the only difference is that in `map` the `f` parameter is
not wrapped inside a Parser. So, if you lift `f` inside a parser with
`pure' f`, you would get exactly the `ap` signature; that is, you can
proceed applying `ap` to the `'a Parser` argument.

Try running all the past tests you wrote so far. Woah! What a
beautiful display of green! It seems that with `pure'` and `<*>` you
really discovered something deep.

## Apply, Apply, Apply!
We opened this chapter claiming that there is something magic about
the native F# function application, because you can apply a function
returning a function returning a function &mdash; ad nauseam &mdash;
thanks to the unsuspectedly powerful whitespace pseudo-operator.

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

Amazing! It works!

## Applicative Functors
I did not introduce `pure'` as a coincidence: in fact, the combination
of `pure'` and `<*>` is what conventionally defines Applicative
Functors. There are 2 possible interpretation of Applicative
Functors. The first is about what we have just experimented: seeing
them as an extension of function application. It should not come as a
surprise that we have made every effort to ensure that using
Applicative Functors resembled just applying functions:

```fsharp
let d:  'd         = f     a      b      c
let dP: 'd Parser  = f <!> aP <*> bP <*> cP
```

I have always been fascinated by the way the [Idris] programming
language took this interpretation to the extreme, with Conor McBride's
[Idiom Brackets][idris-idiom-brackets]:

```idris
d = [| f aP bP cP |]
```

Like this, it really seems an ordinary function application! But
enough for now. Time to get our hands dirty building some real parsers
with `<*>` and `pure'`, and to play with the second interpretation of
Applicative Functors, that has to do with the notion of lifting
functions. Treat yourself with a rösti and get prepared to [Chapter
11](/monadic-parser-combinators-11).

[Previous - Things You Don't Care About](/monadic-parser-combinators-9) ⁓
[Next - Lifting Functions](/monadic-parser-combinators-11)

# References

- [Idris][idris]
- [Idris - Idiom Brackets][idris-idiom-brackets]

[idris]: https://idris-lang.org/
[idris-idiom-brackets]: https://docs.idris-lang.org/en/latest/tutorial/interfaces.html?highlight=idiom%20bracket#idiom-brackets

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

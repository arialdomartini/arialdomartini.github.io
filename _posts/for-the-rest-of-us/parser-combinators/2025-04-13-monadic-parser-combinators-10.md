---
layout: post
title: "Monadic Parser Combinators in F# - Applying functions, ad nauseam"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
- property-based testing
include_in_index: false
---
There is something magic about the native F# function application:
once you apply a function to an argument, if the result is another
function you can just run function application again. It will work.
And if you get another function, you can do the same, ad nauseam.  
See this example:

```fsharp
[<Fact>]
let ``function application with 2 parameters`` () =

    let fa: int  -> (bool -> string) = fun a -> fun b -> $"{a}, {b}"
    let fb:          bool -> string  = fa 42
    let c:                   string  = fb true

    test <@ c = "42, True" @>
```

- `f` is a function that takes `a`,  an `int`, and that returns
  another function, `bool -> string`.
- If you use the native function application to apply `f` to `42`, you
  get that `bool -> string` function back.
- Now, thare's nothing special in that `bool -> string` function. So,
  you can keep using the F# native function application to pass it the
  next argument, a `true` value.
  
It's easy to see how this does not stop with 2-parameter functions.
Here's a test for a 3-parameter function:

```fsharp
[<Fact>]
let ``function application with 3 parameters`` () =

    let fa: int  -> (bool -> (string -> string)) = fun a -> fun b -> fun c -> $"{a}, {b}, {c}"
    let fb:          bool -> (string -> string)  = fa 42
    let fc:                   string -> string = fb true
    let d:                              string = fc "foobar"

    test <@ d = "42, True, foobar" @>
```


Now:

```fsharp
let f:  int  -> (bool -> (string -> string)) = fun a -> fun b -> fun c
-> $"{a}, {b}, {c}"
```

is a very verbose way to define a function returning a function, in
turn, returning a function. F# lets you:

- remove the parenthesis from the signature, since function
  application associates to the right:


```fsharp
let f:  int  -> bool -> string -> string = fun a -> fun b -> fun c
-> $"{a}, {b}, {c}"
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

Ah, much better! But, notice: this is somehow just syntactic sugar.
This is still a function returning a function &mdash; in turn,
returning a function.

## A crocked Function Application
What about the Parser-Powered Function Application `<<|` that we have
so proudly distilled in [Chapter 7](/monadic-parser-combinators-7)?
Can we also apply it repeatedly?

Let's see. We take a generic 3-parameter function `'a -> 'b -> 'c ->
'd`. In this context, we are not concerned how it is implemented, we
can just focus on the signature:


```fsharp

[<Fact>]
let ``Parser-powered function application with 3 parameters`` () =

    let f (a: int) (b: 'b) (c: 'c) = 
        failwith "Not yet implemented"
    ...

```

Then, we apply it to an argument `'a Parser`, of course using `map`.
Let me use the symbol `<!>` instead of `<<|` or `map`; we mentioned
that they are all synonyms:

```fsharp
let (<!>) = map

[<Fact>]
let ``Parser-powered function application with 3 parameters`` () =

    let a: 'a Parser = failwith "Not yet implemented"

    let f (a: int) (b: 'b) (c: 'c) =
        failwith "Not yet implemented"
    
    let fa: ('b -> 'c -> 'd) Parser = f <!> a
    
    ...
```

Oh no! Look the signature of `fa`! The result is not just another
function with 1 parameter less, like it happened before . It's not
even a function anymore: it's a function wrapped inside a Parser. This
means that we cannot apply `<!>` again... We definitely need a
different operator.

If `map` is of little help, it's time to invent a more powerful
version of Functors: enter Applicative Functors.


## Beyond Functors

I'm sure by now you know what is going to happen: we implement a new
operator as an exercise of being driven by the type signature. And
then we invent yet another symbol. You are right. We will invent `<*>`
which, with a burst of creativity, we are going to call "apply" or
`ap`.

We left with this:

```fsharp
let fa: ('b -> 'c -> 'd) Parser = f <!> a
```

Notice: it's a 2-parameter function inside a Parser.

Ideally, we wanted to apply the next argument `'b Parser` to `fa`,
then the next `'c Parser`:

[<Fact>]
let ``Parser-powered function application with 3 parameters`` () =

    let a: 'a Parser = failwith "Not yet implemented"
    let b: 'b Parser = failwith "Not yet implemented"
    let c: 'c Parser = failwith "Not yet implemented"

    let f (a: int) (b: 'b) (c: 'c) =
        failwith "Not yet implemented"
    
    let fa: ('b -> 'c -> 'd) Parser = f  <!> a
    let fb:       ('c -> 'd) Parser = fa <*> b
    let fc:              'd  Parser = fb <*> c
    ...
```


If you are confused, let me reformat this in a more streamlined way.

With the native F# function application you have:

```fharp
val a: 'a
val b: 'b
val c: 'c

val f: 'a -> 'b -> 'c -> 'd
```

and you can apply `f` to arguments just separating them with white
spaces (the pseudo-operator for the F# native function application):


```fsharp
let d = f a b c
```

In the Parser world, you have to use `<!>` first, then you end up with
a Parser-wrapped function, so you have to continue with this
yet-to-be-implemented `<*>` apply operator. 

```fsharp
let dP = f <!> aP <*> bP <*> cP
```

You see the equivalence?


```fsharp
let d:  'd         = f     a      b      c
let dP: 'd Parser  = f <!> aP <*> bP <*> cP
```


Basically, we are writing an enhanced version of whitespace.


So, let's be driven by the type signature! Let's start with the case
of 1-parameter functions. `ap` / `<*>` is that operator that given a
1-parameter function wrapped in a Parser:

```fsharp
val ap : ('a -> 'b) Parser -> ...

let (<*>) = ap
```

lets us apply the `'a -> 'b` wrapped function to an `'a Parser`
argument:


```fsharp
val ap : ('a -> 'b) Parser -> 'a Parser -> ...
```

to give us back what? Let's think about it. We cannot get back a `'b`
value, right? A Parser is a promise of a value, so if we give parsers
it's fair to be paid back with parsers. It's fair to get back a `'b
Parser`:


```fsharp
val ap : ('a -> 'b) Parser -> 'a Parser -> 'b Parser
```

Let's implement it:


```fsharp
let ap fP aP = failwith "Not yet implemented"

let (<*>) = ap

[<Fact>]
let ``ap with a 1-parameter function`` () =
    let aP = Parser (fun input -> Success (input, 42))

    let f a = a * 2

    let fP = Parser (fun input -> Success (input, f))

    test <@ run (fP <*> aP) "some input" = Success ("some input", 84)@>
```

Notice that in the test we have created 2 super simple parsers, which
do not even consume the input. `aP` just parses `42`, `fP` just
returns a Parser-wrapped `fun i -> i * 2` function.

For cases like these, let's have a convenience function `pure'`,
which takes whatever value and wraps it into a doing-nothing Parser:

```fsharp
let pure' a = Parser (fun input -> Success (input, a))

// ('a -> 'b) Parser -> 'a Parser -> 'b Parser
let ap fP aP = failwith "Not yet implemented"

[<Fact>]
let ``ap with a 1-parameter function`` () =
    let aP = pure' 42

    let fP = pure' (fun a -> a * 2)

    test <@ run (fP <*> aP) "some input" = Success ("some input", 84)@>
```

You know how to proceed. The signature of `ap` tells you to return a
`'b Parser`. Just build one:

```fsharp
let ap fP aP = Parser (fun input ->
    ...)
```

Now, we have a function `f` inside a Parser and the argument `a` also
inside a Parser. You have an `input`. Using the box analogy, it seems
that solving this riddle is a matter of:

- opening both the boxes;
- extracting the contained `f` and `a`;
- applying `f` and `a`;
- possibly giving up in case of failure;
- and passing the unconsumed input around.

Conventionally, the first box to open is the one containing the
function:

```fsharp
let ap fP aP = Parser (fun input ->
    match run fP input with
    | Failure e ->  Failure e
    | Success (rf, f) ->
        ...
```

Of course, if we get an error, we give up. If we are successful, we
get the inner function `f` the unconsumed input `rf` ("rest of `f`").
Fine, we have all the ingredients to open the `a` box, now:

```fsharp
let ap fP aP = Parser (fun input ->
    match run fP input with
    | Failure e ->  Failure e
    | Success (rf, f) ->
        match run aP rf with
        | Failure s -> Failure s
        | Success (ra, a) -> ...
```

Cool. We have `f`, we have `a` and the unconsumed input `ra`.
Time to finally apply `f` to `a`, and to combine the result in a
success:

```fsharp
let ap fP aP = Parser (fun input ->
    match run fP input with
    | Failure e ->  Failure e
    | Success (rf, f) ->
        match run aP rf with
        | Failure s -> Failure s
        | Success (ra, a) -> Success (ra, f a))
```

Green test.

Wait a second: why did we have to pattern match and to pass unconsumed
input around? Didn't we say that we could always build on top of the
previous building blocks?  
It turns our that the Applicative Functor's `<*>` operator you just
distilled cannot be built in terms of Functor's `map`. On the
contrary: we can demonstrate that since Applicative Functors are more
powerful than Functors, we can rewrite `map` in terms of `pure'` and
`ap`:

```fsharp
let map (f: 'a -> 'b) (a: 'a Parser) : 'b Parser =
    pure' f <*> a
    
let (<<|) = map
let (<!>) = map
```

Makes sense, right? `map` has the same signature of `<*>`, with
the only difference that `f` is not wrapped inside a Parser. So, if
you lift `f` inside a parser with `pure' f`, you can proceed applying
it to the `'a Parser` argument with `<*>`.

Try running all the past tests you wrote: woah! What a beautiful
display of green! It seems that with `pure'` and `<*>` you really
discovered something deep.

## Apply, apply, apply
We opened this chapter claiming that there is something magic about
the native F# function application, because you can apply a function
returning a function returning a function &mdash; ad nauseam &mdash;
to arguments, and the unsuspectedly powerful whitespace
pseudo-operator will happily do its work.

Does your brand new `<*>` have the same super-power?

Let's see. We want to have the equivalent of this test:

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

Amazing! It works! You have actually invented the most generic
Parser-powered function application ever. Now you are really ready to
fly! Let's see how to build real powerful parser with it.




# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

---
layout: post
title: "Monadic Parser Combinators in F# - Applying Functions, Ad Nauseam"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
- property-based testing
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
- Now, thare's nothing special in that `bool -> string` function. It
  is just another function. So, you can keep using the F# native
  function application to pass it the next argument, a `true` value,
  which finally gets you back a `string`.
  
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

Ah, much better! But, notice: it's just syntactic sugar. This is still
a function returning a function &mdash; in turn, returning a function.

## A crocked Function Application
What about the Parser-Powered Function Application `<<|` that we have
so proudly distilled in [Chapter 7](/monadic-parser-combinators-7)?
Can we also apply it *ad nauseam*?

Let's see. We start from a generic 3-parameter function `'a -> 'b ->
'c -> 'd`. In this context, we are not concerned how it is
implemented, we can just focus on its signature:


```fsharp

[<Fact>]
let ``Parser-powered function application with 3 parameters`` () =

    let f (a: int) (b: 'b) (c: 'c) = 
        failwith "Not yet implemented"
    ...
```

Then, we apply it to an argument `'a Parser`, of course using `map`.
Let me use the symbol `<!>` instead of `<<|` or `map`; after all, we
mentioned that they are all synonyms:

```fsharp
let (<!>) = map

[<Fact>]
let ``Parser-powered function application with 3 parameters`` () =

    let a: 'a Parser = 
        failwith "Not yet implemented"

    let f (a: int) (b: 'b) (c: 'c) =
        failwith "Not yet implemented"
    
    let fa: ('b -> 'c -> 'd) Parser = 
        f <!> a
    
    ...
```

Oh no! Look at `fa`'s signature! The result is not just another
function with 1 parameter less, like it happened before . It's not
even a function anymore: it's a function wrapped inside a Parser.

This all makes sense, if you think what `<!>` does. Applying it to an
`'a -> 'b` function would get you this:

<p align="center">
  <img src="static/img/parser-combinators-for-the-rest-of-us/map-ap-part-1.png"
  alt="" height="350px">
</p>

It's easy to see what happens when you apply it to a 2-parameter
function `'a -> 'b -> 'c`, if you think to it as a 1-parameter
function `'a -> ('b -> 'c)` which just happens to return another
function. Then:

<p align="center">
  <img src="static/img/parser-combinators-for-the-rest-of-us/map-ap-part-2.png"
  alt="" height="350px">
</p>


This means that we cannot apply `<!>` again... We definitely need a
different operator.

If Functor's `map` is of little help, it's time to invent a more
powerful version of Functors: enter Applicative Functors.


## Beyond Functors

I'm confident you can guess our next step: we will implement a new
operator letting its signature lead the way. And then we will invent
yet another symbol. True. Here's the spoiler: we will distill `<*>`
which, with a burst of creativity, we are going to call "apply" or
`ap`.

We left with this:

```fsharp
let fa: ('b -> 'c -> 'd) Parser = f <!> a
```

Notice: it's a 2-parameter function inside a Parser. We want to apply
it to the next argument, a `'b Parser`:

[<Fact>]
let ``Parser-powered function application with 3 parameters`` () =

    let a: 'a Parser = failwith "Not yet implemented"
    let b: 'b Parser = failwith "Not yet implemented"
    let c: 'c Parser = failwith "Not yet implemented"

    let f (a: int) (b: 'b) (c: 'c) =
        failwith "Not yet implemented"
    
    let fa: ('b -> 'c -> 'd) Parser = f  <!> a
    let fb:       ('c -> 'd) Parser = fa <*> b  // <-- this
    let fc:              'd  Parser = fb <*> c  // <-- then this
    ...
```

If this is confusing, let me reformat it in a more streamlined way.

With the native F# function application you have:

```fharp
val a: 'a
val b: 'b
val c: 'c

val f: 'a -> 'b -> 'c -> 'd
```

and you can apply `f` to arguments just by separating them with white
spaces (the F# native function application pseudo-operator):


```fsharp
let d = f a b c
```

In the Parser world, you have to use `<!>` for the first argument,
which gives you back a Parser-wrapped function. So, you have to
continue with this yet-to-be-implemented `<*>` apply operator.

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


So, let's be driven by the type signature! We start with the case of
1-parameter functions. `ap` / `<*>` is that operator that given a
1-parameter function wrapped in a Parser:

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
let ap fP aP =
    failwith "Not yet implemented"

let (<*>) = ap

[<Fact>]
let ``ap with a 1-parameter function`` () =

    let aP = Parser (fun input -> Success (input, 42))

    let f a = a * 2

    let fP = Parser (fun input -> Success (input, f))

    test <@ run (fP <*> aP) "some input" = Success ("some input", 84)@>
```

Notice that in the test we have created 2 super simple parsers, which
do not even consume the input. `aP` just returns a Parser-wrapped
`42`, `fP` a Parser-wrapped `fun i -> i * 2`.

For cases like these, let's have a convenience function `pure'`, which
takes whatever value you give it in and wraps it into a doing-nothing
Parser:

```fsharp
let pure' a = Parser (fun input -> Success (input, a))

// ('a -> 'b) Parser -> 'a Parser -> 'b Parser
let ap fP aP =
    failwith "Not yet implemented"

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
    | Success (rf, f) ->
        ...
```

Of course, if we get an error, we give up. If we are successful, we
get the inner function `f` the unconsumed input `rf` ("rest of `f`").
Fine, we have all the ingredients to open the `a` box:

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
Time to finally apply `f` to `a`, and to wrap the result in a
`Success`:

```fsharp
let ap fP aP = Parser (fun input ->
    match run fP input with
    | Failure e ->  Failure e
    | Success (rf, f) ->
        match run aP rf with
        | Failure s -> Failure s
        | Success (ra, a) -> Success (ra, f a))
```

Green tests.

Wait a second: why did we have to pattern match and to pass unconsumed
input around? Didn't we say that we could always build on top of the
previous building blocks?  
It turns our that the Applicative Functor's `<*>` operator you just
invented cannot be built in terms of humble Functor's `map`. We could
even demonstrate it mathematically. On the contrary: we can
mathematically demonstrate that, since Applicative Functors are more
powerful than Functors, we can alway rewrite `map` in terms of `pure'`
and `ap`:

```fsharp
let map (f: 'a -> 'b) (a: 'a Parser) : 'b Parser =
    pure' f <*> a
    
let (<<|) = map
let (<!>) = map
```

Read `map`'s implementtion. It makes sense, right? `map` has the same
signature of `<*>`, with the only difference that `f` is not wrapped
inside a Parser. So, if you lift `f` inside a parser with `pure' f`,
you can proceed applying it to the `'a Parser` argument with `<*>`.

Try running all the past tests you wrote so far. Woah! What a
beautiful display of green! It seems that with `pure'` and `<*>` you
really discovered something deep.

## Apply, apply, apply
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
Parser-powered function application. I did not introduce `pure'` as a
coincidence: in fact, the combination of `pure'` and `<*>` is what
conventionally defines Applicative Functors.
from discovering monads.

Let's put `<*>` into play to build real powerful parsers.


## Fancy dates

Do you remember when in [Chapter 6](/monadic-parser-combinators-6) we
fantasized on the syntax:

```
7 times date{16/03/1953}
```

to build a list of `7` dates boxed inside a `Foo` object? Such a
stunning feature deserves a parser, `fooP`, and Applicative Functors
can make it happen. Let's build it top-down. Of course, you start from
a test:


```fsharp
type Foo = Foo of (DateOnly list)

let fooP =
  failwith "Not yet implemented"

[<Fact>]
let ``parses a Foo`` () =
  let input = "7 times date{16/03/1953} the rest"

  let date = DateOnly(1953, 03, 16)
  test <@ run fooP input =
         Success (" the rest", Foo [date; date; date; date; date; date; date]) @>
```

As for the implementation, you can fully apply the idea of building
complex parsers as a combination of simpler parsers, and thos on more
simpler parsers, down to the smallest you can create.

The idea is to split the input `7 times date{16/03/1953}` into its syntactical components:

- `7`: the number.
- ` times `: one of your language's commands.
- `date{16/03/1953}` to be parsed as `DateOnly(1953, 03, 16)`.

and to define a builder function in terms of the values, not the
parsers. 

```fsharp
let makeFoo (n: int) (_command: string) (date: DateOnly) : Foo =
    let dates = [ for i in 0 .. n - 1 -> date ]
    Foo dates
```

Notice that `_command` is ignored. In fact, we want the parser to
ensure that the required string is found, but not to include it in the returned value.  
Then, we apply `makeFoo` to parsers of values &dmash; instead of to
values &mdash; using `<!>` and `<*>`:

```fsharp
let intP: int Parser = 
    failwith "Not yet implemented"

let times = (str " times ")

let fancyDate: DateOnly Parser = 
    failwith "Not yet implemented"

let fooP =
  makeFoo <!> intP <*> times <*> fancyDate
```

Good. This is done. Of course, proceeding top-down, we need to build
the underlying parsers. Let's go with the fancy date syntax. To parse
`date{16/03/1953}` as a `DateOnly` we need to make this test pass:

```fsharp
let fancyDate: DateOnly Parser =
    failwith "Not yet implemented"


[<Fact>]
let ``parses the fancy date syntax`` () =
  let input = "date{16/03/1953} the rest"

  test <@ run fancyDate input = Success(" the rest", DateOnly(1953, 03, 16)) @>
```

You have all the necessary building blocks, if you observe that
`date{16/03/1953}` can be thought as:

- the date `16/03/1953`
- surrounded by `{` and `}`
- and prefixed by `date`, whose value can be ignored.

You can use `between` for `{` and `}`, and `>>.` for parsing & ignoring `date`:


```fsharp
let openBrace = str "{"
let closeBrace = str "}"

let date: DateOnly Parser =
    failwith "Not yet implemented"

let fancyDate =
    str "date" >>. (date |> between openBrace closeBrace)

[<Fact>]
let ``parses the fancy date syntax`` () =
  let input = "date{16/03/1953} the rest"

  test <@ run fancyDate input = Success(" the rest", DateOnly(1953, 03, 16)) @>
```

Going deeper, it's `date`'s turn. `date` is the parser for the inner
`16/03/1953` syntax. It can be defined using an Applicative Functor,
feeding a factory method `makeDateOnly`:

```fsharp
let digitsP (nDigits: int) : int Parser = 
    failwith "Not implemented"

let slash = str "/"
let day = digitsP 2
let month = digitsP 2
let year = digitsP 4

let makeDateOnly day _slash1 month _slash2 year =
  DateOnly(year, month, day)

let dateP =
    makeDateOnly <!> day <*> slash <*> month <*> slash <*> year


[<Fact>]
let ``parses the date part`` () =
  let input = "16/03/1953 the rest"

  test <@ run dateP input = Success(" the rest", DateOnly(1953, 03, 16)) @>
```

Dropping down another level, we have to define `digitsP`. This
function takes the expected number of digits and returns an `int
Parser` to parse a number with exactly that many digits.

At this stage, we haven't yet developed the ideal building blocks to
make this parser elegant. We could try anyway with our current tools:

```fsharp
let digitsP nDigits = Parser (fun input ->
    try
        Success(input[nDigits..], Int32.Parse(input[..nDigits-1]))
    with
    | _ -> Failure "Could not parse the int")

[<Fact>]
let ``parses numbers with a specific number of digits`` () =

    test <@ run (digitsP 1) "9" = Success ("", 9)@>
    test <@ run (digitsP 2) "42" = Success ("", 42)@>
    test <@ run (digitsP 2) "42 the rest" = Success (" the rest", 42)@>
    test <@ run (digitsP 4) "1942 the rest" = Success (" the rest", 1942)@>
    test <@ run (digitsP 4) "19429 the rest" = Success ("9 the rest", 1942)@>

    test <@ run (digitsP 4) "19 the rest" = Failure ("Could not parse the int")@>
    test <@ run (digitsP 4) "foo bar baz" = Failure ("Could not parse the int")@>
```


We will improve this parser as we go. Incidentally, this is a key
feature of Parser Combinators: because of their recursive design,
improvements of any low level building block will positively propagate
to the parsers built on top of them. So, don't stress too much over
the result, for now; we'll soon make it better.


We are left with one last building block to write: `intP`. It is
supposed to parse the `7` in `7 times date{16/03/1953}`. We can just
use `digitsP 1`, can't we?  
Sure. But what if it's `42` or `42000` instead of `7`? With the date
we were lucky that the day was always a `2` digits number, and the
year always a `4` digits one. Here we are facing a new challenge: how
to parse an integer with an unknown number of digits? This require a
new tool in our toolbelt, something giving our parsers the ability to
*try* a parsing, and to eventually recover from a failure. We will
cover exactly this in the next chapter. Let's close this one with one
last twist: let's invent the notion of *lifting functions*.

## Lifting functions
We learnt that a function taking values can be applied to parsers of
those values by the means of replacing the  white-space pseudo-operator
with `<!>` and `<*>` like this:

```fsharp
let f:  'value         = f     a      b      c
let fP: 'value Parser  = f <!> aP <*> bP <*> cP
```

In a sense, the combination of `<!>` and `<*>` elevates a function
from the value realm to the parser world, during function application.
It would be nice to have an operator to perform that elevation before
hand, even before function application. In other words, we want to convert:

```fsharp
f:   a -> b -> c -> d
```

into:

```fsharp
fP: 'a Parser -> 'b Parser -> 'c Parser -> 'd Parser
```


<p align="center">
  <img src="static/img/parser-combinators-for-the-rest-of-us/lift3.png" alt="lift3 function" height="350px">
</p>

That's trivial! We don't even need a test for this, type checking will
suffice:

```fsharp
let lift3 f a b c = f <!> a <*> b <*> c 
```

`lift3` comes in handy to simplify the syntax. Refactoring the `fooP`
parser above, instead of:

```fsharp
let fooP =
  makeFoo <!> intP <*> times <*> fancyDate
```

you can just write:

```fsharp
let makeFooP = lift3 makeFoo

let fooP = makeFooP intP times fancyDate
```

It's like writing parser-powered code while removing all the parser
boilerplate from sight, so to get back the original linear, pure code.
Sweet!

As the name suggests, `lift3` works for 3-parameter functions. For
2-parameter functions `lift2` is similarly defined as:

```fsharp
let lift2 f a b = f <!> a <*> b
```

<p align="center">
  <img src="static/img/parser-combinators-for-the-rest-of-us/lift2.png" alt="lift2 function" height="350px">
</p>

Removing 1 parameter more, it's easy to define `lift`, for lifting
1-parameter functions:

```fsharp
let lift f a = f <!> a
```

<p align="center">
  <img src="static/img/parser-combinators-for-the-rest-of-us/lift.png" alt="lift function" height="350px">
</p>


But look! *Eta-reducing* this expression &mdash; that is, removing `a`
and `f` from both sides &mdsah; it's easy to see that `lift` is in
fact our old friend `map`:

```fsharp
let lift = (<!>)
```


<p align="center">
  <img src="static/img/parser-combinators-for-the-rest-of-us/map.png" alt="map function" height="350px">
</p>

Given the diagram, it all makes sense.

Enough for now. Let's take a break: you deserve a pistacchio kulfi to
refresh your mind.  
See you later in [Chapter 11](/monadic-parser-combinators-11).

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

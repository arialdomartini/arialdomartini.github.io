---
layout: post
title: "Monadic Parser Combinators in F# - Lifting Functions"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
include_in_index: false
---
## Fancy dates

Do you remember when in [Chapter 6](/monadic-parser-combinators-6) we
fantasized about the syntax:

```
7 times date{16/03/1953}
```

to build a list of `7` dates boxed inside a `MultiDate` object? Such a
stunning feature deserves a parser, and Applicative Functors can make
it happen. Let's design it top-down, fully applying the idea that
complex parsers can be thought as a combination of even simpler
parsers, down to the smallest parsers you can create. Of course, we
start from a test:


```fsharp
type MultiDate = MultiDate of DateOnly list

let multiDateP: MultiDate Parser = __

[<Fact>]
let ``parses a MultiDate`` () =
    let input = "7 times date{16/03/1953} the rest"

    let date = DateOnly(1953, 03, 16)
    test <@ run multiDateP input =
        Success(MultiDate [ date; date; date; date; date; date; date ], " the rest") @>
```

Applicative Parsers let you think in terms of values, rather than in
terms of parsers, and then feed your functions with parsers using
`<!>` and `<*>`. So, the idea is to split the input `7 times
date{16/03/1953}` into its syntactical components:

- `7`: the number.
- ` times `: one of your language's commands.
- `date{16/03/1953}` to be parsed as `DateOnly(1953, 03, 16)`.

and to define a builder function in terms of those ordinary parsed
values, postponing the problem of defining their parsers:

```fsharp
let makeMultiDate (n: int) (_command: string) (date: DateOnly) : MultiDate =
    let dates = List.replicate n date
    MultiDate dates
```

Note that `_command` is ignored. In fact, we want the parser to
make sure the required string is found, but not to include it in the returned value.  
Armed with the factory `makeMultiDate`, we can apply it to parsers
using `<!>` and `<*>`:

```fsharp
let intP: int Parser = __
let times: string Parser = __
let fancyDate: DateOnly Parser = __

let multiDateP =
  makeMultiDate <!> intP <*> times <*> fancyDate
```

Since we are proceeding top-down, we can proceed building the
underlying parsers. `times` is trivial, it's a string parser for the
hardcoded value `" times "`:

```fsharp
let times = str " times "
```


Let's go with `fancyDate` next. To parse `date{16/03/1953}` as a
`DateOnly` we need to make this test pass:

```fsharp
let fancyDate: DateOnly Parser = __


[<Fact>]
let ``parses the fancy date syntax`` () =
  let input = "date{16/03/1953} the rest"

test <@ run fancyDate input = Success(DateOnly(1953, 03, 16), " the rest") @>
```

You have all the necessary building blocks, if you observe that
`date{16/03/1953}` can be thought as:

- the date `16/03/1953`
- surrounded by `{` and `}`
- and prefixed by `date`, whose value can be ignored.

You can use `between` for `{` and `}`, and `>>.`:


```fsharp
let openBrace = str "{"
let closeBrace = str "}"

let date: DateOnly Parser =  __

let fancyDate =
    str "date" >>. (date |> between openBrace closeBrace)

[<Fact>]
let ``parses the fancy date syntax`` () =
  let input = "date{16/03/1953} the rest"

  test <@ run fancyDate input = Success(DateOnly(1953, 03, 16), " the rest") @>
```


Here we assume we are using the already implemented `>>.` and
`between`. But if you think about it, they are both very simple to
define, using an Applicative Functor:

```fsharp
let (>>.) left right =
    let takeRight _ right = right
    takeRight <!> left <*> right

let between openTagP closedTagP contentP =
    let buildBetween _ content _ = content
    buildBetween <!> openTagP <*> contentP <*> closedTagP

```

In both cases, it's a matter of writing an ordinary factory function,
and then of applying it to parsers, using `<!>` and `<*>`. It's alway
the same trick, over and over.  
Going deeper, it's `date`'s turn. `date` is the parser for the inner
`16/03/1953` syntax. It can be defined &mdash; guess how? &mdash;
using an Applicative Functor, feeding its factory method
`makeDateOnly`:

```fsharp
let digitsP (nDigits: int) : int Parser = __

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

  test <@ run dateP input = Success(DateOnly(1953, 03, 16), " the rest") @>
```

Dropping down another level (we are almost done!) we have to define
`digitsP`. This function takes the expected number of digits and
returns an `int Parser` to parse a number with exactly that many
digits. At this stage, we haven't yet developed the ideal building
blocks to make this parser elegant. Let me cheat using `Int32.Parse`:

```fsharp
let digitsP nDigits = Parser (fun input ->
    try
        Success(Int32.Parse(input[..nDigits-1]), input[nDigits..])
    with
    | _ -> Failure "Could not parse the int")

[<Fact>]
let ``parses numbers with a specific number of digits`` () =

    test <@ run (digitsP 1) "9" = Success (9, "")@>
    test <@ run (digitsP 2) "42" = Success (42, "")@>
    test <@ run (digitsP 2) "42 the rest" = Success (42, " the rest")@>
    test <@ run (digitsP 4) "1942 the rest" = Success (1942, " the rest")@>
    test <@ run (digitsP 4) "19429 the rest" = Success (1942, "9 the rest")@>

    test <@ run (digitsP 4) "19 the rest" = Failure "Could not parse the int"@>
    test <@ run (digitsP 4) "foo bar baz" = Failure "Could not parse the int"@>
```


We will improve this parser in the next chapters. Incidentally, this
is a key feature of Parser Combinators: because of their recursive
design, improvements of any low level building block will positively
propagate to all the parsers built on top of it. So, don't stress too
much over the result, for now; we'll soon make it better.


We are left with one last building block to write: `intP`. It is
supposed to parse the `7` in `7 times date{16/03/1953}`. We can just
use `digitsP 1`, can't we?

```fsharp
let intP: int Parser = digitsP 1
```

Sure. But this is a dirty trick. How do you know that the number is a
1-digit one? What if the input string had `42` or `42000` instead of
`7`? Parsing dates was an easy task, because the day's and the month's
parts were always `2` digits numbers, year's part always a `4` digits
one. Here we are facing a new challenge: how to parse an integer with
an unknown number of digits? We don't know yet how to deal with
unknown things. This require a new tool in our toolbelt, something
giving our parsers the ability to *try* a parsing, and to eventually
recover from a failure. We will cover exactly this in the next
chapter. Instead, let's close this one with a last twist: let's invent
the notion of *lifting functions*, a possible second interpretation of
Applicative Functors.

## Lifting functions
We learnt that a function taking values can be applied to parsers of
those values by the means of replacing the white-space pseudo-operator
with `<!>` and `<*>` like this:

```fsharp
let f:  'value         = f     a      b      c
let fP: 'value Parser  = f <!> aP <*> bP <*> cP
```

In a sense, the combination of `<!>` and `<*>` elevates a function
from the world of ordinary values to the parser world. This lifting
happens as we provide one argument after the other. It would be nice
to have an operator to perform that lifting beforehand, even before we
have an argument to feed the function with. In other words, it would
be amazing if we could convert:

```fsharp
f:   a -> b -> c -> d
```

into:

```fsharp
fP: 'a Parser -> 'b Parser -> 'c Parser -> 'd Parser
```

in one shot. That's the work for `lift3`:

<p align="center">
  <img src="static/img/parser-combinators-for-the-rest-of-us/lift3.png" alt="lift3 function" height="350px">
</p>

But implementing it is a piece of cake! We don't even need a test for
this, type checking will suffice:

```fsharp
let lift3 f =
    fun a b c -> f <!> a <*> b <*> c 
```

or, more concisely:

```fsharp
let lift3 f a b c = f <!> a <*> b <*> c 
```

`lift3` comes in handy to simplify some expressions. For example,
instead of:

```fsharp
let multiDateP =
    makeMultiDate <!> intP <*> times <*> fancyDate
```

you can just write:

```fsharp

let multiDateP = 
    (lift3 makeMultiDate) intP times fancyDate
```

It's like writing parser-powered code while removing all the
boilerplate code from sight, so to get back the original linear, pure
code. Sweet!  
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


But look! [η-reducing][eta-conversion] this expression &mdash; that
is, removing `a` and `f` from both sides &mdash; it's easy to see that
`lift` is in fact our old friend `map`:

```fsharp
let lift = map
```


<p align="center">
  <img src="static/img/parser-combinators-for-the-rest-of-us/map.png" alt="map function" height="350px">
</p>

Given the diagram, it all makes sense.

That'll do for now! Let's take a break: you deserve a pistacchio kulfi to
refresh your mind.  
See you later in [Chapter 12](/monadic-parser-combinators-12).


[Previous - Applying Functions, Ad Nauseam](/monadic-parser-combinators-10) ⁓
[Next - Things You Are Not Sure About](/monadic-parser-combinators-12)


# References

[Eta Reduction][eta-conversion]

[eta-conversion]: https://wiki.haskell.org/Eta_conversion

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

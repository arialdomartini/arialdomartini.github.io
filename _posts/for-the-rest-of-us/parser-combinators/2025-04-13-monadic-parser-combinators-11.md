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

let fooP = __

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

Note that `_command` is ignored. In fact, we want the parser to
ensure that the required string is found, but not to include it in the returned value.  
Then, we apply `makeFoo` to parsers of values &dmash; instead of to
values &mdash; using `<!>` and `<*>`:

```fsharp
let intP: int Parser = __

let times = (str " times ")

let fancyDate: DateOnly Parser = __

let fooP =
  makeFoo <!> intP <*> times <*> fancyDate
```

Good. This is done. Of course, proceeding top-down, we need to build
the underlying parsers. Let's go with the fancy date syntax. To parse
`date{16/03/1953}` as a `DateOnly` we need to make this test pass:

```fsharp
let fancyDate: DateOnly Parser = __


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

let date: DateOnly Parser =  __

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
See you later in [Chapter 12](/monadic-parser-combinators-12).

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

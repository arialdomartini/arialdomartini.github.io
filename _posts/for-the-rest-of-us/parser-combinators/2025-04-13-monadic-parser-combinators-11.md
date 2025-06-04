---
layout: post
title: "Monadic Parser Combinators in F# - Things You Are Not Sure About"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
- property-based testing
include_in_index: false
---
There are only 2 important missing features in the Parser Combinator
library you are building:

- Context Sensitivity.
- Backtracking.

*Context Sensitivity* is the ability of a parser to reference the
result of other parsers. Basically, it's equivalent to equipping
parsers with a form of memory. This capability will allow you to parse
more complex languages, and will dramatically change the way you write
parsers.  
As you have surely guessed already, this is about introducing monads.
We will get there in the very next chapter.


*Backtracking* is the ability to recover from errors and to explore
alternative parsing paths. This is necessary for the last use case we
encountered: parsing an integer of an unknown number of digits.

Let's start from the latter.

## Alternative

The basic operator for implementing backtracking is `<|>`: its purpose
is to try the parser on its left, first; if that parser fails, instead
of propagating the error, it *backtracks* trying the parser on the
right, using the original input. If both parsers fail, only then a
parsing error is returned. The implementation is straightforward.

```fsharp
let orElse tryFirst fallback  =
    Parser (fun input ->
        match run tryFirst input with
        | Success _ as first -> first
        | Failure _ -> run fallback input)

let (<|>) = orElse


type SomeResult = One | Two

[<Fact>]
let ``applies the first parser if successful`` () =
    let firstParser = Parser (fun _ -> Success ("the rest", One))
    let fallback = Parser (fun _ -> Success ("the rest", Two))

    let trying = firstParser <|> fallback


    test <@ run trying "some input" = Success ("the rest", One) @>

[<Fact>]
let ``if the first parser fails, applies the fallback parser`` () =
    let alwaysFails = Parser (fun _ -> Failure "failed!")
    let fallback = Parser (fun _ -> Success ("the rest", Two))

    let trying = alwaysFails <|> fallback

    test <@ run trying "some input" = Success ("the rest", Two) @>
```


## Choice

Naturally, you can apply `<|>` multiple times:


```fsharp
type WhateverResult = Whatever

[<Fact>]
let ``sequence of <|>`` () =
    let p1 = Parser (fun _ -> Failure "failed!")
    let p2 = Parser (fun _ -> Failure "failed!")
    let p3 = Parser (fun _ -> Failure "failed!")
    let p4 = Parser (fun _ -> Failure "failed!")
    let p5 = Parser (fun _ -> Failure "failed!")
    let fallback = Parser (fun _ -> Success ("the rest", Whatever))

    let trying = p1 <|> p2 <|> p3 <|> p4 <|> p5 <|> fallback

    test <@ run trying "some input" = Success ("the rest", Whatever) @>
```

This invites us to conceive a combinator that tries all the parsers we
provide it:

```fsharp
let choice (parsers: 'a Parser list) : 'a Parser =
    failwith "Not yet implemented"

let failing = Parser (fun _ -> Failure "failed!")

[<Fact>]
let ``applies the first successful parser`` () =
    let p1 = failing
    let p2 = failing
    let p3 = failing
    let p4 = failing
    let p5 = failing
    let succeeding = Parser (fun _ -> Success ("the rest", Whatever))
    let p6 = failing
    let p7 = failing

    let firstSucceeding = choice [p1; p2; p3; p4; succeeding; p5; p6; p7;]

    test <@ run firstSucceeding "some input" = Success ("the rest", Whatever) @>
```

A possible recursive implementation could be:

```fsharp
let rec choice<'a> (parsers: 'a Parser list) : 'a Parser =
    match parsers with
    | [] ->
        Parser (fun _ -> Failure "No parsers succeeded")
    | [p] ->
        p
    | p :: ps ->
        p <|> choice ps
```

Amazingly, a shorter working version is:

```fsharp
let choice parsers = 
    List.reduce (<|>) parsers
```

It would be nice to write it even more concisely, in Point Free Style
as:

```fsharp
let choice = List.reduce (<|>)
```

but F# type inference would scream at us.

Technically speaking, this super-short version is based on the fact
that a `Parser`, together with the binary operation `<|>` *forms a
semigroup*. In simple words, this means that we managed to have an
operation to reduce 2 different items into 1, and this is a very well
known pattern in functional programming. Indeed, `List.reduce` is
based on that pattern. It documentation states:

```
val reduce: reduction: ('T -> 'T -> 'T) -> list: 'T list -> 'T

'T is Parser<'a>

[...]
reduction - The function to reduce two list elements to a single element.
```

This is encouraging: whenever you happen to develop a custom operator
and then you discover that the standard F# library natively supports
it, that's the sign that you hit the nail on the head.

### Month names

Let's see how to apply `choice` in a concrete case. Say that you want
to parse a date from the format `12 Oct 2025`. For the month part, you
would like to parse:

| Input   | Parse result |
|---------|--------------|
| `"Jan"` | `1`          |
| `"Feb"` | `2`          |
| `"Mar"` | `3`          |
| `"Apr"` | `4`          |
| `"May"` | `5`          |
| `"Jun"` | `6`          |
| `"Jul"` | `7`          |
| `"Aug"` | `8`          |
| `"Sep"` | `9`          |
| `"Oct"` | `10`         |
| `"Nov"` | `11`         |
| `"Dec"` | `12`         |


Here how to create `12` parsers in one shot:

```fsharp
let months: int Parser list =
    [ "Jan"
      "Feb"
      "Mar"
      "Apr"
      "May"
      "Jun"
      "Jul"
      "Aug"
      "Sep"
      "Oct"
      "Nov"
      "Dec" ]

let monthParsers =
    months
    |> List.mapi (fun idx kw -> ((fun _ -> idx + 1) <!> (str kw)))
```

It's easy to coalesce them in a single parser with `choice`:

```fsharp
let monthParser = choice monthParsers

[<Fact>]
let ``parses a month`` () =
    test <@ run monthParser "Oct 2025" = Success (" 2025", 10) @>
    test <@ run monthParser "Apr 2009" = Success (" 2009", 4) @>
    test <@ run monthParser "not a month" = Failure "Expected Dec" @>
```

Notice that when the collection of parsers fails, it emits the error
of the last parser. This is a bit disappointing, and surely
suboptimal. There are techniques to improve that, but let's not get
sidetracked. We have other interesting combinators to invent, first.

Of course, if you want `monthParser` to return an `int` instead of a
`char`, you can map an `char -> int` to it, using `<!>`. Are you
getting familiar with this way of mixing those little combinators?

## anyOf
A very convenient helper function you can build on top of `choose` is
`anyOf`: it takes a list of characters and it builds a parser for any
of them. Under the hoods, it uses `charP`, a parser for a single
character:

```fsharp
let charP (c: char) = Parser (fun input ->
    if input.StartsWith(c)
    then Success (input[1..], c)
    else Failure "Expected '{c}'" )

let anyOf chars =
    chars |> List.map charP |> choice


let digit = anyOf ['0'..'9']

[<Fact>]
let ``parses any digit`` () =
    test <@ run digit "42 the rest" = Success ("2 the rest", '4') @>
    test <@ run digit "92 the rest" = Success ("2 the rest", '9') @>
```

`anyOf` is a classical low-level parser which can be used as a trivial
building block of other more complex parsers, by the means of `<!>`
and other combinators.

## many
Remember that we started investigating this very topic in the attempt
of parsing an unsigned integer of an unknown number of digits. We are
very close to this goal. Be ready to see a disappointing
implementation, though: in fact, we are scratching the bottom of what
is possible with Functors and Applicative Functors, and soon we will
need Monads.

So, back to our number with arbitrary digits problem. The idea is:

* To build a combinator `many` that keeps trying a specific parser
  over and over, until it fails. The parser generated by `many` would
  return the list of all the successfully parsed values.
* To parse many digits, we could apply `many` to the parser
  `anyDigit`. This in turn must be able to detect a single digit (any
  of `1`, `2`, `3`, etc).
* How to build `anyDigit`? We can apply `choice` on 10 separate simple
  parsers, each handling a single digit.
* Instead of writing 10 different parsers, we could build a factory of
  parsers, to be fed with `[0..9]`.
* The result will be a Parser emitting a the list of digits. We know
  how to convert a list of digits to a number. And we know how to use
  `map` to convert a Parsr of list of digits to a Parser of numbers. 

Let's go.

```fsharp

let many<'a> (parser: 'a Parser): 'a list Parser = 
    failwith "Not yet implemented"
    
let toInteger (digits: char list) : int =
    failwith "Not yet implemented"

let intP: int Parser =
    failwith "Not yet implemented"

[<Fact>]
let ``parse numbers of any number of digits`` () =
    test <@ run intP "1 the rest" = Success (" the rest", 1) @>
    test <@ run intP "42 the rest" = Success (" the rest", 42) @>
    test <@ run intP "2025+7999" = Success ("+7999", 2025) @>
```

Assuming the other components will eventually be there, implementing
`intP` it is a walk in the park:

```fsharp
let intP = many digit |>> toInteger
```

Read it as:

* `intP` is that parsers that expectes an arbitrary number of digits.
* Since the result of `many digit` is a `char list`, we need to
  convert it to an `int` with `toInteger`.
* But we don't have a `char list`: we have a `Parser` of `char list`.
  So, we need to lift `toInteger` into the parser world, using the
  parser-powered pipe operator `|>>`.

Implementing `toInteger` is ordinary F#, nothing to do with parsers:

```fsharp
let toInteger (digits: char list) : int =
    digits
    |> List.map string
    |> String.concat ""
    |> int

[<Fact>]
let ``from list of chars to integer`` () =
    test <@ ['4';'2'] |> toInteger = 42 @>
    test <@ ['1';'9';'9'] |> toInteger = 199 @>
    test <@ ['2';'0';'2';'5'] |> toInteger = 2025 @>
```

The last missing piece is, finally, `many`:

```fsharp
let many<'a> (parser: 'a Parser): 'a list Parser =
        failwith "Not yet implemented"


[<Fact>]
let ``applies a parser many times`` () =

    let manyWell = many (str "well!")

    test <@ run manyWell "well!well!well! the rest" = Success(" the rest", ["well!";"well!";"well!"]) @>
```

`many` will come in very handy: for example, it can be used to handle
the case of an unknown number of spaces language syntactic elements.
But, as you surely got by now, that the parser passed to `many` can be
arbitrarily complex; it can be as simple as the parser of a single
charactes as a whole JSON parser, or the parser for a huge piece of
code in your convoluted programming language syntax. `many` just won't
care.


Implementing `many` is actually very challenging. We have to build a
list of results, so recursion comes naturally to mind. The hard part
is that we are not really building a list, but a parser emitting a
list. What can help is to think that `many`, by itself, can never
fail: it keeps applying a parser, and when this fails, it just stops
cycling. Even if the input is empty, or if the parser immediately
fails, `many` would happily succeed, returning an empty list.

Here's a possible implementation:

```fsharp
let many<'a> (parser: 'a Parser): 'a list Parser = Parser (fun input ->
    let rec zeroOrMore input =
        match run parser input with
        | Failure _ -> (input, [])
        | Success (rest, result) ->
            match (zeroOrMore rest) with
            | rest, [] -> (rest, result :: [])
            | rest, others -> (rest, result :: others)

    Success(zeroOrMore input))
```

As you see, it makes use of a `zeroOrMore` inner function which does
not operate in the parser world. It just executes the parser with
`run`, recursing during the list building. As soon as `parser` fails,
it stops.

Now, this is what I call a disappointing implementation. We have gone
through 11 chapters, developing building blocks after building blocks,
only to be back to square one, building `many` by the means of pattern
matching and passing `rest` around. That's depressing.

Wait a minute! Can't we lift the function for recursively bulding a
list to the Parser world using `<!>` and `<*>`? I mean, if:

```fsharp
let cons head tail = head :: tail
```

can't we just lift it with:

```fsharp
let rec many parser = 
    cons <!> parser <*> (many parser)
```

To be precise: this version is not quite correct, as it requires at
least 1 application of parser (in the parser jargon: this is `many1`).
`many` should succeeds also in case 0 applications. Easy peacy, `<|>`
to the resque:

```fsharp
let rec many parser = 
    (cons <!> parser <*> (many parser)) <|> (pure' [])
```

Here we go! If the first part (the lifted `cons`) fails, we just a
lifted empty list.

It perfectly type checks. This is promising! If it compiles it works,
right! Until it does not. Damn. Try yourself: if you run the test, it
enters an infinite loop. It compiled only because $\bot$, or `bottom`,
the ideal type representing never-returning functions, is a member of
all the types. What a scam...

In simpler words, the problem here is that function application is
eager: F# evaluates all the arguments before passing them to a
function. If we had a lazy language, like Haskell, this implementation
could possibly work, but that's not the case with F#.

What about this implementation?

```fsharp
let rec many p =
    parser {
        let! r = p
        let! rest = many p
        return r :: rest
    } <|> (pure' [])
```

OK, this is a syntax we never encountered before. I don't expect you
to immediately understand it, if you never encountered do-notation.
But maybe you can grasp some of it:

* It's a parser, because of that initial `parser {`.
* It returns the list `r :: rest`.
* The head `r` is somehow related to running the parser (see that
  `let! r = p`).
* The tail `rest` is related to a recursive call to `many p`.
* The last `<|> pure' []` accounts for what we saw before: `many`
  shall not fail if the parser `p` cannot be applied even once.
  
I bet that you agree: besides the funny new syntactic elements, this
version is way more linear than the original:

```fsharp
let many<'a> (parser: 'a Parser): 'a list Parser = Parser (fun input ->
    let rec zeroOrMore input =
        match run parser input with
        | Failure _ -> (input, [])
        | Success (rest, result) ->
            match (zeroOrMore rest) with
            | rest, [] -> (rest, result :: [])
            | rest, others -> (rest, result :: others)

    Success(zeroOrMore input))
```

Ladies and gentlement, welcome monads and monadic computation
expressions. We've dragged this out long enough. It's time to open
that door.

Take a long break. Enjoy a Swiss cheese fondue. We will see in [Chapter 12](/monadic-parser-combinators-12).


# Comments
[GitHub
Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}




# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

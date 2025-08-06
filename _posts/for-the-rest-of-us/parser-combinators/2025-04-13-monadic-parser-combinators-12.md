---
layout: post
title: "Monadic Parser Combinators in F# - Things You Are Not Sure About"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
include_in_index: false
---
There are only 2 important missing features in the Parser Combinator
library you are building:

- Context Sensitivity.
- Backtracking.

*Context Sensitivity* is the ability of a parser to reference the
result of other parsers. Basically, it's equivalent to equipping
parsers with a form of memory. This capability will allow you to parse
more complex languages and will dramatically change the way you write
parsers. This requires introducing Monads. We will get there in the
very next chapter.


*Backtracking* is the ability to recover from errors and to explore
alternative parsing paths. This is necessary for the last use case we
encountered: parsing an integer of an unknown number of digits.

Let's start from the latter.

## Alternative

The basic operator for implementing backtracking is `<|>`: its purpose
is to try the parser on its left, first; if that parser fails, instead
of propagating the error, it *backtracks* trying the parser on the
right, using the original input. If both parsers fail, only then a
parsing error is returned. We implemented it already in [Chapter
3](/monadic-parser-combinators-3), but before introducing a `Parser`
type wrapper. The implementation is straightforward:

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
    let firstParser = Parser (fun _ -> Success (One,  "the rest"))
    let fallback = Parser (fun _ -> Success (Two, "the rest"))

    let trying = firstParser <|> fallback

    test <@ run trying "some input" = Success (One, "the rest") @>

[<Fact>]
let ``if the first parser fails, applies the fallback parser`` () =
    let alwaysFails = Parser (fun _ -> Failure "failed!")
    let fallback = Parser (fun _ -> Success (Two, "the rest"))

    let trying = alwaysFails <|> fallback

    test <@ run trying "some input" = Success (Two, "the rest") @>
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
    let fallback = Parser (fun _ -> Success (Whatever, "the rest"))

    let trying = p1 <|> p2 <|> p3 <|> p4 <|> p5 <|> fallback

    test <@ run trying "some input" = Success (Whatever, "the rest") @>
```

This invites us to conceive a combinator that tries all the parsers we
feed it with:

```fsharp
let choice (parsers: 'a Parser list) : 'a Parser = __


[<Fact>]
let ``applies the first successful parser`` () =

    let failing = Parser (fun _ -> Failure "failed!")
    
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
let choice =
    List.reduce (<|>)
```

but F# type inference would scream at us.

Technically speaking, this super-short version is based on the fact
that a `Parser`, together with the binary operation `<|>` *forms a
[semigroup][semigroup]*. In simple words, this means that we managed
to have an operation to reduce 2 different items into 1, and this is a
very well known pattern in functional programming. Indeed,
`List.reduce` is based on that pattern. [Its documentation][reduce]
states:

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

Let's see how to apply `choice` to a concrete case. Say that you want
to parse a date in the format `12 Oct 2025`. For the month part, you
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


Here is how we could create `12` parsers in one shot:

```fsharp
let months: string list =
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

let monthParsers: int Parser list =
    months
    |> List.mapi (fun idx month -> ((fun _ -> idx + 1) <!> (str month)))
```

Then, we can use `choice` to coalesce them in a single parser:

```fsharp
let monthParser = choice monthParsers

[<Fact>]
let ``parses a month`` () =
    test <@ run monthParser "Oct 2025" = Success (10, " 2025") @>
    test <@ run monthParser "Apr 2009" = Success (4, " 2009") @>
    test <@ run monthParser "not a month" = Failure "Expected Dec" @>
```

Note that when this parser fails, it emits the error produced by the
last parser in the collection. This is less than ideal. There are
techniques to improve that, but let's not get sidetracked. We have
other interesting combinators to invent, first.

## Simplify Until It Can't Be Simpler
Have you noticed how, the more combinators we add to our toolbelt, the
smaller is the need of developing manually-written parsers? Basically,
the only parser we wrote without combinators is `str`. We distilled
almost all the other ones by the application of `<!>`, `<*>`, `<|>`
and their friends. Even further: it's not hard to see how `str` itself
can be decomposed into smaller parsers. What about, for example,
combining:

- A collection of parsers, each for a specific char (like the parser
  for `a`, the parser for `5` and the like).
- `choice`, the combinator we just wrote, to generate a parser for any
  of the provided parsers. Or even better, `anyOf`, to generate a
  parser for any of the provided characters. So, a parser for a digit
  would be simple `anyOf ['0'..'9']`.
- `many`, to keep parsing, until the input string cannot be parsed
  anymore. This way, `many (anyOf ['0'..'9'])` would parse any
  sequence of digits (including the empty one).

You see how these few tools are enough for parsing strings, digits,
sequences of digits (i.e., numbers), etc. And if you take it to the
extreme, you also see how, amazingly, we can decompose the parser for
a specific character even more: the classic approach is to have the
parser `any` for *any* character, combined with `satisfy`, a
combinator that imposes some restrictions to parsers (in this case:
making sure the character parsed by `any` is the desired one).  
In a sense, we are scraping the bottom of the barrel, with parser
combinators: the hand-made parsers we stricly need are reducing to the
really trivial ones. In other words, we are seeing with our eyes how,
at its core, parser design thrives on simplicity: it's an art of
seeing patterns in the complexity, and of abstracting each pattern in
a specific combinator. Applyuing this aproach over and over will lead
us to end up with an interesting asymmetry: a very reach grammar of
combinators, a next-to-empty collection of actual parsers.

Following this path, let's build `anyOf` and `many`.

## anyOf
`anyOf` is just a helper function. It takes a list of characters and
it builds a parser for any of them. Under the hoods, it uses `charP`,
a parser for a single character:

```fsharp

let charP (c: char) = Parser (fun input ->
    if input.StartsWith(c)
    then Success (c, input[1..])
    else Failure "Expected '{c}'" )

let anyOf chars =
    chars |> List.map charP |> choice


let digit = anyOf ['0'..'9']

[<Fact>]
let ``parses any digit`` () =
    test <@ run digit "42 the rest" = Success ('4', "2 the rest") @>
    test <@ run digit "92 the rest" = Success ('9', "2 the rest") @>
```

## many
Remember that we started investigating this very topic in the attempt
of parsing an unsigned integer of an unknown number of digits. We are
very close to this goal. Be ready to see a disappointing
implementation, though: in fact, we are approaching the limit of what
is possible with Functors and Applicative Functors, and soon we will
need Monads.

The idea is:

* To build a combinator `many` that keeps trying a specific parser
  over and over, until it fails. The parser generated by `many` would
  return the list of all the successfully parsed values.
* To feed `many` with the `digit` parser we just built.
* The result will be a Parser for a list of digits. We know how to
  convert a list of digits to a number, so it's a matter of applying
  this logic by the means `map`.

Let's go.

```fsharp
let many<'a> (parser: 'a Parser): 'a list Parser = __ 
    
let toInteger (digits: char list) : int = __

let intP: int Parser = __

[<Fact>]
let ``parse numbers of any number of digits`` () =
    test <@ run intP "1 the rest" = Success (1, " the rest") @>
    test <@ run intP "42 the rest" = Success (42, " the rest") @>
    test <@ run intP "2025+7999" = Success (2025, "+7999") @>
```

Implementing `intP` it is a walk in the park:

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
let many<'a> (parser: 'a Parser): 'a list Parser = __


[<Fact>]
let ``applies a parser many times`` () =

    let manyWell = many (str "well!")

    test <@ run manyWell "well!well!well! the rest" =
    Success(["well!";"well!";"well!"], " the rest") @>
```


Implementing `many` is actually quite challenging. We have to build a
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
        | Failure _ -> ([], input)
        | Success (result, rest) ->
            match (zeroOrMore rest) with
            | [], rest -> (result :: [], rest)
            | others, rest -> (result :: others, rest)

    Success(zeroOrMore input))
```

Not so easy, right? As you see, it makes use of a `zeroOrMore` inner
function which does not operate in the parser world. It just executes
the parser with `run`, recursing during the list building. As soon as
`parser` fails, it stops.



Now, this is what I call a disappointing implementation. We have gone
through 11 chapters, developing building blocks after building blocks,
only to be back to square one, building `many` by the means of pattern
matching and passing `rest` around. That's depressing.

Wait a minute! Can't we lift the function for recursively bulding a
list to the Parser world using `<!>` and `<*>`? I mean, if building a
list can be done [cons-ing][cons] values with:

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
`many` should succeeds also in case 0 applications. Easy peasy, `<|>`
to the resque:

```fsharp
let rec many parser = 
    (cons <!> parser <*> (many parser)) <|> (pure' [])
```

Here we go! If the first part (the lifted `cons`) fails, we just a
lifted empty list.

It perfectly type checks. This is promising! "If it compiles it
works", they told you. Until it does not. Try yourself: if you run the
test, it enters an infinite loop. It compiled only because `⊥`, or
[bottom][bottom], the ideal type representing never-returning
functions, is a member of all the types. What a scam...

In simpler words, the problem here is that function application is
eager: F# evaluates all the arguments before passing them to a
function. If we had a lazy language, like Haskell, this implementation
could possibly work, but that's not the case with F#.


## Many, For The Rest Of Us
What about this implementation?

```fsharp
let rec many parser =
    parse {
        let! x = parser
        let! xs = many parser
        return x :: xs
    } <|> (pure' [])
```

This is a syntax we never encountered before. I don't expect you
to immediately understand it, if you never encountered do-notation.
But maybe you can grasp some of it:

* It's a parser, because of that initial `parse {`.
* It returns the list `x :: xs`.
* The head `x` is somehow related to running the parser (see that
  `let! x = parser`).
* The tail `xs` is related to a recursive call to `many parser`.
* The last `<|> pure' []` accounts for what we saw before: `many`
  shall not fail if the parser `parser` cannot be applied even once.
  
I bet that you agree: besides the funny new syntactic elements, this
version is way more linear than the original:

```fsharp
let many<'a> (parser: 'a Parser) : 'a list Parser =
    Parser(fun input ->
        let rec zeroOrMore input =
            match run parser input with
            | Failure _ -> ([], input)
            | Success(result, rest) ->
                match (zeroOrMore rest) with
                | [], rest -> (result :: [], rest)
                | others, rest -> (result :: others, rest)
         Success(zeroOrMore input))
```

Ladies and gentlemen, enter monads and monadic computation
expressions. We've delayed this out long enough. It's time to open
that door. The next chapter should provide the rational why and in
which cases we need monadic parsers. Then, we will invent them.

Take a long break. Enjoy a Swiss cheese fondue (and take your time to
digest it). We will see in [Chapter
13](/monadic-parser-combinators-13)


[Previous - Lifting Functions](/monadic-parser-combinators-11) ⁓
[Next - Things You Want To Remember](/monadic-parser-combinators-13)


# References

- [nCatLab - Semigroup][semigroup]
- [reduce - FSharp.Core/list.fsi#L1717][reduce]
- [Wikipedia - cons][cons]
- [Bottom][bottom]

[semigroup]: https://ncatlab.org/nlab/show/semigroup
[reduce]: https://fsharp.github.io/fsharp-core-docs/reference/fsharp-collections-listmodule.html#reduce
[cons]: https://en.wikipedia.org/wiki/Cons
[bottom]: https://wiki.haskell.org/Bottom

# Comments
[GitHub
Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

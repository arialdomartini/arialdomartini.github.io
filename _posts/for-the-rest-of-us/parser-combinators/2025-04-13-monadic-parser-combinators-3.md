---
layout: post
title: "Monadic Parser Combinators in F# - That's a Combinator!"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
include_in_index: false
---
So, let's challenge the composability of imperative parsers.
Suppose that other than `parsePerson`:

```fsharp
parsePerson: string -> Person
```

we also wish to write a Parser for `RockTrio` objects:

```fsharp
parseRockTrio: string -> RockTrio
```

where `RockTrio` is:

```fsharp
type RockTrio =
    { Name: string
      BassPlayer: Person
      GuitarPlayer: Person
      Drummer: Person}
```

Beside some specific syntax that might exist for the serializazion of
rock trios&mdash; and which we don't care at the moment &mdash; the
point is that we can think of writing `parseRockTrio` leveraging
`parseString` and `parsePerson`.

What if we also have `SoloArtist` to be parsed as:

```fsharp
type SoloArtist =
    { NickName: string
      Artist: Person }
```

No problem: we can again delegate most of the work to `parseString`
and `parsePerson`. This is what I call reuse!

## Your first Parser Combinator
Wait a sec: what if the input string happens to contain *either* a
`RockTrio` *or* a `SoloArtist`?

We will need to try both the parsers and to keep the value of the one
that happens to succeed. Oh! So there must exist a notion of *success*
and *failure*! This means that somehow a parser needs to signal when
it failed. Uhm...  Maybe we can let parsers raise exceptions in case
of failure. Let's define:

```fsharp
exception ParseException of string
```

OK, fine: provided that `RockTrio` and `SoloArtist` are both cases of
the same union type:

```fsharp
type RockBand =
    | RockTrio of RockTrio
    | SoloArtist of SoloArtist
``` 

our `parseBand` parser could be:

```fsharp

open System
open AutoFixture
open Swensen.Unquote
open global.Xunit
open ParserCombinators.Chapter02.ParsingAPerson

type RockTrio =
    { Name: string
      BassPlayer: Person
      GuitarPlayer: Person
      Drummer: Person }

type SoloArtist = { NickName: string; Artist: Person }

type RockBand =
    | RockTrio of RockTrio
    | SoloArtist of SoloArtist

exception ParseException of string

let parseBand parseRockTrio parseSoloArtist input : RockBand =
    try
        parseRockTrio input
    with :? ParseException ->
        parseSoloArtist input

let fixture = Fixture()
fixture.Customize<DateOnly>(
    _.FromFactory(fun (dt: DateTime) -> DateOnly.FromDateTime(dt)))

let rockTrio = fixture.Create<RockTrio>()
let soloArtist = fixture.Create<SoloArtist>()

[<Fact>]
let ``parses RockTrio`` () =
    let successfullyParseRockTrio input = RockTrio rockTrio
    let wontBeUsed input = SoloArtist soloArtist

    let parser = parseBand successfullyParseRockTrio wontBeUsed

    test <@ parser "some input" = RockTrio rockTrio @>

[<Fact>]
let ``parses SoloArtist if parsing RockTrio fails`` () =
    let justFail input = raise (ParseException "Failing to parse a Rock Trio")
    let successfullyParseSoloArtist input = SoloArtist soloArtist

    let parser = parseBand justFail successfullyParseSoloArtist

    test <@ parser "some input" = SoloArtist soloArtist @>
```

I'm using AutoFixture (with a little trick for handling `DateOnly`)
because I am too lazy for defining every test instances.

The implementation:

```fsharp
let parseBand parseRockTrio parseSoloArtist input : RockBand =
    try
        parseRockTrio input
    with :? ParseException ->
        parseSoloArtist input
```

is straightforward. Also, if you entirely abandon yourself to the F#
type inference, you realize that it is super generic too: indeed, it
works with any couple of parsers. We could generalize it as:

```fsharp
let (<|>) first second =
    fun input ->
        try
            first input
        with :? ParseException ->
            second input

type Cases =
    | First
    | Second

[<Fact>]
let ``uses first parser if successful`` () =
    let successfullyParseFirst input = First
    let wontBeUsed input = Second

    let parser = successfullyParseFirst <|> wontBeUsed

    test <@ parser "whatever input" = First @>

[<Fact>]
let ``falls back to second parser if first parser fails`` () =
    let justFail input = raise (ParseException "I was meant to fail")
    let successfullyParseSecond input = Second

    let parser = justFail <|> successfullyParseSecond

    test <@ parser "whatever input" = Second @>
```

Let's read the signature again:

```fsharp
val (<|>) : (string -> 'a) -> (string -> 'a) -> (string -> 'a)
```


This is a function that, given 2 generic Parsers `(string -> 'a)`,
returns a new Parser `(string -> 'a)`. Think about it: so far, we have
always created parsers by writing their code, directly. At most we
have reused some pre-existing parsers. But here something new is
happening: this is a higher-order function that *combines* parsers,
*generating* a brand new one, seemingly out of thin air.  
Here's how it is used:

```fsharp
let parseRockTrioOrSoloArtist = parseRockTrio <|> parseSoloArtist
```

Look ma, we got a new parser without writing its code!  
Kudos! You just have invented a Parser Combinator! It's not a monadic
one yet but, I mean, wow! Congrats!

By the way: remember the levels 4 and 5? This is the case of "Given 2
instances of `X` they can be combined together to form another `X`,
100% preserving all the expected properties." It's up to you to judge
if the code for generating `parseRockTrioOrSoloArtist` is easy and
elegant enough to deserve the Level 5 reward.


## Who Can Stop Us Now?
So, we have built our first Parser Combinator `<|>` which generates a
new Parser from 2 possibly failing ones. This could be the first
building block of a grammar of Parser Combinators, with which to build
the parser of any arbitrarily complex language. Using a bit of
fantasy, you could conceive other Parser Combinators such as:

| Name        | Signature                                                                     | Generates a parser that...                                                      |
|-------------|-------------------------------------------------------------------------------|---------------------------------------------------------------------------------|
| `many`      | `(string -> 'a) -> (string -> 'a list)`                                       | parses zero or more occurrences of something, collecting the results in a list. |
| `many1`     | `(string -> 'a) -> (string -> 'a list)`                                       | same as above, but expects at least 1 occurrence.                               |
| `skipMany`  | `(string -> 'a) -> (string -> ())`                                            | parses zero or more occurrences of something, discarding results.               |
| `skipMany1` | `(string -> 'a) -> (string -> ())`                                            | same as above, but expects at least 1 occurrence.                               |
| `between`   | `(string -> 'open) -> (string -> 'close) -> (string -> 'a) -> (string -> 'a)` | parses something between opening and closing elements.                          |
| ...         |                                                                               |                                                                                 |

It turns out that if you manage to design a set of very expressful and
fine tuned building blocks, you don't need to write the code of many
parsers: indeed, you will be able to generate any imaginable parser
only combinining the most trivial parsers that could be conceived,
that are:

| Name  | Signature          | Generates a parser that...                  |
|-------|--------------------|---------------------------------------------|
| `eof` | `(string -> ())`   | succeeds only at the end of file.           |
| `any` | `(string -> char)` | succeeds no matter what the input contains. |

Don't despair. We will get to this.

But first, I wish you to realize that we cannot proceed before solving
a structural problem: our parsing logic is too much coupled with the
effectful logic.

I imagine you may not even see this problem &mdash; what the heck is
the *effectful logic*, to begin with? How can this be a show stopper?
And it's fine: the parsing logic is still very simple and usually
problems tend to bite only when the complexity reaches higher
levels. It is also true, though, that when problems start biting, it
is often too late to fix them. So, better investigate.

The good news is, this problem isn't per se a barrier but an
invitation: in the next chapter we will intentionally increase the
complexity of our parsers, so to see the problem arise. Then we will
code-bend it into an improvement, finally getting to Applicative
Functors and Monads. Bear with me.

Have a slice of Black Forest Cake, you deserve it and you need energy
for [the next chapter](monadic-parser-combinators-4).

[Previous - 5 Shades Of Composability](/monadic-parser-combinators-2)
⁓ [Next - I Told You Not Mess With The
Signature!!](/monadic-parser-combinators-4)


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

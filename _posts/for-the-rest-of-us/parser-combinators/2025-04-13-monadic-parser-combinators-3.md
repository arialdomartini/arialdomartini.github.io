---
layout: post
title: "Monadic Parser Combinators in F# - Combinators!"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
- property-based testing
include_in_index: false
---
So, let's challenge the composability of our imperative parsers.
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
point is that we can think of writing `parseRockTrio` reusing
`parseString` and `parsePerson`. This is the basis of composition.

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

We will need to try both parsers and to keep the value of the one
succeeding. Oh! So there must exist a notion of *success* and
*failure*! This means that somehow a parser needs to signal when it
failed. Uhm...  Maybe we can let parsers raise exceptions in case of
failure.  
OK, fine: provided that `RockTrio` and `SoloArtist` are both cases of
the same union type:

```fsharp
type RockBand =
    | RockTrio of RockTrio
    | SoloArtist of SoloArtist
``` 

our parser could be:

```fsharp

let parseBand parseRockTrio parseSoloArtist input: RockBand =
    try
        parseRockTrio input
    with ParseException ->
        parseSoloArtist input

[<Property>]
let ``parses RockTrio`` (rockTrio: RockTrio) (artist: SoloArtist) (input: string) =
    let successfullyParseRockTrio input = RockTrio rockTrio
    let wontBeUsed input = SoloArtist artist

    let parser = parseBand successfullyParseRockTrio wontBeUsed

    test <@ parser input = RockTrio rockTrio @>

[<Property>]
let ``parses SoloArtist if parsing RockTrio fails`` (artist: SoloArtist) (input: string) =
    let justFail input = raise ParseException
    let successfullyParseSoloArtist input = SoloArtist artist

    let parser = parseBand justFail successfullyParseSoloArtist

    test <@ parser input = SoloArtist artist @>
```

The implementation is super easy. Also, if you abandon yourself
entirely to the F# type inference, you realize that it is super
generic too: indeed, it works with any couple of parsers. We could
generalize it as:

```fsharp
let (<|>) first second =
    fun input ->
        try
            first input
        with ParseException ->
            second input

// Tests

type Cases = First | Second

[<Fact>]
let ``uses first parser if successful`` () =
    let successfullyParseFirst input = First
    let wontBeUsed input = Second

    let parser = successfullyParseFirst <|> wontBeUsed

    test <@ parser "whatever input" = First @>

[<Fact>]
let ``falls back to second parser if first parser fails`` () =
    let justFail input = raise ParseException
    let successfullyParseSecond input = Second

    let parser = justFail <|> successfullyParseSecond

    test <@ parser "whatever input" = Second @>
```

Read the signature again:

```fsharp
val (<|>) : (string -> 'a) -> (string -> 'a) -> (string -> 'a)
```


This is a function that, given 2 generic Parsers `(string -> 'a)`,
returns a new Parser `(string -> 'a)`. Think about it: so far we have
thought of creating parsers *writing their code*, at the most reusing
some pre-existing parsers. But here something new happened: this is a
high-order function that *combines* parsers, *generating* out of the
thin air another brand new Parser. See this in use:

```fsharp
let parseRockTrioOrSoloArtist = parseRockTrio <|> parseSoloArtist
```

Look ma, we got a new parser without writing its code!  
Kudos! You just have invented a Parser Combinator! It's not a monadic
one yet but, I mean, wow! congrats!

By the way: remember the levels 4 and 5? This is the case of "Given 2
instances of `X` they can be combined together to form another `X`,
100% preserving all the expected properties." It's up to you to judge
is the code for generating `parseRockTrioOrSoloArtist` is either hard
or easy to grasp.


## Detecting early problems
So, you built your first Parser Combinator `<|>` which generates a new
Parser from 2 possibly failing ones. This could be the first building
block of a grammar of Parser Combinators, with which to build the
parser of any arbitrarily complex language. Using a bit of fantasy,
you could conceive other Parser Combinators such as:

| Name        | Signature                                                                     | Generates a parser that...                                                      |
|-------------|-------------------------------------------------------------------------------|---------------------------------------------------------------------------------|
| `many`      | `(string -> 'a) -> (string -> 'a list)`                                       | parses zero or more occurrences of something, collecting the results in a list. |
| `many1`     | `(string -> 'a) -> (string -> 'a list)`                                       | same as above, but expects at least 1 occurrence.                                |
| `skipMany`  | `(string -> 'a) -> (string -> ())`                                            | parses zero or more occurrences of something, discarding results.               |
| `skipMany1` | `(string -> 'a) -> (string -> ())`                                            | same as above, but expects at least 1 occurrence.                                |
| `between`   | `(string -> 'open) -> (string -> 'close) -> (string -> 'a) -> (string -> 'a)` | parses something between opening and closing elements.                          |

etc.


It turns out that if you manage to design a set of very expressful and
fine tuned building blocks, you don't need to write the code of a
single parser: you will be able to generate any imaginable parser only
combinining the most trivial parsers that could be conceived, that
are:

| Name        | Signature                                                                     | Generates a parser that...                                                      |
|-------------|-------------------------------------------------------------------------------|---------------------------------------------------------------------------------|
| `eof`       | `(string -> ())`                                                              | succeeds only at the end of file.                                               |
| `any`       | `(string -> char)`                                                            | succeeds no matter what the input contains.                                               |
rty

Don't despair. We will get to this.

But first, I wish you to realize that we cannot proceed before solving
a structural problem:

- Our parsing logic is too much coupled with the effectful logic.

I imagine you may not even see this problem &mdash; what the heck is
the *effectful logic*, to begin with?. Maybe if you do, you don't
perceive this as a show stopper just yet. And it's fine: the parsing
logic is still very simple and usually problems tend to bite only when
the complexity reaches higher levels. It is also true, though, that
when problems start biting, it is often too late to fix them. So,
better investigate.

The good news is, this problem isn't per se a barrier but an
invitation: in the next chapter we will intentionally increase the
complexity of our parser, so to see the problem arise. Then we will
code-bend it into an improvement, finally getting to applicative
functors and monads. Bear with me.

Have a slice of Black Forest Cake, you deserve it and you need energy
for [the next chapter](monadic-parser-combinators-4).

# References


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

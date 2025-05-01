---
layout: post
title: "Monadic Parser Combinators in F# - Composition"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
- property-based testing
ftrou: true
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

Beside some specific syntax that might exist for it &mdash; and which
we don't care at the moment &mdash; the point is that we can think of
writing `parseRockTrio` reusing `parseString` and `parsePerson`. This
is the basis of composition.

What if we also have `SoloArtist` to be parsed as:

```fsharp
type SoloArtist =
    { NickName: string
      Artist: Person }
```

No problem: we can again delegate most of the work to `parseString`
and `parsePerson`. This is what I call reuse!

## Detecting early problems
Wait a sec: what if the input string can contain either a `RockTrio` or
a `SoloArtist`?

We will need to try both parsers and to keep the value of the one
succeeding. Oh, so there must be a notion of succeeding and failing.
This means that somehow a parser needs to signal if it failed. Uhm...
Maybe we can let parsers raise exceptions in case of failure. OK,
fine: provided that `RockTrio` and
`SoloArtist` are both cases of the same union type:

```fsharp
type RockBand =
    | RockTrio of RockTrio
    | SoloArtist of SoloArtist
``` 

our parser could be:

```fsharp

let parseBand parseRockTrio parseSoloArtist put : RockBand =
    try
        parseRockTrio input
    with ParseException ->
        parseSoloArtist input

[<Property>]
let ``it parses a SoloArtist if parsing of RockTrio fails`` (artist: SoloArtist) (input: string) =
    let justFail input = raise ParseException
    let successfullyParseSoloArtist input = SoloArtist artist

    let parsed = parseBand justFail successfullyParseSoloArtist input

    test <@ parsed = SoloArtist artist @>
```

This doesn't seem such a big deal, does it? In fact, there are 2
problems here.

- This approach falls short, it does not scale.
- Parsing logic is too much coupled with other concerns.

I imagine you may not perceive those 2 problems as show stoppers just
yet. And it's fine, they are not: the parsing logic is still very
simple and usually problems tend to bite only when the complexity
reaches higher levels. It is also true, though, that when problems
start biting, it is often too late to fix them. So, better
investigate.

The good news is, these aren't per se barriers but invitations: we'll
code-bend them into improvements, finally getting to applicative
functors and monads. Bear with me.

## A matter of choices
So, the 2 problems. The first: the `try/with` approach clearly does
not scale. If besided `RockTrio` and `SoloArtist` the input string
also contained other cases, the cascade would indefinitely grow:

```fsharp
let parseBand parseCase1 parseCase2 parseCase3 parseCase4 ... input =
    try
        parseCase1 input
    with
    | ParseException ->
        try
            parseCase2 input
        with
        | ParseException ->
            try
                parseCase3 input
            with
            | ParseException ->
                try
                    parseCase4 input
                with
                | ParseException ->
                     ....

```

You see where this goes: your esoteric programming language and your
funny serialization format will have tens if not hundreds of cases and
you don't want to write a specific code for each and every combination
of parsers, do you? You will probably want to factor this logic to a
generic function `choise` that, given a collection of parsers, returns
the first successful one:


```fsharp
exception ParseException of string

let rec choice<'a> (parsers: (string -> 'a) list) (input: string) =
    match parsers with
    | [] -> raise (ParseException "All parsers failed")
    | parser::others ->
        try
            parser input
        with
        | ParseException _ -> choice others input


// Tests

let failingParser<'a> (i: int) (input: string) : 'a =
    raise (ParseException $"parser {i} failed")

let failingParsers<'a> : (string -> 'a) list =
    [1..10]
    |> Seq.map failingParser
    |> Seq.toList

[<Fact>]
let ``it fails if all the parsers fail`` () =

    let parser = choice failingParsers

    raisesWith<ParseException>
        <@ parser "whatever input" @>
        (fun e -> <@ e.Data0 = "All parsers failed" @>)


[<Fact>]
let ``uses the first successful parser`` () =

    let first input : string = "first succeeded!"
    let second input : string = "first succeeded!"

    let parser: string -> string =
                choice
                     [failingParser 1
                      failingParser 2
                      first
                      failingParser 3
                      second ]

    test <@ parser "whatever input" = "first succeeded!"@>
```

Notice the signature:

```fsharp
val choice: ((string -> 'a) list) -> string -> 'a
```

This is a function that, given a list of Parsers (`(string -> 'a)
list`), returns a new Parser `(string -> 'a)`. Think about it: so far
we have thought of creating parsers *writing their code*, at the most
reusing some pre-existing parsers. But here something new happened:
this is a high-order function that *combines* parsers *generating*,
out of the thin air, another brand new Parser. See this in use:


```fsharp
let parseRockTrioOrSoloArtist = [parseRockTrio; parseSoloArtist] |> parseFirst
```

Look ma, we got a new parser without writing its code! In fact, there
is more test code than implementation...  
Kudos! You just have invented a Parser Combinator! It's not a monadic
one yet but, I mean, wow! congrats!

By the way: remember the levels 4 and 5? This is the case of "Given 2
instances of `X` they can be combined together to form another `X`,
100% preserving all the expected properties." It's up to you to judge
is the code for generating `parseRockTrioOrSoloArtist` is either hard
or easy to grasp.


## Yep, but it does not scale...
Before talking about the 2nd problem, let's investigate a bit more on
what a parser is supposed to return.

When I wrote:

```fsharp
let parsePerson: string -> Person = fun s ->
    let recordPart: string = ...
    let guidPart: string = ...
    let namePart: string = ...
    let birthdayPart: string = ...
    

    { Id = parseGuid guidPart
      Name = parseString namePart
      Birthday = parseBirthday birthdayPart }
```

I was very reticent. It's OK that `guidPart` is able to parse a GUID
from a string such as:

```
*b19b8e87-3d39-4994-8568-0157a978b89a*
```

but how it manages to understand where that section is, in the whole
string:

```
inst Person
   - Id <- *b19b8e87-3d39-4994-8568-0157a978b89a*
   - Name <- <<Richard>>
   - Birthday <- date{16/03/1953}
```

is still a mystery.

Even worse: I wrote that `recordPart` is that part of the composition
responsible for handling the parsing logic specific to the syntax of a
record. I said like it's nothing! If only it were so easy! Indeed: the
syntax of a record is not just a whole, sequential string; it is
interleaved with the field values. Most likely, `recordPart` needs to
identify the initial string:

```
inst Person
   - Id <-
```

then it must delegate to `parseGuid`; then it needs to check that the
input continues with:

```

   - Name <-
```

etc. I just glissed on this complexity.  
Not to mention that the input string might contain extra spaces such
as in:

```
inst     Person
   - Id       <-   *b19b8e87-3d39-4994-8568-0157a978b89a*
   - Name     <-   <<Richard>>
   - Birthday <-   date{16/03/1953}
```

OK, things start getting very complicated.

We need to break this problem down into smaller ones. An idea that
would immensely help is: each parser could return 3 pieces
information:

1. The parsed value (this is the main goal of a parser).
2. If it either succeeded or failed (we covered this with Exceptions)
3. How much of the input string it consumed and where it stopped.

The last new information is the key. The next parser can keep parsing
where the previous one stopped, so the input string will be consumed,
sequentially, from the first to the last character.

So, rather than:

```fsharp
val parser<'a> = string -> 'a
```

a parser would rather have the signature:

```fsharp
val parser<'a> = string -> (string, 'a)
```

The returned tuple contains the unconsumed input *plus* the parsed
value. When a parser is invoked, the returned unconsumed input may be
passed to the next parser, so that this can keep consuming the input.

`parsePerson` might turn into something like:


```fsharp
let parsePerson: string -> (string * Person) =
    fun input ->

        let (remaining, _) = parseRecord input
        let (remaining, id) = parseGuid remaining
        let (remaining, _) = parseUpToName remaining
        let (remaining, name) = parseString remaining
        let (remaining, _) = parseUpToBirthday remaining
        let (remaining, birthday) = parseBirthday remaining
        let (remaining, _) = parseTillTheEnd remaining

        remaining,
        { Id = id
          Name = name
          Birthday = birthday }
```

No, wait: we also have to consider error handling:

```fsharp
let parsePerson: string -> (string * Person) =
    fun input ->
        try

            let (remaining, _) = parseRecord input
            let (remaining, id) = parseGuid remaining
            let (remaining, _) = parseUpToName remaining
            let (remaining, name) = parseString remaining
            let (remaining, _) = parseUpToBirthday remaining
            let (remaining, birthday) = parseBirthday remaining
            let (remaining, _) = parseTillTheEnd remaining

            remaining,
            { Id = id
              Name = name
              Birthday = birthday }
        with
        | ParseException e ->
            raise (ParseException $"Failed to parse Person because of {e}")
```

You can imagine that in the first invocation, `parseRecord` consumes
the string:

```
inst Person
   - Id <- 
```

It can ignore the output, since it just needs either to fail or to get
to the point where `parseGuid` can proceed.  
Similarly `parseUpToName` would consume:

```
   - Name <- 
```

and so on.  
OK, that's not too complicated. But I bet you agree: it's a bit
repetitive and not very elegant. Passing that `remaining` value around
is super boring.

I'm personally too lazy to even copy paste that monotonous code. As it
often happens, developer's laziness is the catalyst of abstraction:
this code immediately ignites our wish to factor the duplication
away into a separate, generic function:


```fsharp
exception ParseException of string

let sequence<'a> (parsers: (string -> string * 'a) list) (input: string) : string * 'a list =
    let rec parseRec remainingInput parsers acc =
        match parsers with
        | [] -> (remainingInput, List.rev acc)
        | currentParser::remainingParsers ->
            let (newRemaining, parsedValue) = currentParser remainingInput
            parseRec newRemaining remainingParsers (parsedValue::acc)

    parseRec input parsers []


type Something = Something of int

let mockParser (i: int) (input: string) = (input[1..], Something i)

[<Fact>]
let ``applies all the parsers consuming 1 character for parser`` () =

    let fiveParsers =
        [ 1..5 ]
        |> Seq.map mockParser
        |> Seq.toList

    let parser = fiveParsers |> sequence

    let parsedValues = [
        Something 1
        Something 2
        Something 3
        Something 4
        Something 5 ]

    test <@ parser "12345abc" = ("abc", parsedValues) @>
```

Woah! Isn't this another Parser Combinator? It's not directly usable
in `parsePerson`, though, because it requires that all the parsed
elements are members of the same type `'a`, but it's still a
combinator.

If we really wanted to use this combinator in `parsePerson`, we could
wrap `Guid`, `string` and `DateOnly` in a single union type:

```fsharp
type MyTypes =
   | GuidCase of Guid
   | StringCase of string
   | DateOnlyCase of DateOnly
```

While this it surely overkill for a serialization language, it is
indeed the typical approach for programming language parsers.

So, we have to live with this series of:

```fsharp
let (remaining, value1) = parse1 input
let (remaining, value2) = parse2 remaining
let (remaining, value3) = parse3 remaining
let (remaining, value4) = parse4 remaining
....
let (remaining, valueN) = parseN remaining
```

for a bit more. Speaking about elegance, I don't know about you, but
these verbose signatures:

```fsharp
val sequence<'a> (string -> string * 'a) list -> string -> string * 'a list
```

are really starting to get on my nerves. We should do something to
make them simpler. Type aliases for the win! Just defining:

```fsharp
type Parser<'a> = string -> string * 'a
```

`choice` and `sequence` simplify to:

```fsharp
val choice<'a>: 'a Parser list -> 'a Parser

val sequence<'a>: 'a Parser list -> 'a list Parser
```

Ah! Much, much better!  
We still have to talk about the second problem, though. This will
definetely kill our mood.

## It's coupled
The second problem arises because we are impatient and we never
settle. We read what we coded so far and we torment ourselves thinking
"Exception sucks. We are functional programmers, damn! We were
supposed to use an `Either` or a `Result` instead".

OK, fine: let's use a `Result`, then.

There are 2 possibilities. Either we return the unconsumed input only
in case of a successful parsing:

```fsharp
type ParseError = string
type Input = string
type Rest = string

type Parser<'a> = Input -> Rest * Result<'a, ParseError>
```

or we return it in any case:

```fsharp
type Parser<'a> = Input -> Result<Rest * 'a, ParseError>
```

You might recognize this last signature as the one of a State Monad
(go read [State Monad for The Rest of
Us](state-monad-for-the-rest-of-us) if you are curious).

Both approaches are viable and both will throw a wreck on the code we
have written so far, making it apparent that we coupled the error
handling concern with the parsing logic.  
Let's use the first signature.

## From Exceptions to functional error handling
Adapting our 2 combinators and their tests is a piece of cake. For
example, `choice` becomes:

```fsharp
let rec choice<'a> (parsers: 'a Parser list) : 'a Parser =
    fun input ->
        match parsers with
        | [] -> Error "All parsers failed"
        | parser::others ->
            match parser input with
            | Ok success -> Ok success
            | Error _ -> choice others input

let failingParser i : 'a Parser =
    fun _ ->
        Result.Error $"parser {i} failed"

let failingParsers<'a>: 'a Parser list =
    [1..10]
    |> Seq.map failingParser
    |> Seq.toList


[<Fact>]
let ``it fails if all the parsers fail`` () =

    let parser = choice failingParsers

    test <@ parser "whatever input" = Error "All parsers failed" @>

[<Fact>]
let ``uses the first successful parser`` () =

    let first _ = Ok ("", "first succeeded!")
    let second _  = Ok ("", "first succeeded!")

    let parser: string Parser =
                choice
                     [failingParser 1
                      failingParser 2
                      first
                      failingParser 3
                      second ]

    test <@ parser "whatever input" = Ok ("", "first succeeded!") @>
```

Voilà, no more exceptions!  
Unfortunately, the same cannot be said for `parsePerson`:


```fsharp
let parsePerson: Person Parser = fun input ->

    match parseRecord input with
    | Ok (remaining, _) -> 
        match parseGuid remaining with
        | Ok (remaining, id) ->
            match parseUpToName remaining with
            | Ok (remaining, _) ->
                match parseString remaining with
                | Ok (remaining, name) ->
                    match parseUpToBirthday remaining with
                    | Ok (remaining, _) ->
                        match parseBirthday remaining with
                        | Ok (remaining, birthday) ->
                            match parseTillTheEnd remaining with
                            | Ok (remaining, _) ->        
                                Ok (remaining,
                                    { Id = id
                                      Name = name
                                      Birthday = birthday })
                            | Error err -> Error err
                        | Error err -> Error err
                    | Error err -> Error err
                | Error err -> Error err
            | Error err -> Error err
        | Error err -> Error err
    | Error err -> Error err
```

Holy crap! This is absolutely horrific. There is more error control
code than domain logic!

The good news: the attempts to factor this mess out will lead us to
invent Applicative Functors and Monads.

Before proceeding with code, I think it's useful to reflect how we
should proceed.

## A tale of 2 coupling types 

The implementation of `parsePerson` delegates the parsing of GUIDs,
strings and dates to external functions, remember? We think that this
makes `parsePerson` decoupled from the parsing logic of the specif
fields. And this is so true.

Yet, the code we just obtains clearly shows that some problems still
exist. A way to interpret this is to realize that there are in fact 2
levels of coupling:

- A function can be coupled with *the logic* of other components.  
That cannot be our case: `parsePerson` does not even know how GUIDs
are represented, this logic is completely delegated to `parseGuid`.

- A function could be coupled with the mechanic of glueing things
  together.  
  This means that even if it is functionally isolated, the code
  structure suffers from this "glueing logic" dependency. This could
  be our case.
  

Now, if you are not into functional programming, it is likely that you
never heard of the second form of coupling. After all, in OOP "glueing
things together" is rarely a big deal. There are a few of techniques
for gluing things, such as sending messages to objects in a sequence,
or passing values around or applying values to functions. But if you
think about it, they are all natively implemented by the programming
language and, in the end, they all boil down to the notion of function
application.

The native function application works just fine as long as we operate
within the simple case of things with compatible signatures:

```fsharp
f : 'a -> 'b
g : 'b -> 'c
```

Languages natively know how to glue `f` with `g` because the output of
`f` can be passed, just as it is, to `g`.

In other words, in OOP we rarely have to worry about glueing things
because we intentionally design our methods so their signature makes
the programming language happy. When things have imcompatible
signatures, we write wrappers and adapters to work arund the imcompatibility.  
The farest we go with making things intentionally incompatible, in
OOP, is when we break the signature-compatibility rule with Async
functions:


```fsharp
f : 'a -> Task<'b>
g : 'b -> Task<'c>
```

Those functions just don't combine natively. We dare to go this
direction only because is an easy problem to solve: the language
reserves a special treatment to this case, providing us with the
dedicated keywords `async` and `await`.

But that's an ad-hoc solution. We cannot expect that F# provided a
special treatment for parser functions returning `Result`s of tuples.  

In FP it's often the case that we intentionally design the function
signatures ignoring the native gluing mechanism. We take the freedom
to design functions that don't fit together because this gives us the
chance to put some logic in the gluing mechanism. See it like this: the native
language function application is dumb, it just passes values from a
function's output to the next function's input. Maybe we want function
application to do something fancier: we want it to log each call; or
to deal with errors via a `Result` instance, as in our case. Or to do
some combinatorial calculation.

I mean, we dare to do this because functional programming provides a
solution that is way more generic than special language keywords like
`async` and `await`. It is like we have the possibility to develop new
keywords other then `async`/`await`, to obtain custom function
applications on steroids. Those custom function applications mechanism
could let function with peculiar type signatures glue together, doing
something else in the while.

If you read [Monads for The Rest of Us](/monads-for-the-rest-of-us)
the notion of Applicative Functors and Momands as an extension of
function application should not be new to you.

In a sense, if in OOP the signature incompatibility of things is a
problem to be avoided or to be solved by the means of wrappers and
adapters, in FP the same incompatibility is a design tool to be
leveraged.

So, let's see how to fix the pyramid of doom we wrote in
`parsePerson'` and how this leads us to re-invent &mdash; yet another
time &mdash; monads.

Take a break, bite an apple, then jump to [the next installment](/monadic-parser-combinators-4).

# References


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

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
we don't care at the moment &mdash; the point is that we can alway
reuse `parseString` and `parsePerson`. Good.

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
a `SoloArtist`, provided that they are both cases of the same union
type

```fsharp
type RockBand =
    | RockTrio
    | SoloArtist
```

We will need to try both parsers and to keep the value of the one
succeeding. Oh, so there must be a notion of succeeding and failing.
This means that somehow a parser needs to signal if it failed. Uhm...
Maybe we can let parsers raise exceptions in case of failure. OK,
fine: this means that our client code need to be:

```fsharp
let parseBand input =
    try
        parseRockTrio input
    with
    | ParseException ->
        parseSoloArtist input
```

This doesn't seem such a big deal, does it? There are in fact 2 problems
here.

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
let parseBand input =
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
generic function that, given some parsers, returns the first
successfull one:

```fsharp
let choice parser1 parser2 parser3 parser4 parser5 parser6 input =
    try
        parser1 input
    with ParseException ->
        try
            parser2 input
        with ParseException ->
            try
                parser3 input
            with ParseException ->
                try
                    parser4 input
                with ParseException ->
                    try
                        parser5 input
                    with ParseException ->
                        parser6 input
```

Or even better, generalizing:

```fsharp
let rec choice (parsers: (string -> 'a) list) (input: string) =
    match parsers with
    | [] -> raise (ParseException "All parsers failed")
    | parser::others ->
        try
            parser input
        with
        | ParseException _ -> parseFirst others input
```

Notice the signature:

```fsharp
var choice: ((string -> 'a) list) -> string -> 'a
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

then delegate to `parseGuid`; then it needs to check that the input
continues with:

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
var parser<'a> = string -> 'a
```

a parser would rather have the signature:

```fsharp
var parser<'a> = string -> (string, 'a)
```

The returned tuple contains the unconsumed input *plus* the parsed
value. When a parser is invoked, the returned unconsumed input may be
passed to the next parser, so that this can keep consuming the input.

`parsePerson` might turn into something like:


```fsharp
let parsePerson: string -> Person = fun input ->

    let (remaining, _) = parseRecord input
    let (remaining, id) = parseGuid remaining
    let (remaining, _) = parseUpToName remaining
    let (remaining, name) = parseString remaining
    let (remaining, _) = parseUpToBirthday remaining
    let (remaining, birthday) = parseBirthday remaining
    let (remaining, _) = parseTillTheEnd remaining
    
    { Id = id
      Name = name
      Birthday = birthday } , remaining
```

No, wait: we also have to consider error handling:

```fsharp
let parsePerson': string -> Person = fun input ->
    try
        let (remaining, _) = parseRecord input
        let (remaining, id) = parseGuid remaining
        let (remaining, _) = parseUpToName remaining
        let (remaining, name) = parseString remaining
        let (remaining, _) = parseUpToBirthday remaining
        let (remaining, birthday) = parseBirthday remaining
        let (remaining, _) = parseTillTheEnd remaining
        
        { Id = id
          Name = name
          Birthday = birthday } , remaining
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

ignoring the output, only as a mechanism to get to the point where
`parseGuid` can proceed.  
Similarly `parseUpToName` would consume:

```
   - Name <- 
```

and so on.  
OK, that's not too complicated. But I bet you agree: it's a bit
repetitive and not very elegant. Passing that `remaining` value around
is super boring. I'm personally too lazy to even copy paste that
monotonous code.

As it often happens, developers laziness is the catalyst of
abstraction: this code immediately ignites our wish to factoring the
duplication away into a separate, generic function:


```fsharp
let parseSequence (parsers: (string -> string * 'a) list) (input: string) =
    let rec parseRec remainingInput parsers acc =
        match parsers with
        | [] -> (remainingInput, List.rev acc)
        | currentParser::remainingParsers ->
            let (newRemaining, parsedValue) = currentParser remainingInput
            parseRec newRemaining remainingParsers (parsedValue::acc)

    parseRec input parsers []
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


Now, we could even live with this series of:

```fsharp
let (remaining, value1) = parse1 input
let (remaining, value2) = parse2 remaining
let (remaining, value3) = parse3 remaining
let (remaining, value4) = parse4 remaining
....
let (remaining, valueN) = parseN remaining
```

if it wasn't for the second problem. That definetely kills our mood.

## It's coupled
The second problem arises because we are impatient and we never
settle. We read what we coded so far and we torment ourselves thinking
"Exception sucks. We are functional programmers, damn! We were
supposed to use an `Either` or a `Result` instead".

OK, fine: let's use a `Result`, then.

There are 2 possibilities. We could either return the unconsumed input
only in case of a successful parsinsg:

```fsharp
type ParseError = string

var parser : string -> Result<ParseError, (string * 'a)>
```

or in both cases of success and failure:

```fsharp
var parser : string -> string * Result<ParseError, 'a>
```

You might recognize this last signature as the one of a State Monad
(go read [State Monad for The Rest of
Us](state-monad-for-the-rest-of-us) if you are curious).

Both approaches are viable and both will throw a wreck on the code we
have written so far, making it apparent that we coupled the error
handling concern with the parsing logic.  
Let's use the first signature.

## From Exceptions to functional error handling
Adapting our 2 combinators is a piece of cake:

```fsharp
let rec choice parsers input =
    match parsers with
    | [] -> Failure "All parsers failed"
    | parser::others ->
        let result = parser input
        match result with
        | Success _ as success -> success 
        | Failure err -> parseFirst others input 

let parseSequence (parsers: (string -> string * Result<ParseError, 'a>) list) (input: string) =
    let rec parseRec remainingInput parsers acc =
        match parsers with
        | [] -> (remainingInput, List.rev acc)
        | currentParser::remainingParsers ->
            let (newRemaining, parsedValue) = currentParser remainingInput
            parseRec newRemaining remainingParsers (parsedValue::acc)

    parseRec input parsers []
```

Very good! This shows that factoring out functionalities always pays
off! Unfortunately, the same cannot be said for `parsePerson`:



```fsharp
let parsePerson: string -> Person = fun input ->

    let error e = Failure $"Failed to parse Person because of {e}"

    let (remaining, result) = parseRecord input
    match result with
    | Success (remaining, r) ->
        let (remaining, id) = parseGuid remaining
        match result with
        | Success (remaining, id) ->
            let (remaining, _) = parseUpToName remaining
            match result with
            | Success (remaining, id) ->
                let (remaining, name) = parseString remaining
                match result with
                | Success (remaining, id) ->
                    let (remaining, _) = parseUpToBirthday remaining
                    match result with
                    | Success (remaining, id) ->
                        let (remaining, birthday) = parseBirthday remaining
                        match result with
                        | Success (remaining, id) ->
                            let (remaining, _) = parseTillTheEnd remaining
                            match result with
                            | Success (remaining, id) ->
                                Success (
                                    { Id = id
                                      Name = name
                                      Birthday = birthday } , remaining )
                            | Failure err -> error err
                        | Failure err -> error err
                    | Failure err -> error err
                | Failure err -> error err
            | Failure err -> error err
        | Failure err -> error err
    | Failure err -> error err
```

Holy crap. This is absolutely horrific.

The good news: The attempts to factor this mess out will lead us to
invent Applicative Functors and Monads.

Before proceeding with code, I think it's useful to expand our mind
and realize that there might be a kind of coupling we never thought
of.

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

Take a break, bite an apple, then jump to the next installment.

# References


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

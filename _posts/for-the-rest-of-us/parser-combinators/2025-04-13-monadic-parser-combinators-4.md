---
layout: post
title: "Monadic Parser Combinators in F# - I Told You Not To Mess With
The Signature!"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
- property-based testing
include_in_index: false
---
## Arialdo, you are a liar
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

I have been very reticent. It's OK that `parseGuid` is able to parse a
GUID from the string:

```
*b19b8e87-3d39-4994-8568-0157a978b89a*
```

contained in `guidPart`, but how `parsePerson` managed to extract that
string from the whole:

```
inst Person
   - Id <- *b19b8e87-3d39-4994-8568-0157a978b89a*
   - Name <- <<Richard>>
   - Birthday <- date{16/03/1953}
```

is still a mystery. I just wrote:

```fsharp
let guidPart: string = ...
```

If only it were so easy! Indeed: the syntax of a record is not just a
whole, sequential string; it is interleaved with the field
values. Most likely, `parsePerson` needs to identify the initial
string:

```
inst Person
   - Id <-
```

then it must delegate to `parseGuid`; right after this, it needs to
check that the input continues with:

```

   - Name <-
```

etc. I just glissed on this complexity. What a cheater.  
Not to mention that the input string might contain extra spaces and
newlines such as in:

```
inst
   Person
   - Id       <-   *b19b8e87-3d39-4994-8568-0157a978b89a*
   - Name     <-   <<Richard>>
   - Birthday <-   date{16/03/1953}
```

OK, things start getting really complicated.

We need to break this problem down into smaller ones. An idea that
would immensely help is: each parser could return 3 pieces
of information:

1. The parsed value (this is the main goal of a parser).
2. If it either succeeded or failed (we covered this with Exceptions)
3. How much of the input string it consumed and where it stopped.

The last new information is the key. The next parser can start parsing
where the previous one stopped, so the input string will be consumed,
sequentially, from the first to the last character.

So, rather than:

```fsharp
val parser : string -> 'a
```

a parser would rather have the signature:

```fsharp
val parser : string -> (string, 'a)
```

You might recognize this as the signature of the State Monad (go read
[State Monad for The Rest of Us](state-monad-for-the-rest-of-us) if
you are curious).

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
is deadly boring.

I'm personally too lazy to even copy paste that monotonous code. As it
often happens, developer's laziness is the catalyst of abstraction:
this code immediately ignites our wish to factor the duplication
away into a separate, generic function:


```fsharp
exception ParseException of string

let sequence parsers =
    fun input ->
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

Whatever, probably this is not a very useful combinator, after all. We
have to live with this series of:

```fsharp
let (remaining, value1) = parse1 input
let (remaining, value2) = parse2 remaining
let (remaining, value3) = parse3 remaining
let (remaining, value4) = parse4 remaining
....
let (remaining, valueN) = parseN remaining
```

for a bit more.

Please, notice that this mechanism of passing `remaining` around
&mdash; which is now polluting `parsePerson` &mdash; has nothing to do
with parsing a `Person`: it is the consequence of having changed the
parser signature; if you will, it was caused by a *structural* or a
*non-functional* change. Therefore, it is a problem doomed to affect
all our parsers, from now on. Damn!  
This is what in the previous chapter I called the *effectful logic*.
The *effect* is the need of passing `remaining` around, from a call to
the next one. As long as we won't be able to factor it away somewhere
else (yes: in a Monad), it will spoil the elegance of all our parsers.

## Please, gimme a type
Speaking about elegance, I don't know about you, but these verbose
signatures:

```fsharp
val sequence<'a> (string -> string * 'a) list -> string -> string * 'a list
```

are really starting to get on my nerves. We should do something to
make them simpler. Type aliases for the win! Just defining:

```fsharp
type Parser<'a> = string -> string * 'a
```

turns `<|>` and `sequence`'s signatures to:

```fsharp
val (<|>) : 'a Parser -> 'a Parser -> 'a Parser

val sequence : 'a Parser list -> 'a list Parser
```

Ah! Much, much better!

Don't you feel now inspired to pour a bit more complication into our
parsers? We saw before how a change to the parser signature was
reflected into a more convoluted code structure in the parser
implementation. Let's keep exploring this path to see where it leads
us.


## Friends don't let friends use Exceptions
You read what we coded so far and you torment yourself thinking
"*Exception sucks. I am a functional programmer, damn! I am supposed
to use an `Either` or a `Result` instead!*"

OK, I'm sold: let's use a `Result`, then.

There are 2 possibilities. Either we return the unconsumed input only
in case of a successful parsing:

```fsharp
type ParseError = string
type Input = string
type Rest = string

type Parser<'a> = Input -> Result<Rest * 'a, ParseError>
```

or we return it in any case:

```fsharp
type Parser<'a> = Input -> Rest * Result<'a, ParseError>
```

Notice the position of `Rest`: in one case it is part of the
successful case of `Result`, in the other it is external to `Result`.
Both approaches are viable and both will throw a wreck on the code we
have written so far, making it apparent that we coupled the error
handling concern (the *effectful logic*) with the parsing logic.  
Let's use the first signature.

## From Exceptions to functional error handling
Adapting `<|>` and its tests is a piece of cake:

```fsharp
let (<|>) (first: 'a Parser) (second: 'a Parser): 'a Parser =
    fun input ->
        try
            let result = first input
            match result with
            | Ok (_, _) as ok -> ok
            | Error r ->
                second input

// Tests

type Cases = First | Second

[<Fact>]
let ``uses first parser if successful`` () =
    let successfullyParseFirst input = "rest", First
    let wontBeUsed input = "rest", Second

    let parser = successfullyParseFirst <|> wontBeUsed

    test <@ parser "whatever input" = "rest", First @>

[<Fact>]
let ``falls back to second parser if first parser fails`` () =
    let justFail input = raise ParseException
    let successfullyParseSecond input = "rest", Second

    let parser = justFail <|> successfullyParseSecond

    test <@ parser "whatever input" = "rest", Second @>
```


Voilà , no more exceptions!  
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
code than domain logic! But this was somehow expected: changing the
signature of `Parser` implies some kind *structural logic* to be
executed when parsers &mdash; *all the parsers* &mdash; are executed.
In our case we pushed ourselves to the limit combining 2 structural
changes: passing `remaining` around and matching error cases.

The good news: the attempts to factor this mess out will lead us to
invent Applicative Functors and Monads. Let's reflect how we should
proceed.

A quick espresso? Good idea, it's the perfect moment for a break! See
you at the [5th chapter](monadic-parser-combinators-5).

# References


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

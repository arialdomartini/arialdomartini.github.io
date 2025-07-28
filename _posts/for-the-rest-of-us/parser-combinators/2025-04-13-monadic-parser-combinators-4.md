---
layout: post
title: "Monadic Parser Combinators in F# - I Told You Not To Mess With
The Signature!"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
include_in_index: false
---
## Arialdo, You Are A Liar
When I wrote:

```fsharp
let parsePerson: string -> Person =
    fun input ->
        let parseRecordStructure: string -> string * string * string = __

        let guidPart, namePart, birthdayPart = parseRecordStructure input

        { Id = parseGuid guidPart
          Name = parseString namePart
          Birthday = parseDateOnly birthdayPart }
```

I have been very reticent. It's not hard to believe that `parseGuid`
somehow parses a GUID from the string:

```
*b19b8e87-3d39-4994-8568-0157a978b89a*
```

contained in `guidPart`, but how `parseRecordStructure` managed to
extract that string from the whole:

```
inst Person
   - Id <- *b19b8e87-3d39-4994-8568-0157a978b89a*
   - Name <- <<Richard>>
   - Birthday <- date{16/03/1953}
```

is still a mystery. I just wrote:

```fsharp
let guidPart, namePart, birthdayPart = parseRecordStructure input
```

Voilà! The 3 strings, in one shot! If only it were so easy... Reality
is a bit more complicated than this: the syntax of a record is not
just a simple list of elements; it's a mix of field values and other
syntactic elements. We can imagine that `parseRecordStructure` needs
to identify the initial string:

```
inst Person
   - Id <-
```

Only then it can delegate to `parseGuid`; right after this, it needs
to check that the input continues with:

```

   - Name <-
```

and so on. I just glissed on this complexity. What a cheater.  
Not to mention that the input string might contain extra spaces and
newlines such as in:

```
inst
           Person

           - Id           <-   *b19b8e87-3d39-4994-8568-0157a978b89a*
           - Name         <-   <<Richard>>
           - Birthday     <-   date{16/03/1953}
```

OK, things are getting really sophisticated, now. We need to break
this problem down into smaller ones.

## Passing the Baton
An idea that would immensely help is: each parser could return 3
pieces of information:

1. The parsed value (this is the main goal of a parser).
2. If it either succeeded or failed (we covered this with Exceptions)
3. How much of the input string it consumed &mdash; so, basically,
   where it stopped.

The last new information is the key. The next parser can start parsing
where the previous one finished, so the input string can be consumed,
sequentially, from the first to the last character.

So, rather than:

```fsharp
val parser : string -> 'a
```

a parser would rather have the signature:

```fsharp
val parser : string -> ('a * string)
```

Returning a tuple with the (polymorphic) parsed value *plus* the
unconsumed input, a parser can easily hand the work over to the next
parser. You might recognize this as the signature of the State Monad
(go read [State Monad for The Rest of
Us](state-monad-for-the-rest-of-us) if you are curious). The basic
usage pattern, then, could be:

- Invoke a parser.
- Keep the parsed value in a variable.
- Keep processing: invoke the next parser, feeding it with the
  unconsumed input, so that it can continue from the right position.
- Repeat until you are done with all the syntactic elements.
- Finally, compose all the parsed values into the desired object.
- Return this object *plus* the unconsumed input: after all, this
  parser itself may be part of a larger parser.

With this pattern in mind, `parsePerson` turns into something like:

```fsharp
let parseRecord input = __
let parseGuid input = __
let parseUpToName input = __
let parseString input = __
let parseUpToBirthday input = __
let parseBirthday input = __
let parseTillTheEnd input = __

let parsePerson: string -> (Person * string) =
    fun input ->

        let _, rest = parseRecord input
        let id, rest = parseGuid rest
        let _, rest  = parseUpToName rest
        let name, rest = parseString rest
        let _, rest = parseUpToBirthday rest
        let birthday, rest = parseBirthday rest
        let _, rest
        t parseTillTheEnd rest

        { Id = id
          Name = name
          Birthday = birthday },
        rest
```

No, wait: we also have to consider error handling:

```fsharp
let parsePerson: string -> Person * string =
    fun input ->
        try
            let _, rest = parseRecord input
            let id, rest = parseGuid rest
            let _, rest = parseUpToName rest
            let name, rest = parseString rest
            let _, rest = parseUpToBirthday rest
            let birthday, rest = parseBirthday rest
            let _, rest = parseTillTheEnd rest

            { Id = id
              Name = name
              Birthday = birthday },
            rest
        with ParseException e ->
            raise (ParseException $"Failed to parse Person because of {e}")

```

You can imagine that in the first invocation, `parseRecord` consumes
the string:

```
inst Person
   - Id <- 
```

It can ignore the output: it just needs either to get to the point
where `parseGuid` can proceed, or to fail if the string is not found.  
Similarly `parseUpToName` would consume:

```
   - Name <- 
```

and so on.  
OK, that's not too complicated. But I bet you agree: it's a bit
repetitive. There is nothing capturing the syntax structure, like
something modeling the notion of "each item is prefixed with a field
name and separated by its value by a `<-`". Instead, it's all
mechanical and not very elegant.

Also, passing those `rest` values around is deadly tedious. I'm
personally too lazy to even copy paste that monotonous code. As it
often happens, developers' laziness is the catalyst of abstraction:
this code immediately ignites our wish to factor the duplication away
into a separate, generic function to parse based on a list of parsers,
and to return a list of parsed value (being in a list, necesserily of
the same type):


```fsharp
open Xunit
open Swensen.Unquote

let sequence (parsers: (string -> 'v * string) list) =
    fun (input: string) ->
        let rec parseRec parsers (rest: string) acc =
            match parsers with
            | [] -> (List.rev acc, rest)
            | currentParser :: nextParsers ->
                let parsedValue, newRest = currentParser rest
                parseRec nextParsers newRest (parsedValue :: acc)

        parseRec parsers input []


type Something = Something of int

let mockParser (i: int) (input: string) = (Something i, input[1..])

[<Fact>]
let ``applies all the parsers consuming 1 character for parser`` () =

    let fiveParsers = [ 1..5 ] |> Seq.map mockParser |> Seq.toList

    let parser = fiveParsers |> sequence

    let parsedValues =
        [ Something 1; Something 2; Something 3; Something 4; Something 5 ]

    test <@ parser "12345abc" = (parsedValues, "abc") @>
```

Woah! That's way harder than the previous one. Besides that, isn't it
another Parser Combinator? Does it come in handy for our
`parsePerson`?  Not really, because it requires that all the parsed
elements are members of the same type `'a`. If we really wanted to use
this combinator in `parsePerson`, we would need to make `Guid`,
`string` and `DateOnly` instances of the same type, for example by
wrapping them in a single union type:

```fsharp
type MyTypes =
   | GuidCase of Guid
   | StringCase of string
   | DateOnlyCase of DateOnly
```

While this it surely overkill for a serialization language, it is
indeed the typical approach for programming language parsers. Let's
keep this in mind. Whatever, probably this is not a very useful
building block, after all. We have to live with this series of:

```fsharp
let value1, rest = parse1 input
let value2, rest = parse2 rest
let value3, rest = parse3 rest
let value4, rest = parse4 rest
....
let valueN, rest = parseN rest
```

for a bit more.

Please, note that this mechanism of passing `rest` around &mdash;
which is now polluting `parsePerson` &mdash; has nothing to do with
parsing a `Person`: it is the consequence of having changed the parser
signature; if you will, it was caused by a *structural* or a
*non-functional* change. Therefore, it is a problem doomed to affect
all our parsers, from now on. Damn!  
This is what the previous chapter referred to as the *effectful
logic*.  The *effect* is the need of passing `rest` around, from
a call to the next one. As long as we won't be able to factor it away
somewhere else (yes: in a Monad), it will spoil the elegance of all
our parsers.

## Please, Gimme A Type
Speaking about elegance, I don't know about you, but these verbose
signatures:

```fsharp
val sequence<'a> (string -> 'a * string) list -> string -> 'a list * string
```

are really starting to get on my nerves. We should do something to
make them simpler. Type aliases for the win! Just defining:

```fsharp
type Parser<'a> = string -> 'a * string
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


## Friends Don't Let Friends Use Exceptions
You read what we coded so far and you torment yourself thinking
"*Exception sucks. I am a functional programmer, great Scott! I am
supposed to use an `Either` or a `Result` instead!*"

OK, I'm sold: let's use a `Result`, then.

There are 2 possibilities. Either we return the unconsumed input only
in case of a successful parsing:

```fsharp
type ParseError = string
type Input = string
type Rest = string

type Parser<'a> = Input -> Result<'a * Rest, ParseError>
```

or we return it in any case:

```fsharp
type Parser<'a> = Input -> Result<'a, ParseError> * Rest
```

Note the position of `Rest`: in one case it is part of the
successful case of `Result`, in the other it is external to `Result`.
Both approaches are viable and both will throw a wreck on the code we
have written so far, making it apparent that we coupled the error
handling concern (the *effectful logic*) with the parsing logic.  
Let's use the first signature.

## From Exceptions To Functional Error Handling
Adapting `<|>` and its tests is a piece of cake:

```fsharp
let (<|>) (first: 'a Parser) (second: 'a Parser) : 'a Parser =
    fun input ->
        let result = first input

        match result with
        | Ok _ as ok -> ok
        | Error r -> second input

type Cases =
    | First
    | Second

[<Fact>]
let ``uses first parser if successful`` () =
    let successfullyParseFirst input = Ok(First, "rest")
    let wontBeUsed input = Ok(Second, "rest")

    let parser = successfullyParseFirst <|> wontBeUsed

    test <@ parser "whatever input" = Ok(First, "rest") @>

[<Fact>]
let ``falls back to second parser if first parser fails`` () =
    let justFail input = Error "I was meant to fail"
    let successfullyParseSecond input = Ok(Second, "rest")

    let parser = justFail <|> successfullyParseSecond

    test <@ parser "whatever input" = Ok(Second, "rest") @>
```


Voilà, no more exceptions!  
Unfortunately, the same cannot be said for `parsePerson`:


```fsharp
let parsePerson: Person Parser =
    fun input ->

        match parseRecord input with
        | Ok(_, rest) ->
            match parseGuid rest with
            | Ok(id, rest) ->
                match parseUpToName rest with
                | Ok(_, rest) ->
                    match parseString rest with
                    | Ok(name, rest) ->
                        match parseUpToBirthday rest with
                        | Ok(_, rest) ->
                            match parseBirthday rest with
                            | Ok(birthday, rest) ->
                                match parseTillTheEnd rest with
                                | Ok(_, rest) ->
                                    Ok(
                                        { Id = id
                                          Name = name
                                          Birthday = birthday },
                                        rest
                                    )
                                | Error err -> Error err
                            | Error err -> Error err
                        | Error err -> Error err
                    | Error err -> Error err
                | Error err -> Error err
            | Error err -> Error err
        | Error err -> Error err
```

Holy cow! This is absolutely horrific. There is more error control
code than domain logic! But this was somehow expected: changing the
signature of `Parser` implies some kind *structural logic* to be
executed when parsers &mdash; *all the parsers* &mdash; are executed.
In our case we pushed ourselves to the limit combining 2 structural
changes: passing `rest` around and matching error cases. And we got a
[Pyramid of Doom][pyramid-of-doom]

The good news: the attempts to factor this mess out will lead us to
invent Applicative Functors and Monads. Let's reflect how we should
proceed.

A quick espresso? Good idea, it's the perfect moment for a break! See
you at the [5th chapter](monadic-parser-combinators-5).

[Previous - That's a Combinator!](/monadic-parser-combinators-3)
⁓ [Next - A Different Kind of Coupling](/monadic-parser-combinators-5)


# References
- [Pyramid of Doom][pyramid-of-doom]

[pyramid-of-doom]: https://en.wikipedia.org/wiki/Pyramid_of_doom_(programming)

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

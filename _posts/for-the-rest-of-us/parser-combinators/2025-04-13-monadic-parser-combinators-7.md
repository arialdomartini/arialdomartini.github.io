---
layout: post
title: "Monadic Parser Combinators in F# - bind, ap, map"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
- property-based testing
ftrou: true
include_in_index: false
---
Time to build an applicative and monadic parser combinator library.
Warm up the keyboard!

Until this point, we have worked with the type:

```fsharp
type ParseError = string
type Input = string
type Rest = string

type Parser<'a> = Input -> Result<Rest * 'a, ParseError>
```

So defined, `Parser` is a type alias. It could be a good idea to wrap
the function in its own type, in a Single-case Discriminated Union:

```fsharp
type Parser<'a> = Parser of Input -> Result<Rest * 'a, ParseError>
```

Notice the `Parser of` case constructor. Fine, that's the starting point.

## A grammar for a parsing language

Let's give a glimpse to the ending point, too. Over the course of this
chapter, we will develop several functions and operators. They will
form the syntactic elements of a language grammar with which you'll be
able to build whatever parser.

Let me list them below so you know right away where we're headed.

| Name        | Alternative name | Signature                                                                    | Purpose                                                                      |
|-------------|------------------|------------------------------------------------------------------------------|------------------------------------------------------------------------------|
| `.>>.`      | `andThen`        | `'a Parser -> 'b Parser -> ('a * 'b) Parser`                                 | Parse `'a`, then `'b`, and finally return both in a tuple.                   |
| `>>=`       | `bind`           | `'a Parser -> ('a -> 'b Parser) -> 'b Parser`                                | Parse `'a` and pass it to a continuation.                                    |
| `<!>`       | `map`            | `('a -> 'b) -> 'a Parser -> 'b Parser`                                       | Transform a Parser of `'a` into a Parser of `'b`.                            |
| `|>>`       | `pipe`           | `'a Parser -> ('a -> 'b) -> 'b Parser`                                       | Like F# pipe operator `|>`, but operating with Parsers.                      |
| `<*>`       | `ap`             | `('a -> 'b) Parser -> 'a Parser -> 'b Parser`                                | Partial application of a Parser argument to a multi-parameters function.     |
| `<|>`       | "or"             |                                                                              | Try applying a Parser. It if fails, try another one.                         |
| `.>>`       |                  |                                                                              | Apply 2 parsers, returning the result of the first one only.                 |
| `>>.`       |                  |                                                                              | Apply 2 parsers, returning the result of the second one only.                |
| `many`      |                  | `'a Parser -> 'a list Parser`                                                | Repeatedly apply a parser until it fails, returning a list of parsed values. |
| `many1`     |                  | `'a Parser -> 'a list Parser`                                                | Same as above, but expects at least 1 occurrence.                            |
| `skipMany`  |                  | `'a Parser -> () Parser`                                                     | Parse zero or more occurrences of something, discarding the result.          |
| `skipMany1` |                  | `'a Parser -> () Parser`                                                     | Same as above, but expects at least 1 occurrence.                            |
| `between`   |                  | `'o Parser -> 'c Parser -> 'a Parser -> 'a Parser`                           | Parse something between opening and closing elements.                        |
| `sepBy`     |                  | `'a Parser -> 'b Parser -> 'a list Parser`                                   | Parse a list of `'a` elements separate by `b`.                               |
| `returnp`   |                  | `'a -> 'a Parser`                                                            | Lift a plain value into the Parser world.                                    |
| `lift`      | Synonym of `map` |                                                                              | Elevate a 1-parameter function into the Parsers world.                       |
| `lift2`     |                  | `('a -> 'b -> 'c) -> 'a Parser -> 'b Parser -> 'c Parser`                    | Elevate a 2-parameter function into the Parsers world.                       |
| `lift3`     |                  | `('a -> 'b -> 'c -> 'd) -> 'a Parser -> 'b Parser -> 'c Parser -> 'd Parser` | Elevate a 3-parameter function into the Parsers world.                       |
| `pipe2`     |                  | `'a Parser -> 'b Parser -> ('a -> 'b -> 'c) -> 'c Parser`                    | Apply 2 Parser arguments to a 2-parameter function expecting values.         |
| `pipe3`     |                  | `'a Parser -> 'b Parser -> 'c Parser -> ('a -> 'b -> 'c -> 'd) -> 'd Parser` | Apply 3 Parser arguments to a 3-parameter function expecting values.         |

Don't feel overwhelmed. They are way easier to write than they appear
at first.

## Parser-powered function application
So, the idea is to develop an alternative to the native F# function
application that takes care, under the hood, to pass `remaining`
around and to do error handling.

What's the native F# function application, for a starter?

```fsharp
[<Fact>]
let ``function application`` () =
    let twice x = x * 2

    test <@ twice 42 = 84 @>
```

Do you see that whitespace between `twice` and `42`?

```fsharp
            twice 42
                 ^
```

With a bit of fantasy, you imagine that this is an actual operator. If
it were real, its signature would be:

```fsharp
let ( ) (f: 'a -> 'b) (a: 'a) : 'b = ....
```

This is fictional code: an operator cannot have a whitespace as its
name. But wait a moment! F# *does provide* an actual operator with
that exact signature! It's `<|`! Its signature and implementation
could be:

```fsharp
let (<|) (f: 'a -> 'b) (a: 'a) : 'b = f a

[<Fact>]
let ``function application`` () =
    let twice x = x * 2

    test <@ twice <| 42 = 84 @>
```


Its real implementation ([FSharp.Core/prim-types.fs#L4546][apply-source]) is:

```fsharp
let inline (<|) func arg1 = func arg1
```

`<|` is the same of the famous pipe operator `|>`, with flipped
parameters:


```fsharp
let (|>) (a: 'a) (f: 'a -> 'b) : 'b = f a

[<Fact>]
let ``function application with pipe`` () =
    let twice x = x * 2

    test <@ 42 |> twice = 84 @>
```

`|>` is actually defined like this in the F# code ([FSharp.Core/prim-types.fs#L4540][pipe-source]):

```fsharp
let inline (|>) arg func = func arg
```




## andThen

This Tupled Sequencing Combinator

sequence and keep both


- Run the first parser.
- If it fails, return the error.
- Otherwise, run the second parser with the remaining input.
- If it fails, return the error.
- If both parsers succeed, return a tuple with both the parsed values.

# References

- [F# source code: `<|` operator][apply-source]
- [F# source code: `|>` operator][pipe-source]


[apply-source]: https://github.com/dotnet/fsharp/blob/main/src/FSharp.Core/prim-types.fs#L4546
[pipe-source]: https://github.com/dotnet/fsharp/blob/main/src/FSharp.Core/prim-types.fs#L4540

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

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

So defined, `Parser` is a type alias. With regards to encapsulation,
it's a good idea to wrap the function on the right side in its own
type, in a Single-case Discriminated Union:

```fsharp
type Parser<'a> = Parser of Input -> Result<Rest * 'a, ParseError>
```

Notice the `Parser of` case constructor. We can also provide a type to
the returned value:

```fsharp
type ParseResult<'a> =
    | Success of (Rest * 'a)
    | Failure of ParseError

type Parser<'a> = Parser of (Input -> 'a ParseResult)
```

This defined, the detail about passing `remaining` and handling errors
is not directly visible from the outside. Good for information hiding.
A drawback of having the function wrapped inside a `Parser`, though,
it that you can't direcly apply it to a string input. A helper
function will come in handy:

```fsharp
let run (p: 'a Parser) (input: string)=
    match p with
    | Parser f -> f input


// Test

type SomeType = One | Two | Three

[<Fact>]
let ``runs a parser`` () =
    let mockParser = Parser(fun input -> Success ("rest", One))

    test <@ run mockParser "any input" = Success ("rest", One) @>
```

Actually, you can simplify `run` as:

```fsharp
let run (Parser f) input =
    f input
```

If you think to `Parser` as a container, you can imagine `run` as the
function that *opens* it and reveals its content. This is a common
theme when working with monads: even if monads are not boxes &mdash;
nor burritos &mdash; some times the idea of *opening a monad*,
operating with its content, and then *wrapping* the result back in
another monad comes in handy.  
Fine. Anyway, the `Parser` type and the `run` function are an
excellent starting point.

## A grammar for a parsing language

Let's give a look to the ending point, too. Over the course of the
next chapters, we will develop several functions and operators. They
will form the syntactic elements of a grammar with which you'll be
able to build whatever parser.

Let me list them below so you know right away where we're headed.

| Name        | Alternative name | Signature                                                                    | Purpose                                                                      |
|-------------|------------------|------------------------------------------------------------------------------|------------------------------------------------------------------------------|
| `.>>.`      | `andThen`        | `'a Parser -> 'b Parser -> ('a * 'b) Parser`                                 | Parse `'a`, then `'b`, and finally return both in a tuple.                   |
| `>>=`       | `bind`           | `'a Parser -> ('a -> 'b Parser) -> 'b Parser`                                | Parse `'a` and pass it to a continuation.                                    |
| `<!>`       | `<<|`, `map`     | `('a -> 'b) -> 'a Parser -> 'b Parser`                                       | Transform a Parser of `'a` into a Parser of `'b`.                            |
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
at first. In this chapter you will build `|>>` and `<<|`.

## From the F# native function application...
So, the idea is to develop an alternative to the F# native function
application that, under the hood, takes care of passing `remaining`
around and of handling errors.

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

With a bit of fantasy, you can imagine that this is an actual
operator: it applies `twice` to the value `42`. If it was a real
operator, its signature would be:

```fsharp
val ( ) : ('a -> 'b) -> 'a -> 'b
```

This is fictional code, though: a whitespace cannot be used as an
operator name. But wait a moment! F# *does provide* an actual operator
with that exact signature! It's `<|`! If you were to write it
manually, you would have:

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

Almost identical! `<|` is the same of the famous pipe operator `|>`,
only, with flipped parameters. Let's reinvent `|>` too:


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

## ...to a parser-powered function application

Fine. So, if we want to define a parser-powered function application
we just need to change the signature of `<|` to accept an `'a Parser`
instead of just an `'a`, right? We can try to define 2 enhanced
operators:


| F# native | Parser-powered |
|-----------|----------------|
| `<|`      | `<<|`          |
| `|>`      | `|>>`          |


with these signatures:

| Version        | Apply                                    | Pipe                                     |
|----------------|------------------------------------------|------------------------------------------|
| F# native      | `(  <| ): ('a -> 'b) -> 'a        -> 'b` | `( |> ):  'a        -> ('a -> 'b) -> 'b` |
| Parser-powered | `( <<| ): ('a -> 'b) -> 'a Parser -> 'b` | `( |>> ): 'a Parser -> ('a -> 'b) -> 'b` |



```fsharp
let (<<|) (f: 'a -> 'b) (a: 'a Parser) : 'b =
    failwith "Not yet implemented"

let (|>>) (a: 'a Parser) (f: 'a -> 'b) : 'b =
    failwith "Not yet implemented"

// A never-failing mock parser always returning 42
let p42: int Parser =
    Parser (fun _ -> Success("rest", 42))

[<Fact>]
let ``parser-powered function application`` () =
    let twice x = x * 2

    test <@ twice <<| p42 = 84 @>
    test <@ p42 |>> twice = 84 @>
```

But this is a complete nosense! Think about it: an `int Parser` is not
an `int` value; it's *a promise* of an `int`. Put your ear up to it
and you'll hear it whispering:

> True, I am not an integer. But I promise I can provide you with one.
> Just feed me with an input string: I will do my best and, if I don't
> fail, I will eventually give you back an `int` value".

Read the 2nd to last line again:

```fsharp
    test <@ twice <<| pint = 84 @>
```

How can possibly `twice <<| p42` return `84` if `pint` will
materialize the value `42` only *after* it is fed with an input? If
`twice <<| p42` could speak, it would tell you:

> I see what you want to do! You want to multiply some integer 
> value by `2`.  
> I also see that you don't have that integer value just yet:
> you will eventually obtain one as soon as you have
> parsed an input string.
>
> Fine, let's make a deal: I will parse that input string
> for you, so I will be able to finally perform my calculation.
> I promise.
>
> I mean: you gave me a Parser, I pay you back with
> another Parser. It's only fair.

This means that the signature of our enhanced operators should rather
be:


| Version        | Apply                                           | Pipe                                            |
|----------------|-------------------------------------------------|-------------------------------------------------|
| F# native      | `(  <| ): ('a -> 'b) -> 'a        -> 'b`        | `( |> ):  'a        -> ('a -> 'b) -> 'b`        |
| Parser-powered | `( <<| ): ('a -> 'b) -> 'a Parser -> 'b Parser` | `( |>> ): 'a Parser -> ('a -> 'b) -> 'b Parser` |

Notice that both `<<|` and `|>>` now return a `'b Parser`. This means
that the assertions:

```fsharp
test <@ twice <<| p42 = 84 @>
test <@ p42 |>> twice = 84 @>
```

will not compile: we cannot compare the returned `'b Parser` with
`84`: what you got back is a Parser that will eventually return `84`.
Is a sense, you got back a future, successfully parsed `84` wrapped in
a box. You need `run` to unpeel it:

```fsharp
test <@ run (twice <<| p42) "some input" = Success ("rest", 84) @>
test <@ run (p42 |>> twice) "some input" = Success ("rest", 84) @>
```

It compiles.  
Now that we are armed with a failing test, we are ready to implement
`|>>` and `<<|`. Not only can you be driven by tests: types can drive
you too. Listen to the signature:

```fsharp
let (<<|) (f: 'a -> 'b) (ap: 'a Parser) : 'b Parser = ...
```

It tells you to return an instance of `Parser`. Fine, let's obey. We
need to invoke the `Parser` case constructor:

```fsharp
let (<<|) (f: 'a -> 'b) (ap: 'a Parser) : 'b Parser =
    Parser ...
```

What does the case constructor want as an argument? A function from an
input `string` to something. Fine, type system, I trust you:

```fsharp
let (<<|) (f: 'a -> 'b) (ap: 'a Parser) : 'b Parser =
    Parser (fun input ->
        ...)
```

Now:

- If you want to return a `'b Parser` you have to obtain a `'b` value.
- The only way to get one is through `f`, which is an `'a -> 'b`.
- In order to invoke `f` you need an `'a` value.
- Damn. You don't have it. You have a parser of `'a`, instead, so
  something that can give you an `'a` value, if only you run it with a
  `string` input.
- But look! You do have a `string` input, because you live inside a
  lambda!
- So, just *run* `ap` with `input` and you will get an `a
  ParseResult`. If it is successful, you will find the `'a` value
  there, together with the unconsumed input.
- (If it fails, you are safe to let `<<|` fail too).
- With the obtained `'a` value, you can finally invoke `f`, and get a
  `'b` value.
- You can wrap the `'b` value and the unconsumed value in a tuple, and
  finally return it, successfully.

Let's translate all of this to code:

```fsharp
let (<<|) (f: 'a -> 'b) (ap: 'a Parser) : 'b Parser =
    Parser (fun input ->
        let ar : 'a ParseResult = run ap input
        match ar with
        | Success (rest, a) -> Success (rest, f a)
        | Failure s -> Failure s )
```

Relying on type inference, you can simplify this to:


```fsharp
let (<<|) f ap =
    Parser (fun input ->
        match run ap input with
        | Success (rest, a) -> Success (rest, f a)
        | Failure s -> Failure s )
```

Not bad! And not too difficult, after all. This function captures the
*effectful logic*:

- It passes the unconsumed input forward.
- It deals with failures.

Also notice that it does not contain any logic other than this:

- `f` encapsulates the actual domain logic.
- `<<|` encapsulates the *glueing* logic.


They have nothing in common. This is a very good separation of
concerns. As we said in [chapter 5](monadic-parser-combinators-5),
it's a form of structural decoupling, obtained with a tool other than
the classic dependency injection.


Defining `|>>` is trivial: just swap the parameters:

```fsharp
let (|>>) a f = f <<| a
```

Green tests. Hurray!  
Let's see a couple of examples how you can use these brand new operators.

## What an epic time to live!
Imagine that for your language you already managed to write a
`DateTime` parser which expects dates to be written in the boring
format `yyyy-MM-dd hh:mm:ss`:


```fsharp
let dateTimeP: DateTime Parser =
    Parser (fun input ->
        match DateTime.TryParse(input[..18]) with
        | true, dateTime -> Success (input[19..], dateTime)
        | false, _ -> Failure "Expected a date")

[<Fact>]
let ``date test`` () =
    test <@ run dateTimeP "2025-01-01 18:11:12, the rest" = Success (", the rest", DateTime(2025, 01, 01, 18, 11, 12)) @>
```

You want to find a convenient internal format for storing the date. Of
course, you really dislike the native .NET `DateTime`: your language
will eventually replace C#, F# and the whole .NET anyway, so
what's the point in using it?  
You once read about the [Epoch time][epoch-time] which represents time
by the number of seconds since the midnight of 1 January 1970. Why
1970, you wonder? Because Unix was created in the early 1970s.  
What a lame excuse! You rather prefer a format that celebrates
yourself. What about using:

```fsharp
type EpicTime = EpicTime of float
```

measuring time as the number of second since where *you* were born?
After all, that was the most epic moment in the universe history.

(Let's say, it was on 18 February 2005, at 18:24:02, OK?)

Here's how to convert a depressing `DateTime` to a gorgeous
`EpicTime`:

```fsharp
let toEpicTime (date: DateTime) =
    let aRemarkableDate = DateTime(2005, 02, 28, 18, 24, 02)
    let secondsFromTheEpicDate = date.Subtract(aRemarkableDate).TotalSeconds
    EpicTime secondsFromTheEpicDate

[<Fact>]
let ``Epic Time is so much better than Epoch Time`` () =
    test <@ DateTime(2025, 01, 01, 18, 11, 12) |> toEpicTime = EpicTime 626140030.0 @>
```

Of course, all your language users will agree that `EpicTime
626140030.0` is a way more convenient representation than `01 January
2025 18:11:12`.

Now you need an `EpicTime Parser`. Presto! You can combine the
`DateTime Parser` with `toEpicTime`, using `<<|`:

```fsharp
let epicTime : EpicTime Parser =
    toEpicTime <<| dateTimeP

[<Fact>]
let ``epicTime test`` () =
    test <@ run epicTime "2025-01-01 18:11:12, the rest" = Success (", the rest", EpicTime 626140030.0) @>
```

Look how concise:

```fsharp
let epicTime = dateTimeP |>> toEpicTime
```

We know that this expression is manipulating functions wrapped in a
`Parser` type. It does not even mention a single function parameter.
This style is called Point Free Style or [Tacit
Programming][tacit-programming], and is typical when playing with
functional combinators. It's fundamentally the consequence of
manipulating things in an elevated, more abstract context. We will get
back to this later.


## Mapping strings to types
Let's see an other simple example.  
You want to parse keywords of your programming language. You can start
with a parsers that checks when the input contains specific keywords.
Later, you can refine it to return specific custom types.

To check the presence of a specific string, if you want to be generic,
you can define a parser factory, something that gets the string you
want to match and generates a parser for it. That is, a parser failing
if the string is not found:

```fsharp
let str (s: string) =
    Parser (fun input ->
        if input.StartsWith(s) then Success (input[s.Length..], s)
        else Failure $"Expected {s}" )

[<Fact>]
let ``test str`` () =
    test <@ run (str "foo") "foobar" = Success ("bar", "foo") @>
    test <@ run (str "foo") "barbaz" = Failure ("Expected foo") @>
```

Using this generic `str`, you can define booleans for your language.
With a stroke of genius you take the decision to have three-state
booleans:

```fsharp
type Boolish =
    | SoTrue
    | SoFalse
    | Occasionally
```

Which keywords should your language use? `"true"` and `"false"` are so
overrated and boring... What about using German instead?

| Boolish value  | Keyword        |
|----------------|----------------|
| `SoTrue`       | `richtig`      |
| `SoFalse`      | `falsch`       |
| `Occasionally` | `gelegentlich` |


Sounds promising! Building a parser for the keyword `falsch` is
straighforward if you use `str`:

```fsharp
let falschParser: string Parser = str "falsch"

[<Fact>]
let ``parsing falsch`` () =

    test <@ run falschParser "falsch as a 3 dollar bill" = Success (" as a 3 dollar bill", "falsch") @>
    test <@ run falschParser "if(2+2=5 -> gelegentlich) then foo()" = Failure ("Expected falsch") @>
```

This is OKish. The problem is that `falschParser` is a `String Parser`,
not a `Boolish Parser`. `|>>` to the resque!

```fsharp
let boolishFalscheP = falseRaw |>> (fun _ -> SoFalse)

[<Fact>]
let ``boolish test`` () =
    test <@ run boolishFalscheP "falsch as a 3 dollar bill" = Success (" as a 3 dollar bill", SoFalse) @>
```

## F-word
Do you know what you have just done? You have re-invented functors!
Read `<<|`'s signature again:

```fsharp
let (<<|) (f: 'a -> 'b) (ap: 'a Parser) : 'b Parser =
    Parser (fun input ->
        let ar : 'a ParseResult = run ap input
        match ar with
        | Success (rest, a) -> Success (rest, f a)
        | Failure s -> Failure s )
```

Given that all the functions in F# are curried, there are 2 ways to
interpret it:

| Definition                    | Input                              | Output                   | Interpretation                         |
|-------------------------------|------------------------------------|--------------------------|----------------------------------------|
| `let (<<|) f ap = ...`        | `(f: 'a -> 'b) -> (ap: 'a Parser`) | `'b Parser`              | Apply `f` to the value parsed by `ap`. |
| `let (<<|) f = fun ap -> ...` | `(f: 'a -> 'b)`                    | `'a Parser -> 'b Parser` | Lift `f` to the Parser world           |


You can easily apply the 1st interpretation to the `EpicTime` case.
You had a function from `DateTime` to `EpicTime`:

```fsharp
val toEpicTime : DateTime -> EpicTime
```

and you wanted to apply it to *the value inside the* `DateTime Parser` box:

```fsharp
val dateTimeP: DateTime Parser
```

You did it with:

```fsharp
let epicTime : EpicTime Parser =
    toEpicTime <<| dateTimeP
```

The types in the game are:

| Element      | Signature                                                        |
|--------------|------------------------------------------------------------------|
| `toEpicTime` | ` DateTime -> EpicTime`                                          |
| `dateTimeP`   | `                            DateTime Parser`                    |
| `<<|`        | (`DateTime -> EpicTime`) -> `DateTime Parser -> EpicTime Parser` |


You see how `toEpicTime` is applied to "the content" of `dateTimeP`.

The second interpretation arises as soon as you partially apply `<<|`.

```fsharp
let epicTime' = (<<|) toEpicTime
```

Having an operator used in prefix fashion is a bit weird. Let's define
an alias. You can call it either `map` or `lift` &mdash; the reason 
should be immediately clear:


```fsharp
let map = (<<|)
```

Read `map`'s signature as:

```fsharp
val map: ('a -> 'b) -> ('a Parser -> 'b Parser)
```

`map` is that combinator that given a function `f` operating on
ordinary values (`'a -> 'b`) *lifts* it to work on parsers (`('a
Parser -> 'b Parser)`). It maps things from the lower world to the elevated, Parser-powered universe.  
Look:

- You have `toEpicTime: DateTime -> EpicTime`.
- Tou lift `toEpicTime`:

```fsharp
let toEpicTimeP = map toEpicTime
```

- It was transformed to `toEpicTimeP: Parser DateTime -> Parser EpicTime`.

Now you can feed it with a parser:

```fsharp
let epicTimeParser = toEpicTimeP dateTimeP
```

This will give you back an `EpicTime Parser`:

```fsharp
[<Fact>]
let ``date test`` () =
    test <@ run epicTimeParser "2025-01-01 18:11:12, the rest" = Success (", the rest", EpicTime 626140030.0) @>
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
- [Tacit Programming][tacit-programming]
- [Epoch time][epoch-time]

[apply-source]: https://github.com/dotnet/fsharp/blob/main/src/FSharp.Core/prim-types.fs#L4546
[pipe-source]: https://github.com/dotnet/fsharp/blob/main/src/FSharp.Core/prim-types.fs#L4540
[tacit-programming]: https://en.wikipedia.org/wiki/Tacit_programming
[epoch-time]: https://en.wikipedia.org/wiki/Unix_time

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

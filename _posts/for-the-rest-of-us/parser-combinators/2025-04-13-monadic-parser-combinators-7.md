---
layout: post
title: "Monadic Parser Combinators in F# - Parser-Powered Function Application"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
include_in_index: false
---
Until this point, we have worked with these types:

```fsharp
type ParseError = string
type Input = string
type Rest = string

type Parser<'a> = Input -> Result<'a, Rest, ParseError>
```

So defined, `Parser` is a type alias. It's a good idea to improve the
encapsulation wrapping the function on the right side in a Single-case
Discriminated Union:

```fsharp
type Parser<'a> = Parser of (Input -> Result<'a * Rest, ParseError>)
```

Note the `Parser of` case constructor. We can also grant
`Result<Rest * 'a, ParseError>` the dignity of its own type,
independent from `Result`:

```fsharp
type ParseResult<'a> =
    | Success of ('a * Rest)
    | Failure of ParseError

type Parser<'a> = Parser of (Input -> 'a ParseResult)
```

So defined, the details about passing `rest` and handling errors are
not directly visible from the outside. Good for information hiding.  A
drawback of having the function wrapped inside `Parser`, though, is
that you can't direcly apply it to a string input anymore. A helper
function will come in handy:

```fsharp
let run (p: 'a Parser) (input: string)=
    match p with
    | Parser f -> f input


type SomeType =
    | One
    | Two
    | Three

[<Fact>]
let ``runs a parser`` () =
    let mockParser = Parser(fun _ -> Success(One, "rest"))

    test <@ run mockParser "any input" = Success(One, "rest") @>
```

Actually, you can simplify `run` as:

```fsharp
let run (Parser f) input =
    f input
```

If you think to `Parser` as a container, you can imagine `run` as the
function that *opens* it and reveals its content. This is a common
theme when working with monads: even if monads are not boxes &mdash;
nor burritos &mdash; sometimes the idea of *opening a monad*,
operating with its content, and then *wrapping* the result back in
another Monad comes in handy.  
Fine. The `Parser` type and the `run` function are an excellent
starting point.

## A Grammar For A Parsing Language

Let's give a look to the ending point, too. Over the course of the
next chapters, we will develop several functions and operators. They
will form the syntactic elements of a grammar with which you'll be
able to build whatever parser.

Let me list them below so you know right away where we're headed.

<table border="1" cellpadding="6" cellspacing="0" style="border-collapse:collapse; width:100%;">
  <thead>
    <tr>
      <th>Name</th>
      <th style="width:60%;">Signature</th>
      <th>Purpose</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><code>.>>.</code><br/><code>andThen</code></td>
      <td><code>'a Parser -> 'b Parser -> ('a * 'b) Parser</code></td>
      <td>Parse <code>'a</code>, then <code>'b</code>, and finally return both in a tuple.</td>
    </tr>
    <tr>
      <td><code>>>=</code><br/><code>bind</code></td>
      <td><code>'a Parser -> ('a -> 'b Parser) -> 'b Parser</code></td>
      <td>Parse <code>'a</code> and pass it to a continuation.</td>
    </tr>
    <tr>
      <td><code><!></code><br/><code><<|</code><br/><code>map</code></td>
      <td><code>('a -> 'b) -> 'a Parser -> 'b Parser</code></td>
      <td>Transform a Parser of <code>'a</code> into a Parser of <code>'b</code>.</td>
    </tr>
    <tr>
      <td><code>|>></code><br/><code>pipe</code></td>
      <td><code>'a Parser -> ('a -> 'b) -> 'b Parser</code></td>
      <td>Like F# pipe operator <code>|></code>, but operating with Parsers.</td>
    </tr>
    <tr>
      <td><code><*></code><br/><code>ap</code></td>
      <td><code>('a -> 'b) Parser -> 'a Parser -> 'b Parser</code></td>
      <td>Partial application of a Parser argument to a multi-parameters function.</td>
    </tr>
    <tr>
      <td><code><|></code></td>
      <td><code>'a Parser -> 'a Parser -> 'a Parser</code></td>
      <td>Try applying a Parser. It if fails, try another one.</td>
    </tr>
    <tr>
      <td><code>.>></code></td>
      <td><code>'a Parser -> 'b Parser -> 'a Parser</code></td>
      <td>Apply 2 parsers, returning the result of the first one only.</td>
    </tr>
    <tr>
      <td><code>>>.</code></td>
      <td><code>'a Parser -> 'b Parser -> 'b Parser</code></td>
      <td>Apply 2 parsers, returning the result of the second one only.</td>
    </tr>
    <tr>
      <td><code>many</code></td>
      <td><code>'a Parser -> 'a list Parser</code></td>
      <td>Repeatedly apply a parser until it fails, returning a list of parsed values.</td>
    </tr>
    <tr>
      <td><code>many1</code></td>
      <td><code>'a Parser -> 'a list Parser</code></td>
      <td>Same as above, but expects at least 1 occurrence.</td>
    </tr>
    <tr>
      <td><code>skipMany</code></td>
      <td><code>'a Parser -> () Parser</code></td>
      <td>Parse zero or more occurrences of something, discarding the result.</td>
    </tr>
    <tr>
      <td><code>skipMany1</code></td>
      <td><code>'a Parser -> () Parser</code></td>
      <td>Same as above, but expects at least 1 occurrence.</td>
    </tr>
    <tr>
      <td><code>between</code></td>
      <td><code>'o Parser -> 'c Parser -> 'a Parser -> 'a Parser</code></td>
      <td>Parse something between opening and closing elements.</td>
    </tr>
    <tr>
      <td><code>sepBy</code></td>
      <td><code>'a Parser -> 'b Parser -> 'a list Parser</code></td>
      <td>Parse a list of <code>'a</code> elements separate by <code>b</code>.</td>
    </tr>
    <tr>
      <td><code>returnp</code></td>
      <td><code>'a -> 'a Parser</code></td>
      <td>Lift a plain value into the Parser world.</td>
    </tr>
    <tr>
      <td><code>lift</code><br/><code>map</code></td>
      <td><code>('a -> 'b) -> 'a Parser -> 'b Parser</code></td>
      <td>Elevate a 1-parameter function into the Parsers world.</td>
    </tr>
    <tr>
      <td><code>lift2</code></td>
      <td><code>('a -> 'b -> 'c) -> 'a Parser -> 'b Parser -> 'c Parser</code></td>
      <td>Elevate a 2-parameter function into the Parsers world.</td>
    </tr>
    <tr>
      <td><code>lift3</code></td>
      <td><code>('a -> 'b -> 'c -> 'd) -> 'a Parser -> 'b Parser -> 'c Parser -> 'd Parser</code></td>
      <td>Elevate a 3-parameter function into the Parsers world.</td>
    </tr>
    <tr>
      <td><code>pipe2</code></td>
      <td><code>'a Parser -> 'b Parser -> ('a -> 'b -> 'c) -> 'c Parser</code></td>
      <td>Apply 2 Parser arguments to a 2-parameter function expecting values.</td>
    </tr>
    <tr>
      <td><code>pipe3</code></td>
      <td><code>'a Parser -> 'b Parser -> 'c Parser -> ('a -> 'b -> 'c -> 'd) -> 'd Parser</code></td>
      <td>Apply 3 Parser arguments to a 3-parameter function expecting values.</td>
    </tr>
  </tbody>
</table>


Don't feel overwhelmed. They are way easier to write than they appear
at first.  
As a starter, we will build `|>>` and `<<|`.

## From The F# Native Function Application...
Our goal is to develop an alternative to the F# native function
application that, under the hood, takes care of passing `rest` around
and of handling errors. This bears the question: what's the native F#
function application, to begin with?

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
operator: it applies `twice` to the value `42`. If it were a real
operator, its signature would be:

```fsharp
val ( ) : ('a -> 'b) -> 'a -> 'b
```

This is fictional code, though: a whitespace cannot be used as an
operator name. But wait a moment! F# *does provide* an actual operator
with that exact signature! It's `<|`. If you were to write it
manually, you would have:

```fsharp
let (<|) (f: 'a -> 'b) (a: 'a) : 'b = f a

[<Fact>]
let ``function application, via an operator`` () =
    let twice x = x * 2

    test <@ twice <| 42 = 84 @>
```

Its real implementation ([FSharp.Core/prim-types.fs#L4546][apply-source]) is:

```fsharp
let inline (<|) func arg1 = func arg1
```

Almost identical! Oh, that feeling when a reimplementation matches the
original! So gratifying...  
`<|` is the same of the famous pipe operator `|>`, only with flipped
parameters. Let's reinvent `|>` too:


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

## ...To A Parser-Powered Function Application

If we want to define a parser-powered function application, don't we
just need to change the signature of `<|` to accept an `'a Parser`
instead of just an `'a`? Good question. Let's see. We can try to
define 2 enhanced operators:


| F# native | Parser-powered |
|-----------|----------------|
| `<|`      | `<<|`          |
| `|>`      | `|>>`          |


with these signatures:

| Version        | Operator | Signature                       |
|----------------|----------|---------------------------------|
| F# native      | `<|`     | `('a -> 'b) -> 'a -> 'b` |
| Parser-powered | `<<|`    | `('a -> 'b) -> 'a Parser -> 'b` |

and:

| Version        | Operator | Signature                       |
|----------------|----------|---------------------------------|
| F# native      | `|>`     | `'a        -> ('a -> 'b) -> 'b` |
| Parser-powered | `|>>`    | `'a Parser -> ('a -> 'b) -> 'b` |



```fsharp
let (<<|) (f: 'a -> 'b) (aP: 'a Parser) : 'b = __

let (|>>) (aP: 'a Parser) (f: 'a -> 'b) : 'b = __

// A never-failing mock parser always returning 42
let p42: int Parser =
    Parser (fun _ -> Success(42, "rest"))

[<Fact>]
let ``parser-powered function application`` () =
    let twice x = x * 2

    test <@ twice <<| p42 = 84 @>
    test <@ p42 |>> twice = 84 @>
```

But this is a complete nonsense! Think about it: an `int Parser` is
not an `int` value; it's *a promise* of an `int`. Put your ear up to
it and you'll hear it whispering:

> True, I am not an actual value myself. But I promise I can provide you with one.  
> Just feed me with an input string: I will do my best to parse it
> and, if I don't fail, I will eventually give you back that
> much-desired `int` value.

To keep the metaphor going, if `twice <<| p42` could speak, it would
tell you:

> I see what you want to do! You want to multiply some integer 
> value by `2`.  
> You know that we don't have that integer value just yet; you also
> know we will eventually obtain one as soon as you feed with an input
> string.
>
> Let's make a deal: pass me the calculation you want to run. I will
> parse that input string for you, then I will apply that function. I
> promise.
>
> I mean: you gave me a Parser, I pay you back with
> another Parser. It's only fair.

This means that the signature of our enhanced operators should rather
be:


| Version        | Operator | Signature                              |
|----------------|----------|----------------------------------------|
| F# native      | `<|`     | `('a -> 'b) -> 'a -> 'b`               |
| Parser-powered | `<<|`    | `('a -> 'b) -> 'a Parser -> 'b Parser` |

and:

| Version        | Operator | Signature                              |
|----------------|----------|----------------------------------------|
| F# native      | `|>`     | `'a        -> ('a -> 'b) -> 'b`        |
| Parser-powered | `|>>`    | `'a Parser -> ('a -> 'b) -> 'b Parser` |


Since both `<<|` and `|>>` now return a `'b Parser`, the assertions:

```fsharp
test <@ twice <<| p42 = 84 @>
test <@ p42 |>> twice = 84 @>
```

will not compile. We cannot compare the returned `'b Parser` with
`84`. Is a sense, you got back a future, successfully parsed `84`
wrapped in a box. You need `run` to unwrap it:

```fsharp
test <@ run (twice <<| p42) "some input" = Success ("rest", 84) @>
test <@ run (p42 |>> twice) "some input" = Success ("rest", 84) @>
```

Putting all together:

```fsharp
let (<<|) (f: 'a -> 'b) (aP: 'a Parser) : 'b Parser = __

let (|>>) (aP: 'a Parser) (f: 'a -> 'b) : 'b Parser = __

let p42: int Parser =
    Parser (fun _ -> Success(42, "rest"))

[<Fact>]
let ``parser-powered function application`` () =
    let twice x = x * 2

    test <@ run (twice <<| p42) "some input" = Success(84, "rest") @>
    test <@ run (p42 |>> twice) "some input" = Success(84, "rest") @>
```

It compiles.  
Now that we are armed with a failing test, we are ready to implement
`|>>` and `<<|`. Not only can you be driven by tests: types can drive
you too. Listen to the signature:

```fsharp
let (<<|) (f: 'a -> 'b) (aP: 'a Parser) : 'b Parser = ...
```

It tells us to return an instance of `Parser`. Fine, let's obey. We
need to invoke the `Parser` case constructor:

```fsharp
let (<<|) (f: 'a -> 'b) (aP: 'a Parser) : 'b Parser =
    Parser ...
```

What does the case constructor want as an argument? A function from an
input `string` to something. Fine, type system, I trust you:

```fsharp
let (<<|) (f: 'a -> 'b) (aP: 'a Parser) : 'b Parser =
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
- So, just *run* `aP` with `input` and you will get an `a
  ParseResult`. If it is successful, you will find the `'a` value
  there, together with the unconsumed input.
- (If it fails, you are safe to let `<<|` fail too).
- With the obtained `'a` value, you can finally invoke `f`, and get a
  `'b` value.
- You can wrap the `'b` value and the unconsumed value in a tuple, and
  finally return it, successfully.

Let's translate all of this to code:

```fsharp
let (<<|) (f: 'a -> 'b) (aP: 'a Parser) : 'b Parser =
    Parser (fun input ->
        let ar : 'a ParseResult = run aP input
        match ar with
        | Success (a, rest) -> Success (f a, rest)
        | Failure s -> Failure s )
```

Relying on type inference, you can simplify this to:

```fsharp
let (<<|) f aP =
    Parser (fun input ->
        match run aP input with
        | Success (rest, a) -> Success (rest, f a)
        | Failure s -> Failure s )
        
let (<<|) f aP =
    Parser (fun input ->
        match run aP input with
        | Success (a, rest) -> Success (f a, rest)
        | Failure s -> Failure s )
```

Not bad! And not too difficult, after all. This function is
interesting because it captures the *effectful logic*:

- It passes the unconsumed input forward.
- It deals with failures.

It does not contain any logic other than this. It really keeps
concerns separate:

- `f` encapsulates the actual domain logic.
- `<<|` encapsulates the *glueing* logic.

As we said in [chapter 5](monadic-parser-combinators-5), it's a form
of structural decoupling, obtained with a tool other than the classic
dependency injection.


Defining `|>>` is trivial: just swap the parameters:

```fsharp
let (|>>) aP f = f <<| aP
```

Green tests. Hurray!  
In the next pages, we'll keep using `|>>`: similarly to `|>`, its form
strongly evokes the idea of sending an argument through a pipe to the
next function. As we'll see shortly, however, when it comes to `<<|`,
we will prefer a different interpretation, one that involves mapping a
function from the lower world to the higher realm of parsers. In this
case, we'll prefer the name `map` or the symbol `<!>`. More on this
later. First, let's see a couple of examples how you can use these
brand new operators.

## What An Epic Time To Live!
Let's cover this use case: you want to parse an input in a specific
way, but you can't get to the desired result all at once. Instead,
first, you parse the input into a value that is not exactly what you
want. Then, in each of the next steps, you apply a function to
transform that value to something else, possibly of a different type,
until you get the final type you had in mind. Of course, since we are
talking about parser combinators, you'd like to combine all these
steps into a single, composed parser.

For example, it is trivial to write a DateTime parser for the boring
format `yyyy-MM-dd hh:mm:ss` if only you cheat and you use the native `TryParse` method:


```fsharp
let dateTimeP: DateTime Parser =
    Parser (fun input ->
        match DateTime.TryParse(input[..18]) with
        | true, dateTime -> Success (dateTime, input[19..])
        | false, _ -> Failure "Expected a date")

[<Fact>]
let ``date test`` () =
    test <@ run dateTimeP "2025-01-01 18:11:12, the rest" = Success (DateTime(2025, 01, 01, 18, 11, 12), ", the rest") @>
```

But when it's time to use `DateTime` as an internal representation of
dates in your esoteric programming language, you reject the idea: it's
way too conventional. You once read about the [Epoch time][epoch-time]
which represents time by the number of seconds since the midnight of 1
January 1970. Why 1970, you wonder? Because Unix was created in the
early 1970s. What a lame excuse! You rather prefer a format that
celebrates yourself. What about using:

```fsharp
type EpicTime = EpicTime of double
```

measuring time as the number of second since where *you* were born?
After all, that was the most epic moment in the universe history.

(Let's say, it was on 18 February 2005, at 18:24:02, OK?)

Here's how to convert a depressing `DateTime` to a gorgeous
`EpicTime`:

```fsharp
type EpicTime = EpicTime of double

let toEpicTime (date: DateTime) =
    let aRemarkableDate = DateTime(2005, 02, 28, 18, 24, 02)
    let secondsFromTheEpicDate = date.Subtract(aRemarkableDate).TotalSeconds
    EpicTime secondsFromTheEpicDate

[<Fact>]
let ``Epic Time is so much better than Epoch Time`` () =
    test <@ DateTime(2025, 01, 01, 18, 11, 12) |> toEpicTime = EpicTime 626140030.0 @>
```

As a fan of your language, I see how `EpicTime 626140030.0` is a way
more convenient and intuitive representation than `01 January 2025
18:11:12`. Only, now we need an `EpicTime Parser`. Presto! You can
combine the `DateTime Parser` with `toEpicTime`, using `|>>`:

```fsharp
let epicTimeP : EpicTime Parser =
    dateTimeP |>> toEpicTime 

[<Fact>]
let ``epicTime test`` () =
    test <@ run epicTimeP "2025-01-01 18:11:12, the rest" = Success (EpicTime 626140030.0, ", the rest") @>
```

Look how concise:

```fsharp
let epicTimeP = dateTimeP |>> toEpicTime
```

It does not even mention a single function parameter: it a pure
combination of parsers and functions. This style is called Point Free
Style or [Tacit Programming][tacit-programming], and is typical when
playing with functional combinators. It's fundamentally the
consequence of manipulating things in an elevated, more abstract
context. We will get back to this later.


## Mapping strings to types
Let's see an other simple example based on the very same idea:
applying a function to the value eventually parsed by a parser or,
anticipating the lingo you will encounter, *mapping* functions to parsers.  
Say you want to parse keywords of your programming language. You can
proceed by steps. First, you start with a parsers that just checks if
the input contains specific keywords. In other words, a parser reading
an input string and succeeding if it contains a specific string. This
trivial parser would return the specific string, if it was found in
the input; otherwise, it would fail. Later, you can refine it to
return specific custom types, by the means of `|>>`.

To check the presence of a specific string, if you want to be generic,
you can define a parser factory, something that gets the string you
want to match and generates a parser for it:

```fsharp
let str (s: string) =
    Parser (fun input ->
        if input.StartsWith(s) then Success (s, input[s.Length..])
        else Failure $"Expected {s}" )

[<Fact>]
let ``test str`` () =
    test <@ run (str "foo") "foo-then something else" = Success ("foo", "-then something else") @>
    test <@ run (str "foo") "notfoo--then something else" = Failure "Expected foo" @>
```

Keep `str` in mind: we are going to use it over and over. You can use
it as a building block to parse more refined parsers. For example,
here's how to define booleans in your language.  With a stroke of
genius you take the decision to have three-state booleans:

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
let falschStr: string Parser = str "falsch"

[<Fact>]
let ``parsing the string 'falsch'`` () =

    test <@ run falschStr "falsch as a 3 dollar bill" = Success ("falsch", " as a 3 dollar bill") @>
    test <@ run falschStr "if(2+2=5 -> gelegentlich) then foo()" = Failure "Expected falsch" @>
```

We are not done yet. `falschParser` is a `String Parser`, not a
`Boolish Parser`. `|>>` to the resque!

```fsharp
let boolishFalscheP = falschStr |>> (fun _ -> SoFalse)

[<Fact>]
let ``parsing the string 'falsch as an instance of Boolish`` () =
    test <@ run boolishFalscheP "falsch as a 3 dollar bill" = Success (SoFalse, " as a 3 dollar bill") @>
```


## F-word
Do you realize know what you have just accomplished? You have invented
Functors! In order to understand this better, we need to examine the
`<<|` signature and to reinterpret it through a new lens. Here's the implementation:

```fsharp
let (<<|) (f: 'a -> 'b) (ap: 'a Parser) =
    Parser (fun input ->
        match run ap input with
        | Success (a, rest) -> Success (f a, rest)
        | Failure s -> Failure s )
```

and here the signature:

```fsharp
val (<<|) : ('a -> 'b) -> 'a Parser -> 'b Parser
```

Given that all the functions in F# are curried, there are 2 ways to
read it:

| Input                     | Output                   | Interpretation                             |
|---------------------------|--------------------------|--------------------------------------------|
| `('a -> 'b) -> 'a Parser` | `'b Parser`              | Apply a function to the result of a parser |
| `('a -> 'b)`              | `'a Parser -> 'b Parser` | Lift a function to the Parser world        |


You can easily apply the 1st interpretation to the `EpicTime`
case. You have a function from `DateTime` (the sad cat) to `EpicTime`
(the happy, punk cat):

```fsharp
val toEpicTime : DateTime -> EpicTime
```

and you wanted to apply it to *the value inside the* `DateTime Parser`
box (the glass can):

```fsharp
val dateTimeP: DateTime Parser
```

You can do this with:

```fsharp
let epicTimeP : EpicTime Parser =
    toEpicTime <<| dateTimeP
```

The types in the game are:

| Element      | Signature                                                        |
|--------------|------------------------------------------------------------------|
| `toEpicTime` | `DateTime -> EpicTime`                                           |
| `dateTimeP`  | `DateTime Parser`                                                |
| `<<|`        | (`DateTime -> EpicTime`) -> `DateTime Parser -> EpicTime Parser` |


You can imagine how `<<|`:

- Gets the glass can with the sad `DateTime` cat.
- Opens the can freeing the cat.
- Applies `toEpicTime` to the sad `DateTime` cat turning it into a
  happy, punk `EpicTime` cat.
- Then, secures the cat back in the glass can.


Generally: `<<|` lets you apply a function to "the content" of a
box. This box is called Functor and `<<|` is also called `map`. You
will often hear expressions such as "A Functor is something you can
map over".

The second &mdash; more fascinating and powerful &mdash;
interpretation arises from the second signature interpretation. Or as
soon as you partially apply `<<|`:

```fsharp
let toEpicTimeP = (<<|) toEpicTime
```

Wait, having an operator used in prefix fashion is a bit weird. Let's
define an alias before proceeding. We can call it either `map` or
`lift`, and the reason will be immediately clear:


```fsharp
let map = (<<|)

let toEpicTimeP = map toEpicTime
```

Read `map`'s signature using the 2nd interpretation, as:

```fsharp
val map: ('a -> 'b) -> ('a Parser -> 'b Parser)
```

`map` is that combinator that given a function `f : 'a -> 'b`
operating on ordinary values *lifts* it to work on parsers, as an `'a
Parser -> 'b Parser` function. It maps things from the lower world to
the elevated, Parser-powered universe.    

<p align="center"> <img
  src="static/img/parser-combinators-for-the-rest-of-us/map.png"
  alt="" height="350px"> </p>

Applied to our case:

- You have `toEpicTime: DateTime -> EpicTime`.
- You lift `toEpicTime` with `map`:

```fsharp
let epicTime = map toEpicTime
```

- It is transformed to `toEpicTimeP: Parser DateTime -> Parser
  EpicTime`.



<p align="center"> <img
  src="static/img/parser-combinators-for-the-rest-of-us/map-applied.png"
  alt="" height="350px"> </p>


When you can feed it with a `DateTime Parser`:

```fsharp
let epicTimeParser = toEpicTimeP dateTimeP
```

you will give you back an `EpicTime Parser`:

```fsharp
let toEpicTimeP = map toEpicTime

[<Fact>]
let ``applying a lift toEpicTime`` () =
    test <@ run (toEpicTimeP dateTimeP) "2025-01-01 18:11:12, the rest" = Success (EpicTime 626140030.0, ", the rest") @>
```


Wow... That was a mouthful, wasn't it? Not only are Functors
incredibly powerful, but they are also pervasive in Functional
Programming, forming a fundamental building block. During your journey, you will surely encounter them in many other contexts.

We've covered many details about transforming a single parser and adapting it to our needs, but we haven't yet looked at sequencing two parsers in a row.

It's time to take a break, enjoy a salmiakki, and then jump to
[chapter 8](monadic-parser-combinators-8) to explore this new topic.

[Previous - Mapping the Journey](/monadic-parser-combinators-6)
⁓ [Next - Here Comes The Tuple](/monadic-parser-combinators-8)


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

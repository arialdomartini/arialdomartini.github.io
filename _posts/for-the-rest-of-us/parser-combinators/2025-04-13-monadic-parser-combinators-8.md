---
layout: post
title: "Monadic Parser Combinators in F# - Here Comes The Tuple"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
include_in_index: false
---
Very well. We have a powerful tool at our disposal for applying
functions to the content of a parser. The biggest limitation is that
it works with a single parser only. In this installment we will
distill one of the several possible approaches for combining 2 parsers
together.


## Variable Assignment

Let's develop another one of the brilliant syntax costructs of your
language. Torn between using `=` or `:=` for variable assignment, you
resolve the dilemma by not using any symbol whatsoever. A simple:

```
42foo
```

will assign the `int` value `42` to the variable `foo`. Terrific.

Internally, you want to store the parsed value with the type:

```fsharp
type VariableName = VariableName of string

type Assignment<'a> =
  { VariableName: VariableName
    Value: 'a }

type IntVariable = IntVariable of int Assignment
```

Say you managed to solve both the problem of parsing an `int` and the
problem of parsing a `VariableName`: how to build an `IntVariable
Parser`? You need something that runs a `int Parser`, then a
`VariableName Parser` and finally combines them into an `IntVariable Parser`.

An idea could be to break this problem in 2 steps:

- first, you combine the `int Parser` and the `VariableName Parser` into a `Parser` for the tuple `(int * VariableName)`.
- Then, you use `map` / `<<|` to open the glass jar and to convert the
  tuple into an actual `IntVariable` instance.
  
Indeed, if you have an `(int * VariableName) Parser`, building an
`IntVariable Parser` is straightforward, once you have a function from
`(int * VariableName) -> IntVariable`:

```fsharp
[<Fact>]
let ``from tuple to IntVariable`` () =
    let tupleP: (int * VariableName) Parser =
        Parser(fun _ -> Success("rest", (42, VariableName "foo")))

    let buildIntVariable: int * VariableName -> IntVariable =
        (fun (value, name) -> 
            IntVariable { VariableName = name; Value = value })

    let intVariableP: IntVariable Parser =
        tupleP |>> buildIntVariable

    let expected: IntVariable =
        IntVariable { VariableName = VariableName "foo"; Value = 42 }

    test <@ run intVariableP "whatever" = Success("rest", expected) @>
```
  
Yes, our old friend `map` / `|>>` is enough for this.  
This bears the question: how to combine 2 parsers into a tuple?

## Let there be tuples

OK, let's design this parser starting from the desired signature. We
want to be as much generic as possible, therefore we assume that we
start from an `'a Parser` and a `b Parser`:

```fsharp
var andThen : 'a Parser -> 'b Parser -> ???
```

We know that parsers are promises, not concrete values. So, `andThen`
cannot help but returning another parser. By the way, we use tuples
because in the general case `a` and `b` are different types: putting
them together in a list or an array is just not possible. We could
also use a class or a record, but tuples are simpler. Let's go with
them.

```fsharp
let andThen<'a, 'b>: 'a Parser -> 'b Parser -> ('a * 'b) Parser = failwith "Not yet implemented"
```

The conventional operator symbol for `andThen` is `.>>.`:

```fsharp
let (.>>.) = andThen
```

Let's have a test for guiding the implementation:

```fsharp
[<Fact>]
let ``parse the assignment of an int variable`` () =
    let intP : int Parser = 
        Parser (fun input -> Success (input[2..], 42))
    
    let variableNameP : string Parser =
        Parser (fun input -> Success (input[3..], "foo"))

    let tupleP = intP .>>. variableNameP
    
    test <@ run tupleP "42foo bla bla bla" = Success (" bla bla bla", (42, "foo")) @>
```

Of course, in the test we don't care how `intP` and `variableNameP`
work, so it's fine to give them a dummy implementation.

As for the implementation of `andThen`:

```fsharp
let andThen<'a, 'b>: 'a Parser -> 'b Parser -> ('a * 'b) Parser = 
```

as usual we can let types drive us. We know we have to return a
`Parser`. So, let's build one:


```fsharp
let andThen<'a, 'b>: 'a Parser -> 'b Parser -> ('a * 'b) Parser = 
    Parser ...
```

The Case Constructor wants a function from `input: string`. Let's go:


```fsharp
let andThen (aP: 'a Parser) (bP: 'b Parser): ('a * 'b) Parser = 
    Parser (fun input -> 
        ...)
```

OK. In case of success, we have to return a tuple `(valueA, valueB)`,
together with the unconsumed input. How to obtain `valueA`? We have an
`'a Parser`, we have an input. That's easy, with `run`:


```fsharp
let andThen<'a, 'b> (aP: 'a Parser) (bP: 'b Parser): ('a * 'b) Parser =
    Parser (fun input ->
        let resultA = run aP input
        ...
```

You know that a parser can. It's fair to assume that if `aP` fails,
the whole `andThen` must also fail:


```fsharp
let andThen<'a, 'b> (aP: 'a Parser) (bP: 'b Parser): ('a * 'b) Parser =
    Parser (fun input ->
        let resultA = run aP input
        match resultA with
        | Failure f -> Failure f
        ...
```

If `aP` succeeds, it returns the unconsumed input `restA` and a parsed
value `valueA`, the first part of the tuple you want to return:

```fsharp
let andThen<'a, 'b> (aP: 'a Parser) (bP: 'b Parser): ('a * 'b) Parser =
    Parser (fun input ->
        let resultA = run aP input
        match resultA with
        | Failure f -> Failure f
        | Success (restA, valueA) ->
            ...
```

We are almost done. With `restA` it's easy to also run `bP`:


```fsharp
let andThen<'a, 'b> (aP: 'a Parser) (bP: 'b Parser): ('a * 'b) Parser =
    Parser (fun input ->
        let resultA = run aP input
        match resultA with
        | Failure f -> Failure f
        | Success (restA, valueA) ->
            let resultB = run bP restA
            ...
```

Same story here: should `bP` fail, we let `andThen` fail; otherwise,
we successfully return the tuple:


```fsharp
let andThen<'a, 'b> (aP: 'a Parser) (bP: 'b Parser): ('a * 'b) Parser =
    Parser (fun input ->
        let resultA = run aP input
        match resultA with
        | Failure f -> Failure f
        | Success (restA, valueA) ->
            let resultB = run bP restA
            match resultB with
            | Failure f -> Failure f
            | Success (restB, valueB) -> Success (restB, (valueA, valueB)))
```

You can make it slightly shorter like this:


```fsharp
let andThen<'a, 'b> (aP: 'a Parser) (bP: 'b Parser): ('a * 'b) Parser =
    Parser (fun input ->
        match run aP input with
        | Failure f -> Failure f
        | Success (restA, valueA) ->
            match run bP restA with
            | Failure f -> Failure f
            | Success (restB, valueB) -> Success (restB, (valueA, valueB)))
```

Putting `.>>.` and `|>>` togehter you get:


```fsharp
let intAssignment =
    intP .>>. variableNameP
    |>> toIntVariable
```

with its test:


```fsharp
[<Fact>]
let ``parse the assignment of an int variable`` () =
    let intP = Parser(fun input -> Success(input[2..], 42))

    let variableNameP =
        Parser(fun input -> Success(input[3..], VariableName "foo"))

    let toIntVariable (value, name) =
        IntVariable { VariableName = name; Value = value }


    let intAssignment =
        intP .>>. variableNameP
        |>> toIntVariable


    let expected: IntVariable =
        IntVariable
            { VariableName = VariableName "foo"
              Value = 42 }

    test <@ run intAssignment "42foo bla bla bla" = Success(" bla bla bla", expected) @>
```

Nice. You did it.

In the next chapters you will learn how to reduce `andThen` / `.>>.`
to the (way more readable):

```fsharp
let andThen aP bP =
    parser {
        let! a = aP
        let! b = bP
        return (a, b) }
```

or to the super short:

```fsharp
let andThen = lift2 (fun a b -> (a, b))
```

which will teach you how to understand the mindblowingly short Haskell
version:

```haskell
let andThen = liftA2 (,)
```

But be patient, we will get there.


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

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
distill one of the simplest approaches for combining 2 parsers
together.


## Variable Assignment

Let's develop another syntax costruct of your stunning language. Torn
between using either `=` or `:=` for variable assignment, you resolve
the dilemma by not using any symbol whatsoever. A simple:

```
42foo
```

will assign the `int` value `42` to the variable `foo`. Terrific.  
Internally, you want to store the parsed value in an instance of type `IntVariable`:

```fsharp
type VariableName = VariableName of string

type Assignment =
  { variableName: VariableName
    value: int }
```

If you had a parser returning an `(int * VariableName)` tuple,
building an `Assignment` parser would be a breeze, with the help of
your old friend `map`:

```fsharp
// Test parser, magically returning (42, VariableName "foo")
let tupleP: (int * VariableName) Parser =
    Parser(fun _ -> Success((42, VariableName "foo"), "rest"))

let assignmentP: Assignment Parser =
    tupleP 
    |>> (fun (value, name) -> { variableName = name; value = value })

[<Fact>]
let ``from tuple to Assignment`` () =

    let expected: Assignment =
        { variableName = VariableName "foo"
          value = 42 }

    test <@ run assignmentP "42foo" = Success(expected, "rest") @>
```

How to build a real `tupleP`, though, is a whole different story. Even
if you already had both an `int` parser and a `VariableName` parser,
how to compose them to produce an `(int * VariableName)` parser is not
immediately evident. The reason why this exercise is worth to be done
is because its generalization leads to the discovery of the next
important building block in our functional programming journey:
the Applicative Functor. Let's see.

## Let There Be Tuples
We can start designing this combinator from the desired signature. We
want it to be as generic as possible, therefore we assume that we
start from an `'a Parser` and a `'b Parser`. Since there is a notion
of parsing elements in a sequence, we will call it `andThen`:

```fsharp
let andThen<'a, 'b>: 'a Parser -> 'b Parser -> ('a * 'b) Parser = __
```

By the way, we use a tuple as the return value because in the most
general case `'a` and `'b` are different types: putting non homogeneus
value in a list or an array is just not possible (F# is not Ruby). We
could also use a class or a record, but tuples are simpler. Let's go
with them.

The conventional operator symbol for `andThen` is `.>>.`:

```fsharp
let (.>>.) = andThen
```

Let's have a test for guiding the implementation:

```fsharp
type VariableName = VariableName of string

type Assignment =
    { variableName: VariableName
      value: int }

[<Fact>]
let ``combine 2 parsers generating a parser of tuples`` () =
    let intP : int Parser = 
        Parser (fun input -> Success (42, input[2..]))
    
    let variableNameP : VariableName Parser = str "foo" |>> VariableName

    let tupleP = intP .>>. variableNameP
    
    test <@ run tupleP "42foo the rest" = 
        Success ((42, VariableName "foo"), " the rest") @>
```

Of course, in the test we don't care how `intP` and `variableNameP`
work, so it's fine to give them a dummy, hardcoded implementation. As
for the implementation of `andThen`, as usual we can let types drive
us. We know we have to return a `Parser`. So, let's build one:


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

It's fair to assume that if `aP` fails, the whole `andThen` must also
fail:


```fsharp
let andThen<'a, 'b> (aP: 'a Parser) (bP: 'b Parser): ('a * 'b) Parser =
    Parser (fun input ->
        let resultA = run aP input
        match resultA with
        | Failure f -> Failure f
        ...
```

If `aP` succeeds, it returns the parsed value `valueA` (the first part
of the tuple you want to return) plus the unconsumed input `restA`, :

```fsharp
let andThen<'a, 'b> (aP: 'a Parser) (bP: 'b Parser): ('a * 'b) Parser =
    Parser (fun input ->
        let resultA = run aP input
        match resultA with
        | Failure f -> Failure f
        | Success (valueA, restA) ->
            ...
```

We are almost done. With `restA` it's easy to also run the second
parser `bP`:


```fsharp
let andThen<'a, 'b> (aP: 'a Parser) (bP: 'b Parser): ('a * 'b) Parser =
    Parser (fun input ->
        let resultA = run aP input
        match resultA with
        | Failure f -> Failure f
        | Success (valueA, restA) ->
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
        | Success (valueA, restA) ->
            let resultB = run bP restA
            match resultB with
            | Failure f -> Failure f
            | Success (valueB, restB) -> Success ((valueA, valueB), restB))
```

You can make the whole expression slightly shorter like this:


```fsharp
let andThen<'a, 'b> (aP: 'a Parser) (bP: 'b Parser) : ('a * 'b) Parser =
    Parser(fun input ->
        match run aP input with
        | Failure f -> Failure f
        | Success(valueA, restA) ->
            match run bP restA with
            | Failure f -> Failure f
            | Success(valueB, restB) -> Success((valueA, valueB), restB))
```

You are done! Keep `.>>.` in your tool belt, it will come in easy very
often.  
Armed with `andThen` / `.>>.` and `|>>`, you can finally build the
`Assignment` parser:


```fsharp
let intP: int Parser = Parser(fun input -> Success(42, input[2..]))

let variableNameP: VariableName Parser = str "foo" |>> VariableName

let assignmentP =
    intP .>>. variableNameP
    |>> (fun (i,v) -> { variableName = v; value = i })


[<Fact>]
let ``combine 2 parsers generating a parser of tuples`` () =
    
    let expected = {variableName = VariableName "foo"; value = 42}
    test <@ run assignmentP "42foo the rest" = Success(expected, " the rest") @>
```

Nice! You did it!

## Umpf
Can I say something? This syntax:

```fsharp
let assignmentP =
    intP .>>. variableNameP
    |>> (fun (i,v) -> { variableName = v; value = i })
```

just sucks. I swear that there are occasions where `.>>.` shines. I
also swear that you will eventually get used to such succint, operator
dense expressions. However, I am sure that you are happy to know that
in the next chapters you will learn how to write `andThen` / `.>>.`
using a completely different syntax:

```fsharp
let andThen aP bP =
    parser {
        let! a = aP
        let! b = bP
        return (a, b) }
```

Isn't it just easier to interpret? Funny enough, you will also learn
to write it in a more concise way like this:

```fsharp
let andThen = lift2 (fun a b -> (a, b))
```

which will lead you to understand the mindblowingly short Haskell
version:

```haskell
let andThen = liftA2 (,)
```

But be patient, we will get there. I guess you can reward yourself
with a slice of castagnaccio and then move to [Chapter
9](/monadic-parser-combinators-9), where we will play with the idea of
ignoring parsers. Buon appetito!

[Previous - Parser-Powered Function Application](/monadic-parser-combinators-7)
⁓ [Next - Things You Don't Care About](/monadic-parser-combinators-9)


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

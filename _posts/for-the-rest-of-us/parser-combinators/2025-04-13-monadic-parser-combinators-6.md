---
layout: post
title: "Monadic Parser Combinators in F# - Bird's-Eye View Of What You Will Build"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
include_in_index: false
---
    
The gist of the previous chapter is: we should factor the structural
dependencies away from our code.

This will let us write and combine parsers focusing on the essence of
parsing, ignoring the uninteresting mechanic of passing `rest`
around and of pattern-matching errors. If we find a way to abstract
over these aspects, we could think of having:

1. Easy-to-use functions for combining generic parsers.
2. Operators for building parsing-combining expressions.
3. A special syntax for manipulating parsers with imperative code,
  with the same ease of manipulating values.
4. Lifting functions and operators, to project ordinary functions into
  the Parser world.

Let me give you some examples for each of those 4 categories.

## Functions for combining generic parsers

Imagine that, building your language, you already managed to parse
predicates like `(a or b) and not callme()` with a parser called
`predicate`, and arbitrary code blocks with a parser called
`codeBlock`. You want to have a parser able to build an internal
representation of an `if` statement:

```fsharp
type If =
    { Predicate: Predicate
      CodeBlock: CodeBlock }
```

from a source code like:

```csharp
IF -> <<whatever_predicate>>
  whatever_code_block
```

(I'm amazed by the beauty of this syntax: definetely, I'm a fan of your language).

You will be able to build the new parser combining the 2 existing
`predicate` and `codeBlock` with something like:

```fsharp
let parseIf =
    predicate 
    |> between (str "IF -> <<") (str ">>")
    |> andThen codeBlock
    |>> fun (predicate, codeBlock) -> 
          { Predicate = predicate
            CodeBlock = codeBlock }
```

Read it as:

- build a parser that
- given any predicate
- that is surrounded by `IF -> <<` and `>>`
- and followed by any code block
- returns an instance of the custom type `If`, containing the parsed
  predicate and the parsed code block.


I skated over parsing the empty spaces and the indentation, but I hope
you get the idea: you can use combinators like `between` and `andThen`
to *describe* your syntax and to get back a new parser, without being
distracted by the unconsumed input and the error handling. In
particular, notice the arguments we are passing these `between` and
`andThen` functions: they are by themselves parsers. For example: `str
">>"` is not the string `">>"`, but a parser that succeeds when the
input contains `>>`.  
You are actually building a new parser combining other parsers in a
descriptive way.

## Operators for building parsing-combining expressions

Given 3 parsers:

- one able to detect an opening tag
- one able to detect a closing tag
- one that parses something (you don't care what)

then you can think of transforming a "parser of something" into a
"parser of something surrounded by tags", with:

```fsharp
let surroundedBy before after parser = before >>. parser .>> after
```

Read `a >>. b` as:

- give me a parser that
- expects whatever the parser `a` expects
- then continues parsing whatever `b` is good at parsing
- and, finally, returns only the thing parsed by `b`, dropping the
result of `a`.

Don't stress too much yourself understanding `>>.` and `.>>` just yet.
We will build several other little operators like these, so you'll
have plenty of time to grasp them. For now, just get the idea: you
will end up enriching F# with a bunch of new little grammatical
constructs and syntactic elements, to make your parsing language more
expressive.


## Special syntax for writing imperative code

We would like to manipulate parsers with the same ease of manipulating
values. For example, for parsing:

```
'john',2025-01-02,42
```

as a:

```fsharp
(string * DateTime * int)
```

tuple, you might write:
  
  
```fsharp
let tuple : (string * DateTime * int) Parser = 
    parser {
        let! s = singleQuotedString
        let! _ = comma
        let! d = date
        let! _ = comma
        let! i = number
        return (s, d, i) }

[<Fact>]
let ``parses a tuple`` () =
    let input = "'john',2025-01-02,42"

    test <@ run tuple input = ("john", DateTime(2025,01,02), 42)@>
```

Read it as:

- `tuple` is a parser (see that `parser {`)
- that builds a `(string * DateTime * int)` from
- a single quoted string
- a date
- and a number
- separate by commas.

Squinting your eyes, you can think that a sequence of `let!`
statements like:

```fsharp
    let! s = singleQuotedString
    let! d = date
    let! i = number
```

is a way to assign to variables values magically being distilledfrom
the input.

In reality, what you see on the right side of a `let!` is not a parsed
value, but a parser. The special syntax `let!` runs the parser on the
right, saves its result in the variable on the left and then continues
parsing the rest, doing all the magic about passing `rest` and
pattern matching the `Result`. Also in this case: we will build this
syntax by hand, from the ground up, so don't worry if you cannot wrap
your head around it just yet.


## Lifting functions and operators
Consider the case of parsing an arithmetic expression:

```
42+79
```

In the AST of your language, this should be represented as an instance
of `Expression`:

```fsharp
type Operation = Sum | Sub | Mul | Div

type Expression = Expression of int * int * Operation

let ``parses a sum`` () =
    let input = "42+49"
    
    test <@ run parseExp input = Expression(42, 79, Sum) @>
```

Well, if you had the 2 int values `42` and `79` and the value of
`Sum`, building an instance of `Expression` would be a matter of
defining:

```fsharp
let buildExpression (a: int) (op: Operation) (b: int) = 
    Expression (a, b, op)
```

and of invoking it:

```fsharp
let expression = buildExpression 42 79 Sum
```

Sure, that's trivial. But, first: you don't want an expression; you
want a parser of expressions. Also, you don't have those values. You
have *parsers* of those values. You cannot feed `buildExpression` with
parsers: it wants *the result of parsing*.

Fear not! You can lift `buildExpression` into the magic world of
parsers, so that it becomes a `buildExpressionOnSteroids`. It will
happily accepts parsers of values rather than values:

```fsharp
let buildExpressionOnSteroids = lift3 buildExpression
```

Believe me or not, while `buildExpression` signature was:

```fsharp
val buildExpression : int -> Operation -> int -> Expression
```

by the application of `lift3` the `buildExpressionOnSteoids`'
signature turned into:

```fsharp
val buildExpressionOnSteoids : int Parser -> Operation Parser-> int
Parser -> Expression Parser
```

It's a parser! And it passes the test. Unbelievable.  
Think about it: from a humble, ordinary factory function that *builds
something* you managed to create a function that *parses that
something*. Diabolic.


Or look this. As the Benevolent Dictator For Life of your language,
you proclaim that the syntax:

```
7 times date{16/03/1953}
```

builds a list of `7` dates (all the same), boxed inside a `Foo`
object. Sounds a very useful construct, doesn't it?

```fsharp
type Foo = Foo of (DateOnly list)

[<Fact>]
let ``parses a Foo`` () =
  let input = "7 times date{16/03/1953}"
  
  let date = DateOnly(1953, 03, 16)
  test <@ run fooParser input = 
         Success ("", Foo [date; date; date; date; date; date; date]) @>
```

(By the way: I'm sold. Yours is, hands down, the most beautiful
language I have ever seen).

Instead of writing this `fooParser`, imagine to split the input `7 times date{16/03/1953}` into its syntactical components:

- `7`: the number.
- ` `: a space
- `times`: one of your language's commands.
- ` `: a space
- `DateOnly(1953, 03, 16)`: the date.

For now, just ignore the problem of *obtaining* those values. Assuming
you have them already, how would you build a `Foo`? Easy peasy:

```fsharp
let makeFoo (n: int) (space: char) (command: string) (space2: char) (date: DateOnly) : Foo =
    let dates = [ for i in 0 .. n - 1 -> date ]
    Foo dates
```

That's trivial (by the way, notice how `space` and `space2` are
ignored). The problem is: you don't have the values `n`, `space`,
`command` and `date`. Instead, you have *a parser* for each of them:

```fsharp
let nP:        int Parser      = intParser
let spaceP:    char Parser     = charParser ' '
let commandP:  string Parser   = str "times"
let dateP:     DateOnly Parser = parseDateOnly
```

(we don't actually mind how they work).

Can you feed `makeFoo` with parsers instead of actual values?

```fsharp
let foo : Foo = 
    makeFoo nP spaceP commandP spaceP dateP
```

Of course you can't! This won't even compile!  
What if instead of the F# function application you use a specialized
*parser-aware function application*?

```fsharp
let fooParser: Foo Parser =
    makeFoo <!> nP <*> spaceP <*> commandP <*> spaceP <*> dateP
```

What the heck? It works!!! This funny syntax gives you back is *a
parser* for `Foo`. How can it be? There must be some black magic
involved!



If all of this sounds confusing, that's perfectly fine. I hope it
sounds also a bit exciting.  
What you see above involves a fair bit of syntactic sugar, and a good
amount of behind-the-scenes magic. As with any magic trick, true
understanding comes from peeking behind the curtain and rebuilding it
from scratch. That's exactly what we are doing in [the next
chapter](monadic-parser-combinators-7).

Enough with reading code: let's finally hit some keys!


[Previous - A Different Kind of Coupling](/monadic-parser-combinators-5)
⁓ [Next - Parser-Powered Function Application!](/monadic-parser-combinators-7)


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

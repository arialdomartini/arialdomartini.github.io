---
layout: post
title: "Monadic Parser Combinators in F# - bird's-eye view of what you will build"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
- property-based testing
ftrou: true
include_in_index: false
---
The gist of the previous chapter is: we should factor the structural
dependencies away from our code.

This will let us write and combine parsers focusing on the essence of
parsing, ignoring the uninteresting mechanic of passing `rest` around
and of pattern-matching errors. If we find a way to abstract over
these aspects, we could think of having:

- Easy-to-use functions for combining generic parsers.
- Operators for building parsing-combining expressions.
- A special syntax for manipulating parsers with imperative code, with
  the same ease of manipulating values.
- Lifting functions and operators, to project ordinary functions into
  the Parser world.

Let me give you some examples.

## Functions for combining generic parsers

Imagine that for your language you already managed to parse predicates
like `(a or b) and not callme()` with a parser called `predicate`, and
arbitrary code blocks with a parser called `codeBlock`. You want to
build a parser able to build an internal representation of an `if`
statement:

```fsharp
type If =
    { Predicate: Predicate
      CodeBlock: CodeBlock }
```

from a string like:

```csharp
IF -> <<whatever_predicate>>
  whatever_code_block
```

(I'm amazed by the beauty of this syntax: definetely, I'm a fan of your language).

You like to build the new parser combining the 2 existing `predicate`
and `codeBlock` with something like:

```fsharp
let parseIf:
    predicate 
    |> between (str 'IF -> <<') (str '>>')
    |> andThen codeBlock
    |>> fun (predicate, codeBlock) -> 
          { Predicate = predicate
            CodeBlock = codeBlock }
```

Read it as:

- build a parser that given any predicate
- that is surrounded by `IF -> <<` and `>>`
- and followed by any code block
- returns an instance of the custom type `If`, containing the parsed
  predicate and the parsed code block.


Yes, there are slightly more details for ignoring the empty spaces and
indentation, but I hope you get the idea: you could use combinators
like `between` and `andThen` to *describe* your syntax and get back a
new parser, without being distracted by the unconsumed input and the
error handling. In particular, notice the arguments we are passing
these functions: they are by themselves parsers. We are actually
building a new parser combining other parsers in a descriptive way.

## Operators for building parsing-combining expressions

Given 3 parsers:

- one able to detect an opening tag
- one able to detect a closing tag
- one that parses something (you don't care what)

then you can think of transforming a parser of "something" into a
parser of "something surrounded by tags", with:

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

We would like to manipulate parsers with the same ease of manipulating values. For example:
  
  
```fsharp
let tuple : (string * DateTime * int) Parser = parse {
    let! s = singleQuotedString
    let! _ = comma
    let! d = date
    let! _ = comma
    let! i = number
    return (s, d, i)
}

[<Fact>]
let ``parses a tuple`` () =
    test <@ run tuple "'john',2025-01-02,42" = ("john",
    DateTime(2025,01,02), 42)@>
```

Read it as:

- `tuple` is a parser that builds a `(string * DateTime * int)` from
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

is assigning values to variables, magically taking those values from
the input.

In reality, what you see on the right side of a `let!` is a parser,
not a value. The special syntax `let!` runs the parser on the right,
saves its result in a variable and then continues parsing the rest.


## Lifting functions and operators
Consider again the case for the funny `if` statement. You want to
build an instance of:

```fsharp
type If =
    { Predicate: Predicate
      CodeBlock: CodeBlock }
```

Well, if you had an actual predicate and an actual code block,
building an instance of `If` would be a matter of defining:

```fsharp
let buildIf (predicate: Predicate) (codeBlock: CodeBlock) = 
    { Predicate = predicate
      CodeBlock = codeBlock}
```

and of invoking it passing it a predicate and a code block. Just that
easy.  
Sure: but you want to build a parser. You don't actually have a
predicate and a code block! All that you have is *a parser of
predicates* and *a parser of code blocks*. And you cannot feed
`buildIf` with parsers: it wants *the result of parsing*.

Fear not! You can lift `buildIf` into the magic world of parsers, so
that it becomes a `buildIf` on steroids. It willlq happily accepts parsers
of values rather than values:

```fsharp
let buildIfOnSteroids = lift2 buildIf
```

Believe me or not, while `buildIf` signature was:

```fsharp
val buildIf : Predicate -> CodeBlock -> If
```

`buildIfOnSteroids`' signature is:

```fsharp
val buildIfOnSteroids : Predicate Parser -> CodeBlock Parser -> If
Parser
```

This about it: from a humble, ordinary factory function that *builds
something* you managed to create a function that *parses that
something*. And all of this just applying `lift2`. There must be some
black magic involved.

Or look this, imagine this use case. For some crazy reason, in your
language:

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
         Foo [date; date; date; date; date; date; date]
```

(I'm sold: yours is definitely the most beautiful language I have ever
seen).

Instead of writing this `fooParser`, imagine to split the input `7
times 2025-09-18` into its syntactical components:

- `7`: the number.
- ` `: a space
- `times`: one of your language's commands.
- ` `: a space
- `2025-09-18`: the date.

For now, just ignore the problem of *obtaining* those values. Assuming
you have them, how would you build a `Foo`? Easy peasy:

```fsharp
let makeFoo (n: int) (space: char) (command: string) (space2: char) (d: DateOnly) : Foo =
    let dates = [ for i in 0 .. n - 1 -> d ]
    Foo dates
```

Wonderful. Here's a builder of `Foo`s. The problem is: you don't have
`n`, `command` and `d`. Instead, you have *a parser* for each of them:

```fsharp
let nP:             int Parser     = intParser
let spaceP:         char Parser    = charParser ' '
let commandP:       string Parser  = str " times "
let dateP:          DateOnly Parer = parseDateOnly
```

(we don't actually mind how they work).

Fine. But can you feed `makeFoo` with parsers instead of actual
values?

```fsharp
let foo = makeFoo nP spaceP commandP spaceP dateP
```

Of course you can't! This won't even compile!  
Wait for it: what if you use instead a specialized *parser-aware
function application*?

```fsharp
let fooParser: Foo Parser =
    makeFoo <!> nP <*> spaceP <*> commandP <*> spaceP <*> dateP
```

What the!? This funny syntax gives you back is *a parser* for `Foo`.
How can it be?



If all of this sounds confusing, that's perfectly fine. What you see
above involves a fair bit of syntactic sugar, and a good amount of
behind-the-scenes magic. As with any magic trick, true understanding
comes from peeking behind the curtain and rebuilding it from scratch.
That's exactly what we are doing in [the next
chapter](/monadic-parser-combinators-7).

Enough with reading code: let's finally hit some keys!

# References


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

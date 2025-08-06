---
layout: post
title: "Monadic Parser Combinators in F# - Mapping the Journey"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
include_in_index: false
---
I sometimes feel uneasy when books keep providing too many details
without giving a hint of where things are headed. This chapter takes a
more relaxed approach, setting aside implementation details to offer
you an early overview of what to expect. I hope it helps you getting
oriented as you read the next chapters.

The gist of the previous few pages is: we should factor structural
dependencies away from our code. This will allow us to create and
combine parsers while focusing on the essence of parsing, without
getting bogged down by the uninsteresting mechanical details of
passing `rest` and of pattern-matching errors. Once we understand how
to abstract these concerns, we can set our sights on developing the
following tools:

1. Some easy-to-use functions for combining generic parsers.
2. Some operators for building more fluent parsing-combining
   expressions.
3. A special syntax for manipulating parsers in imperative style
   &mdash; of course, while still being purely functional.
4. Lifting functions, to project ordinary functions into the Parser
  world.
5. Parser-powered function application

Let me show you some examples for each of those 4 categories.

## Functions For Combining Generic Parsers
Your language needs a switch statement. You get a burst of inspiration
and come up with this:

```csharp
SWITCH
/condition/ ~~ <block>
/condition/ ~~ <block>
/condition/ ~~ <block>
...
```

The beauty of this syntax! I'm such a fan of your language! You wish
to parse this as an instance of `Switch`:

```fsharp
type SwitchBranch = { Condition : Condition; Block : Block }
type Switch = { Branches : SwitchBranch list }
```

Imagine that you already managed to have a parser for `predicate` and
one for `block`. Presto! Here is `switchParser`:

```fsharp
let branch =
    tuple3
        (condition |> between (pchar '/') (pchar '/'))
        (pstring " ~~ ")
        (block |> between (pchar '<') (pchar '>'))
    |> map (fun (cond, _, block) -> { Condition = cond; Block = block })

let branches =
    branch |> sepBy (pchar '\n')

let switchParser =
    tuple3
        (pstring "SWITCH")
        (pchar '\n')
        branches
    |> map (fun (_, _, branches) -> { Branches = branches })
```

Read it from the bottom:

- the `switchParser` parses 3 elements in a sequence (`tuple3`):
  - the initial string `SWITCH`
  - a newline
  - and then all the branches (the `branches` parser).
- In turn, the branches section is:
  - a repetition of branch elements (`branch`)
  - separated by newlines `sepBy (pchar '\n')`
- And, finally, what's the syntax of a branch? It's 3 elements:
  - a condition, surronded by `/`
  - a lovely `~~`
  - and a block, between `<` and `>`

I hope you get the idea: you can use combinators like `between` and
`sepBy` to *describe* your syntax and to build parsers without being
distracted by the unconsumed input and the error handling. You can see
this as an internal Domain Specific Language that tries to be more
descriptive than imperative.

Also, note that the arguments we feed `between` and `sepBy` with are
parsers themselves. The outputs of `between` and `sepBy` are also
parsers, which are then fed into `branch`, producing yet another
parser. This parser is subsequently passed to `branches`, which
multiplies it and generates a new parser. Finally, all of this
culminates in `switchParser`, the outermost parser. [Satoshi
Kon][paprika] would be surely delighted by this recursive dreamscape,
where each parser unfolds into another parser, like a never ending
spiral of dreams nested within dreams.

## Operators For Building Parsing-Combining Expressions
Sometimes code is more expressive when infix operators are used. The
syntax of F# is often regarded as a notable example, because it allows
you to write expressions in a way that closely resembles natural
language. Instead of a series of nested function calls like:

```fsharp
let res =
    saveAudit "user_flow" (
        sendWelcomeIfNew "welcome_template" (
            updateLastLogin true (
                fetchProfile "basic" (
                    getUser 42))))

```

one may prefer a cascade of calls connected with by the pipe operator
`|>`:

```fsharp
let res =
    getUser 42
    |> fetchProfile "basic"
    |> updateLastLogin true
    |> sendWelcomeIfNew "welcome_template"
    |> saveAudit "user_flow"
```

Since F# supports custom operators (C#, why, why don’t you?) it is
only logic that you will want some convenient infix operators for
manipulating parsers.

Here's an example. You want a combinator to transform a *parser of
something* into a *parser of something surrounded by tags*. It would
take 3 parameters:

- A parser able to detect an opening tag.
- A parser for the closing tag.
- The parser you want to enrich.

Here's an implementation:

```fsharp
let between before after parser = 
    before >>. parser .>> after
```

Besides the internal implementation of those `>>.` and `.>>` &mdash;
which we will see in the next pages &mdash; you can think to them as
pipe operators similar to the familiar `|>`: they connect the left
parser with the right parser. See the `.` on one side of them? It
indicates which parser you want to obtain the result from; the other
parser will be executed, but then its result will be ignored.

So, an expression like `a >>. b` can be read as:

- give me a parser that
- expects whatever the parser `a` expects
- then continues parsing whatever `b` is good at parsing
- and, finally, returns only the thing parsed by `b`, dropping the
result of `a`.

We will build several other little operators, like `|>>`, `>>=` and
`<|>`. You'll have plenty of time to grasp them. For now, just get the
idea: you will end up enriching F# with a bunch of new little
grammatical constructs and syntactic elements, to make your parsing
language more expressive.


## Special Syntax For Writing Imperative Code
Sometimes infix operators are beautiful. Sometimes the dense syntax
they produce is too much for our brain to crunch, and we prefer a more
familiar, imperative style. Wouldn't be amazing if F# let you write
imperative-like code, while making sure it's still functional?  Enter
do-notation, or computation expressions. Here is how the `between`
combinator we defined before can be written with this style:

```fsharp
let between parser openTag closeTag =
    parse {
        let! _ = openTag
        let content = parser
        let! _ = closeTag
        
        return content
    }
```

- See the `parse {` in the second line? It makes it clear you are
building a parser.
- Each line runs a parser and stores the resulting parsed value in
  variable, for future use.
- Notice how they use a special parser-powered `let!` keyword.
- It is apparent which values are being ignored and which one is
  returned.
  
Despite the syntax seems a series of statements, it is in fact a
combination of high-order functions. F#'s syntactic sugar magic lets
you ignore this fact and just focus on the task at hand. We will see
in a few pages how this works under the hood. For the time being, I
invite you to see this as a way to easily express parsing activities
that you wish to execute in a specific sequence.  
Let me show you a second example. Say you want to parse a tuple:

```
(42, 99)
```

as an instance of:

```fsharp
(int * int)
```

So, it's a `(` followed by a number, then a comma, then some spaces,
etc. The corresponding needed parser is pretty much a literal
translation of this description:
  
  
```fsharp
let tuple : (int * int) Parser = 
    parser {
        let! _ = str "("
        
        let! a = number
        let! _ = comma
        let! _ = many space
        let! b = number
        
        let! _ = str ")"
        
        return (a, b)
    }
```

Isn't this very conveniently linear? It looks like just assigning
parsed values to variables. In fact, what you see on the right side of
a `let!` is not a parsed value, but a parser. The special `let!` runs
the parser on the right side, saves its result in the variable on the
left side (possibly, ignoring it) and then continues parsing the rest,
doing all the magic about passing `rest` and pattern matching the
`Result`.

Of course, you can add any complexity there, like recursive calls or
nested computation expressions. More on this in the upcoming pages.


## Lifting Functions
Manipulating parsers is so fun and rewarding. But often, you would
prefer to solve the problem at hand in terms of values, rather than in
terms of the parsers that emit those values: it's just one level of indirection less.  
If I may borrow a metaphor, it's like there are 2 separate realms: the
poor's man world of ordinary functions, manipulating simple values;
and the elevated World Of Parsers, up there beyond the clouds, a realm
full of funny operators, Functors and Monads. It would be awesome to
work down here on the ground, as we are already used to do, then to
pop the result into a magic elevator, hit the button for the Parser
World floor, and take it all in up there, for free. This is what the
lifting functions and operators are about. Let me show you.

Consider the case of parsing an arithmetic expression:

```
42+79
```

In the AST of your language, this can be represented as an instance of
`Expression`:

```fsharp
type Operation = Sum | Sub | Mul | Div

type Expression = Expression of int * int * Operation
```

Building an instance of `Expression` is trivially a matter of
defining:

```fsharp
let buildExpression (a: int) (op: Operation) (b: int) = 
    Expression (a, b, op)
```

and of invoking it:

```fsharp
let expression = buildExpression 42 79 Sum
```

Now, let's push `buildExpression` into the elevator. It will lift it
into the world of parsers, so that it becomes a
`buildExpressionOnSteroids`:

```fsharp
let buildExpressionOnSteroids = lift3 buildExpression
```

That's it. While `buildExpression` signature was:

```fsharp
val buildExpression : int -> Operation -> int -> Expression
```

by the application of `lift3` the signature turned into:

```fsharp
val buildExpressionOnSteoids : int Parser -> Operation Parser-> int
Parser -> Expression Parser
```

It became a parser combinator manipulating parsers to produce another parser! Unbelievable!  
Think about it: from a humble factory building *something* and knowing
absolutely nothing about parsing, you managed to create a function
that *parses that something*. Diabolic.

## Parser-Powered Function Application
Look this other example. As the Benevolent Dictator For Life of your
language, you proclaim that the syntax:

```
7 times date{16/03/1953}
```

builds a list of `7` dates (all the same), boxed inside a `MultiDate`
object. Sounds like a very useful construct, doesn't it?

```fsharp
type MultiDate = MultiDate of (DateOnly list)

let multiDate : MultiDate Parser = __

[<Fact>]
let ``parses a MultiDate`` () =
  let input = "7 times date{16/03/1953}"
  
  let date = DateOnly(1953, 03, 16)
  test <@ run multiDate input = 
             Success (MultiDate [date; date; date; date; date; date; date], "") @>
```

To build `multiDate`, you can start by splitting the input `7 times
date{16/03/1953}` into its syntactical components:

1. `7`: the number of dates you wish.
2. ` `: a space
3. `times`: one of your language's keywords.
4. ` `: a space
5. `DateOnly(1953, 03, 16)`: the date.

With those 5 values, building a `MultiDate` instance is a breeze:

```fsharp
let makeMultiDate (n: int) (_space: char) (command: string) (_space2: char) (date: DateOnly) : Foo =
    let dates = [ for i in 0 .. n - 1 -> date ]
    MultiDate dates
```

The problem is: you don't have *values*; instead, you have *parsers of
values*:

```fsharp
let nP:        int Parser      = intParser
let spaceP:    char Parser     = charParser ' '
let commandP:  string Parser   = str "times"
let dateP:     DateOnly Parser = parseDateOnly
```

Can you feed `makeMultiDate` with parsers instead of with actual
values?

```fsharp
let multiDate : MultiDate = 
    makeMultiDate     nP     spaceP     commandP     spaceP     dateP
```

Of course you can't! This won't even compile! That's not how function
application works.  
What if instead of the native F# function application you use a
specialized *parser-aware function application*?

```fsharp
let multiDate: MultiDate Parser =
//  makeMultiDate     nP     spaceP     commandP     spaceP     dateP
    makeMultiDate <!> nP <*> spaceP <*> commandP <*> spaceP <*> dateP
```

What the heck? It works!!! This funny syntax gives you back is *a
parser* for `MultiDate`. How can it be? There must be some black magic
involved!


## Did It Pique Your Curiousity?

If all of this sounds confusing, that's perfectly fine: I just hope it
also sounds a bit exciting.  
What you saw above involves a fair bit of syntactic sugar, and a good
amount of behind-the-scenes magic. As with any magic trick, true
understanding comes from peeking behind the curtain and rebuilding it
from scratch. That's exactly what we are doing in [the next
chapter](monadic-parser-combinators-7).

Enough with reading code: take a moment for a Yerba mate, warm up the
keyboard and finally hit some keys!


[Previous - A Different Kind of Coupling](/monadic-parser-combinators-5)
⁓ [Next - Parser-Powered Function Application!](/monadic-parser-combinators-7)


# References
[Satoshi Kon - Paprika (2006)][paprika]

[paprika]: https://en.wikipedia.org/wiki/Paprika_(2006_film)


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

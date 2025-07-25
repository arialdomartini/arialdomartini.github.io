---
layout: post
title: "Monadic Parser Combinators in F# - Bird's-Eye View Of What You Will Build"
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
4. Lifting functions and operators, to project ordinary functions into
  the Parser world.

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

one may prefer a cascade of calls connected with the pipe operator
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

Here's an example. You can think of a combinator to transform a
*parser of something* into a *parser of something surrounded by
tags*. It would take 3 parameters:

- A parser able to detect an opening tag.
- A parser for the closing tag.
- The parser you want to enrich.

Here's an implementation:

```fsharp
let between before after parser = 
    before >>. parser .>> after
```

Besided the implementation &mdash; which we will see in the next pages
&mdash; you can think to `>>.` and `.>>` as pipe operators similar to
the familiar `|>`: they connect the left parser with the right
parser. See the `.` on one side? It indicates which parser you want to
obtain the result from; the other parser will be executed, but then
its result will be ignored.

So, an expression like `a >>. b` can be read as:

- give me a parser that
- expects whatever the parser `a` expects
- then continues parsing whatever `b` is good at parsing
- and, finally, returns only the thing parsed by `b`, dropping the
result of `a`.

We will build several other little operators, like `|>>`, `>>=`, `<|>`
and the like. You'll have plenty of time to grasp them. For now, just
get the idea: you will end up enriching F# with a bunch of new little
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
For example, say you want to parse a tuple:

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
left side (possibly, ignoring the result) and then continues parsing
the rest, doing all the magic about passing `rest` and pattern
matching the `Result`.

Of course, you can have any complexity there, like recursive
calls or nested computation expressions. More on this in the upcoming pages.


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


# Refeences
[Satoshi Kon - Paprika (2006)][paprika]

[paprika]: https://en.wikipedia.org/wiki/Paprika_(2006_film)

[Previous - A Different Kind of Coupling](/monadic-parser-combinators-5)
⁓ [Next - Parser-Powered Function Application!](/monadic-parser-combinators-7)


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

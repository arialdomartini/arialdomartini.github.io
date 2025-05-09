---
layout: post
title: "Monadic Parser Combinators in F#"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
- property-based testing
ftrou: true
---
You want to invent a new language, and you want to do this in F#,
don't you? And, of course, you want to base its parser on Monadic
Parser Combinators. You've always wanted, just admit it.

<!--more-->

The compiler of your beautiful new language &mdash; with which you
want to bring the use of `goto` and `null` back to life &mdash; will
have several components:

* A Lexer, for splitting the source code into tokens.
* A Parser, to analyze the token sequence and to convert it into a
  syntax tree, and to check the grammatical structure.
* An Intermediate Code Generator.
* A Linker, and so on.

Here we are focusing on the very first component: a piece of code able
to analyze the source, to check its syntax against the esotheric
formal grammar you defined, and to generate something very well
structured for the next components to crunch.

It would be a (probably very complex) function with a signature
like:

```fsharp
type SourceCode = string

type AbstractSyntaxTree =
| Goto of Label
| VariableDefinition of ...
| ...

val parser : SourceCode -> AbstractSyntaxTree
```

Most likely, you would like the parser to fail and to emit syntax
errors in case there are any. Better return a `Result`, then:

```fsharp
type ParserError = { Expected: string; Encountered: string }
// or just: type ParserError = string

val parser : SourceCode -> Result<ParserError, AbstractSyntaxTree>
```

If you think about it, that's not qualitatively different from
deserializing a JSON string:

```fsharp
type JSON = string
type Error = string

val jsonDeserializer : JSON  -> Result<Error, MyObject>
```

Of course, it's likely that a programming language grammar is more
complex than the JSON grammar. But the two concepts are alike, and so are
the signatures.

[Tree-sitter][tree-sitter] too does something similar. It parses a string like:

```
"let x = 42;"
```

and emits a tree like:

```
(program
  (variable_declaration
    (lexical_declaration
      (identifier)
      (assignment_expression
        (number)))))
```

We can imagine the Tree-sitter grammar for F# as a function with this signature:

```fsharp
val treeSitter : SourceCode -> Result<TreeSitterError, TreeSitterSExpression>
```

I guess you see the pattern.  
A parser is a function that takes loosely-structured data (usually
&mdash; but not necessarily &mdash; text), and tries to build a more
structured data out of it, accordingly to the rules of a formal
grammar.

## An inherent recursive structure

We say that the input data is loosely-structured because, in fact, it
is not granted to adhere to the rules of the chosen grammar. Indeed,
if it violates them, then we expect the parser to fail and to emit an
error, to help the user identify the syntax errors.

There are multiple approaches to parsing, including the renowned
Regular Expressions.  
Monadic Parser Combinators are a particularly fascinating one: they
are an example of [Recursive Descent
Parsers][recursive-descent-parser]. This means that no matter how
complex the parser for a grammar is, it is defined based on smaller,
simpler parsers, and those in turn are defined based on even smaller
and simpler ones, and so on recursively, down to the trivial parsers.  
You can see the same from the opposite perspective: starting from the
trivial parsers, by *combining* them together and then by combining
their results, recursively, the parser for any arbitrary grammar can
be built.

Now, if writing the trivial parsers is, well, trivial, the only
challenge that's left is to learn how to *combine* parsers. That is,
how these Parsers Combinators work.

That's the goal of this series.

## How we will proceed

There are many similar series online, some specific to F# such as [The
"Understanding Parser Combinators" series][wlaschin] by Scott
Wlaschin. This post tries to stand out in a few different ways:

- Many tutorials begin with writing a simple parser &mdash;
conventionally, the single-character parser. This does not.  
Instead, we will focus on combinators first, postponing the
implementation of concrete parsers to the last chapter. When I was
first introduced to parsers, I was just confused: what on earth does
it mean to parse a single character? What's the point, where is this
leading me? My own 'aha!' moment came when I got how
composition turns tiny parsers into something actually useful.  
I hope I can help you skip past that initial disorientation entirely.

- We will write code with Test-Driven Development.  
Isn't it ironic that we developers often lament the absence of tests
in our daily job projects and yet, when it comes to writing posts,
tutorials and books, we never address testing at all?

Ready? Let's go started.

# References

* [Recursive Descent Parser][recursive-descent-parser]
* [Tree-sitter][tree-sitter]
* [Scott Wlaschin - The "Understanding Parser Combinators" series][wlaschin]

[recursive-descent-parser]: https://en.wikipedia.org/wiki/Recursive_descent_parser
[tree-sitter]: https://tree-sitter.github.io/tree-sitter/
[wlaschin]: https://fsharpforfunandprofit.com/series/understanding-parser-combinators/


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)


{% include fp-newsletter.html %}

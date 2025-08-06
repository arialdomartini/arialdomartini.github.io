---
layout: post
title: "Monadic Parser Combinators in F# - Intro"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
ftrou: true
---
You want to invent a new language, and you want to do this in F#,
don't you? And, of course, you want to base its parser on Monadic
Parser Combinators. You've always wanted. So, let's make it happen!

<!--more-->

## Index

1. [Intro](/monadic-parser-combinators)
2. [5 Shades Of Composability](/monadic-parser-combinators-2)
3. [That's a Combinator!](/monadic-parser-combinators-3)
4. [I Told You Not Mess With The Signature!](/monadic-parser-combinators-4)
5. [A Different Kind of Coupling](/monadic-parser-combinators-5)
6. [Mapping the Journey](/monadic-parser-combinators-6)
7. [Parser-Powered Function Application](/monadic-parser-combinators-7)
8. [Here Comes The Tuple](/monadic-parser-combinators-8)
9. [Things You Don't Care About](/monadic-parser-combinators-9)
10. [Applying Functions, Ad Nauseam](/monadic-parser-combinators-10)
11. [Lifting Functions](/monadic-parser-combinators-11)
12. [Things You Are Not Sure About](/monadic-parser-combinators-12)
13. [Things You Want To Remember](/monadic-parser-combinators-13)
14. [Mind The Context](/monadic-parser-combinators-14)
15. [One Combinator to Rule Them All](/monadic-parser-combinators-15)
16. [A Programmable Semicolon](/monadic-parser-combinators-16)

## Introduction

The compiler of your beautiful new language &mdash; with which you
will bring the use of `goto` and `null` back to life &mdash; will have
several components:

* A Lexer, for splitting the source code into tokens.
* A Parser, to convert the token sequence into a syntax tree, and to
  check the grammatical structure.
* An Intermediate Code Generator.
* A Linker, and so on.

Here we are focusing on the very first component: a piece of code able
to analyze the source, to check its syntax against the esoteric
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

If you think about it, that's not qualitatively different from
deserializing a JSON string:

```fsharp
type JSON = string

val jsonDeserializer : JSON  -> MyObject
```

Of course, it's likely that a programming language grammar is more
complex than the JSON grammar. But the two concepts are alike, and so are
the signatures.

[Tree-sitter][tree-sitter] too does something similar. It parses a string like:

```
"let x = 42"
```

and it emits a tree like:

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
val treeSitter : SourceCode -> TreeSitterSExpression
```

I guess you see the pattern.  
A parser is a function that takes loosely-structured data (usually
&mdash; but not necessarily &mdash; text), and tries to build a more
structured data out of it, accordingly to the rules of a formal
grammar.

## Mr.James, It's Parsers all The Way Down

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
how Parsers Combinators work.

That's the goal of these pages.

## How we will proceed

There are many similar series online, some specific to F# &mdash; such
as [The "Understanding Parser Combinators" series][wlaschin] by Scott
Wlaschin &mdash; many others based on Haskell, like the excellent
[Parser Combinators: a Walkthrough, Or: Write you a Parsec for Great
Good][leblanc] by Antoine Leblanc.  
This post tries to stand out in a few different ways:

- If other attempts to this topic left you scratching your head, this
  series should make things a lot easier.  
  I've done my best to keep the learning curve as smooth as possible.
  Having to pick between being brief and assuming you knew a lot, or
  taking a longer path I went with the latter. I think it's nicer to
  know why stuff works rather than being hit with jargon-heavy
  explanations.

- Many tutorials begin with writing a simple parser &mdash;
  conventionally, the single-character parser. This does not. Instead,
  we will focus on combinators first, postponing the implementation of
  concrete parsers. When I was first introduced to parsers, I was just
  confused: what on earth does it mean to parse a single character
  returning a character?  What's
  the point? Where is this leading me?  
  I hope I can help you skip past that initial disorientation
  entirely.

- Parser Combinators are the the *leit-motiv* and serve as the central
  theme of this book. Nevertheless, we'll often stray from the main
  path and let our imagination roam, exploring a variety of other
  subjects along the way. You can consider these pages an invitation
  to discover Functors, Applicatives, and Monads.

- We will write code with Test-Driven Development.  
  Isn't it ironic that we developers often lament the absence of tests
  in our daily job projects and yet, when it comes to writing posts,
  tutorials and books, we never address testing at all?


Fine, enough with the introduction. Ready? Treat yourself to a sorbet,
then [let's get started](/monadic-parser-combinators-2).

## Notes

I am not a native English speaker: if you spot any typo or weird
sentence, feel free to [send me a pull
request](https://github.com/arialdomartini/arialdomartini.github.io/).

This blog is crafted by people, not AI. Illustrations are original
work by Nanou.

[Next: 5 Shades Of Composability](/monadic-parser-combinators-2)

# References

* [Recursive Descent Parser][recursive-descent-parser]
* [Tree-sitter][tree-sitter]
* [Scott Wlaschin - The "Understanding Parser Combinators" series][wlaschin]
* [Antoine Leblanc - Parser Combinators: a Walkthrough, Or: Write you
  a Parsec for Great Good][leblanc]
* [Turtles all the way down][turtles]

[recursive-descent-parser]: https://en.wikipedia.org/wiki/Recursive_descent_parser
[tree-sitter]: https://tree-sitter.github.io/tree-sitter/
[wlaschin]: https://fsharpforfunandprofit.com/series/understanding-parser-combinators/
[leblanc]: https://hasura.io/blog/parser-combinators-walkthrough
[turles]: https://en.wikipedia.org/wiki/Turtles_all_the_way_down

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)


{% include fp-newsletter.html %}

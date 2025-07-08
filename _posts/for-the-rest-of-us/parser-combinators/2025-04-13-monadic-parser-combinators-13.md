---
layout: post
title: "Monadic Parser Combinators in F# - Mind the Context"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
include_in_index: false
---
Boiling the problem down, we can say that, in a Context-sensitive
grammar, we need a parser to read a value &mdash; like a `Foo Parser`
&mdash; and then use another parser based on that value &mdash; such
as a `Foo -> Bar Parser`. Keeping types generic, this means that first
we find an `'a Parser`, then eventually an `'a -> 'b Parser` follows.

Our goal is to write a parser combinator (`bind` or `>>=`) that given
those 2 elements generates us back a `'b Parser`:

```fsharp
bind : 'a Parser -> ('a -> 'b Parser) -> 'b Parser
```

First things first: we need a unit test. To keep things simple, we
strip the rules down to the minimum, and we work with empty XML nodes;
in other words, we remove the XML node content so that the opening tag
is directly followed by the closing tag:

| Input string  | Expected parsing result  |
|---------------|--------------------------|
| `<pun></pun>` | `Success ("", "pun")`    |
| `<pun></xyz>` | `Failure "Expected pun"` |


```fsharp
let tagNameP = many1 (anyOf ['a'..'Z'])

let openingTagP = tagNameP |> between (str "<")  (str ">")
let closingTag tagName = (str tagName) |> between (str "</") (str ">")

let bind (m: 'a Parser) (f: 'a -> 'b Parser) =
    failwith "Not yet implemented"

let (>>=) = bind

let openCloseP =
    openingTagP >>= (fun tagName -> closingTag tagName)

let ``closingTag works in a context-sensitive grammar`` () =

  test <@ run openCloseP "<pun></pun>" = Success ("", "pun") @>
  test <@ run openCloseP "<pun></xyz>" = Failure ("Expected pun") @>
```

The disrupting element is the closing tag parser, that depends on the
`tagName` value, parsed by the previous parser. The combination of the
2 parsers is obtained by the application of `>>=`:


```fsharp
let openCloseP = 
    openingTagP >>= (fun tagName -> closingTagP tagName)
```

Read it like this:

* Combining the `openingTagP` with `closingTagP` requires
* First, to apply `openingTagP`.
* Then (`>>=`) passing the parsed value (`fun tagName ->`)
* To the next parser (`closingTagP tagName`)


If you squint your eyes you could read the funny `>>=` syntax as:

```fsharp
let openCloseP = 
         openingTagP    >>=    (fun tagName -> closingTagP tagName)
// apply openingTagP   then    pass tagName to closingTagP 
```

Oh! Of cours you can write this in Point Free style:


```fsharp
let openCloseP = openingTagP >>= closingTagP
```

Believe me or not, you will eventually find this syntax more
expressive and clear than the former.  
Fine. Let's finally implement this infamous `bind` combinator.

## Follow the type signature
```fsharp
// 'a Parser -> ('a -> 'b Parser) -> 'b Parser
let bind m f = ...
```

Going with the flow and following the type signature, we know we have
to return a `'b Parser`:


```fsharp
let bind m f = Parser (fun s ->
    ...)
```

We have the input string and `m`, the `'a Parser`. If we run this
parser with the input string, we will get back an `'a` value:

```fsharp
let bind m f = Parser (fun s ->
    let a, rest = run m s
    ...)
```

The `'a` value is exactly what we needed to get the `'b Parser`:


```fsharp
let bind m f = Parser (fun s ->
    let a, rest = run m s
    let bParser = f a)
```

We are done! We got the `'b Parser` we wanted. We cannot just return
it, because our code is all surronded by `Parser (fun s -> ...)` and
we would end up with a parser inside a parser. Idea: we can run the `b
Parser` with the `rest` input to get its parsed value:


```fsharp
let bind m f = Parser (fun s ->
    let a, rest = run m s
    let bParser = f a
    let b = run bParser rest
    b)
```

Test it. Green!



# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

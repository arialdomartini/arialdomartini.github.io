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
parsing a `Foo` instance &mdash; and then use another parser based on
that value &mdash; such as a `Foo -> Bar Parser` consuming that `Foo`
value to generate a `Bar Parser`. Keeping types generic, this means
that first we find an `'a Parser`, then eventually an `'a -> 'b
Parser` follows.

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
let alphaChars = [ 'a' .. 'z' ] @ [ 'A' .. 'Z' ]
let punctuationMarks = [' '; ';'; ','; '.']

let tagNameP = many1 (anyOf alphaChars) |>> String.Concat

let openingTagP = tagNameP |> between (str "<")  (str ">")
let closingTagP tagName = (str tagName) |> between (str "</") (str ">")

let bind m f = failwith "Not yet implemented"

let (>>=) = bind
let return' v = Parser (fun s -> Success (s, v))

let nodeP = failwith "Not yet implemented"

[<Fact>]
let ``closingTag works in a context-sensitive grammar`` () =
  let s = "<pun>Broken pencils are pointless</pun>rest"

  let expected =
      { tag = "pun"
        content = "Broken pencils are pointless" }

  test <@ run nodeP s = Success ("rest", expected) @>

[<Fact>]
let ``not matching closing tags raise a failure`` () =
  let s = "<pun>Broken pencils are pointless</xml>rest"

  test <@ run nodeP s = Failure "Expected pun" @>
```

The disrupting element is the closing tag parser, since it depends on
the `tagName` value parsed by the previous parser. The combination of
the 2 parsers is obtained by the application of `>>=`:

```fsharp
let nodeP = 
    openingTagP >>= (fun tagName ->
        contentP >>= (fun content ->
            (closingTagP tagName) >>= (fun _ ->
                return' { tag = tagName; content = content })))
```


If you squint your eyes you could read the funny `>>=` syntax as:

```fsharp
let openCloseP = 
         openingTagP    >>=    (fun tagName -> ...)
// apply openingTagP   then    pass tagName to a lambda continuation
```

so you can read the whole sequence as:

* In order to parse an XML node
* We first parse the opening tag (`openingTagP`).
* Then, we pass forward the value it parses (`>>= (fun tagName -> ...`)
* To a continuation. This, in turn will parse the content (`contentP`)
* Passing forward the parsed value (`>>= (fun content -> ...`) 
* to the next part. This will use `tagName` to build the
  parser for the closing tag (`closingTagP tagName`)
* Finally, handing over (`>>= fun _ ->`) to the last part
* Whose purpose is to just return an instance of the tag record.


Notice that `return'` is identical to the `pure'` function introduced
with Applicative Functors.

If you find this code convoluted because of the value passing boiler
plate, you are absolutely right: it sucks. Hang in there for a few
more minutes: soon we will introduce a technique to dramatically
streamline the code.

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

We have the input string `s` and `m`, the `'a Parser`. If we run this
parser with the input string, we will get back a parsing result,
possibly containing a parsed value `a: 'a`:

```fsharp
let bind m f = Parser (fun s ->
    let resultA = run m s
    ...)
```

We are not sure that the parsing succeeded. We'd better pattern
match. Of course, in case of failure, we can let `binda just fail.

```fsharp
let bind m f = Parser (fun s ->
    let resultA = run m s
    match resultA with
    | Failure f -> Failure f
    | Success(rest, a) ->
        ...)
```

In case of success, we get the the unconsumed input and the `'a`
value: exactly what we needed to get the `'b Parser`:


```fsharp
let bind m f = Parser (fun s ->
    let resultA = run m s
    match resultA with
    | Failure f -> Failure f
    | Success(rest, a) ->
        let bParser = f a
        ...)
```

We are done! We got the `'b Parser` we wanted. We cannot just return
it, because our code is surrounded by `Parser (fun s -> ...)`  and we
would end up with a parser inside a parser. Idea: we can `run` the `b
Parser` with the `rest` input to get its parsed value:


```fsharp
let bind m f = Parser (fun s ->
    let resultA = run m s
    match resultA with
    | Failure f -> Failure f
    | Success(rest, a) ->
        let bParser = f a
        run bParser rest)
```

Test it. Green!

## Is That All, Folks?

You might not be impressed by this result. In fact, it's an explosive
one. This little unsuspected `bind` function, together with `return'`,
is so powerful that it could replace everything you did in the last 13
chapters. It's such a game changer that F# provides native support for
its style, which will bring a dramatic shift to both the syntax and
style of your code, for the better.

If you never enjoyed a Tamil Kootu, that's the perfect chance to give
it a try. [Chapter 14](/monadic-parser-combinators-14), here we come!

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

---
layout: post
title: "Monadic Parser Combinators in F# - Mind the Context"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
include_in_index: false
---
Boiling the problem down, we can say that, in Context-sensitive
grammars, the key challenge is that the parsing of later elements
depends on values obtained earlier. For example, suppose you have an
`int Parser` to read a number. Then, based on that number, you
dynamically create a new parser tailored to it, essentially a parser
factory `int -> Foo Parser`, that takes the parsed integer and returns
a parser for a structure depending on that integer.

If this sounds like a contrived example, here are other cases where
this context-sensitiveness may result more intuitively justified:

- Making sure that parentheses are properly balanced in nested
  expressions such as `(f a, g (h (b, c)))`: each opening bracket must
  have a corresponding closing one. While you proceed parsing, somehow
  you have to carry along a counter.

- Checking that variables are declarated before use.

- Indentation-based block structures in F#: the number of leading
  spaces of each line depends on the indentation level established
  earlier. Again, you have to carry along some context.

- Parsing command line options, where the presence of one option
  modifies the availability of subsequent options. For this case you
  might count on a parser factory that, depending on the previous
  option, generates one or another parser for the next option.

- Besides programming languages, as a linguistic example, checking
  that in subject+verb+object structures the agreement in gender and
  number is ensured. As a trivial example, you want that "I am" and
  "You are" succeed, and that "I are" and "You am" fail.


## It All Begins With A Signature
Keeping types generic, this means that first we find an `'a Parser`,
then eventually an `'a -> 'b Parser` follows. Our goal is to write the
parser combinator `bind` (or `>>=`) that, given those 2 elements,
generates a `'b Parser`:

```fsharp
val bind : 'a Parser -> ('a -> 'b Parser) -> 'b Parser
```

We are also going to use our old acquaintance `pure'`, but we will
adhere to the convention to call it `return'`. 

```fsharp
let return' = pure'
```

First things first: we need a unit test. Let's continue working with
the XML tag example:

```fsharp
let bind m f = __

let (>>=) = bind
let return' = pure'


type Node =
    { tag: string
      content: string }

let alphaChars = [ 'a' .. 'z' ] @ [ 'A' .. 'Z' ]
let punctuationMarks = [' '; ';'; ','; '.']

let tagNameP = many1 (anyOf alphaChars) |>> String.Concat

let openingTagP = tagNameP |> between (str "<")  (str ">")
let makeClosingTagP tagName = (str tagName) |> between (str "</") (str ">")

let contentP = many (anyOf (alphaChars @ punctuationMarks)) |>> String.Concat


let nodeP = __

[<Fact>]
let ``closingTag works in a context-sensitive grammar`` () =
  let s = "<pun>Broken pencils are pointless</pun>rest"

  let expected =
      { tag = "pun"
        content = "Broken pencils are pointless" }

  test <@ run nodeP s = Success (expected, "rest") @>

[<Fact>]
let ``not matching closing tags raise a failure`` () =
  let s = "<pun>Broken pencils are pointless</xml>rest"

  test <@ run nodeP s = Failure "Expected pun" @>
```

Even before implementing `>>=`, it is worth to analyze its use. The
disrupting element is the closing tag parser, since it depends on the
`tagName` value parsed by the previous parser. The combination of the
2 parsers is obtained by the application of `>>=`. Given its
signature, you can use it like this:


```fsharp
let openThenCloseP = 
    openingTagP >>= (fun tagName ->
            let closingTagP = makeClosingTagP tagName
            ...)
```

* First parse the opening tag (`openingTagP`).
* Then, pass forward the value it parses (`>>= (fun tagName -> ...`)
as the argument to a continuation.
* The continuation can use that value to invoke `makeClosingTagP` to
  generate a tailored `closingTagP` parser
* ...


We are not required to immediately use the `tagName` value: in fact,
between the opening and the closing tags, we want to take the chance
to parse the content. It's a matter of using a chain of `>>=` applications:

```fsharp
let nodeP = 
    openingTagP >>= (fun tagName ->
        contentP >>= (fun content ->
            (makeClosingTagP tagName) >>= (fun _tagName ->
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
* first parse the opening tag (`openingTagP`).
* Then, pass forward the value it parses (`>>= (fun tagName -> ...`)
* to a continuation. This, in turn will parse the content (`contentP`)
* eventually passing forward the parsed value (`>>= (fun content ->
  ...`)
* to the next part. This will use `tagName` to build the
  parser for the closing tag (`closingTagP tagName`)
* Finally, handing over (`>>= fun _tagName ->`) to the last part (not
  interested in the last parsed value)
* whose purpose is to just return an instance of the tag record
  (wrapped in a Parser, with `return'`).


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
    | Success(a, rest) ->
        ...)
```

In case of success, we get the `'a` value and the unconsumed input:
exactly what we needed to get the `'b Parser`:


```fsharp
let bind m f = Parser (fun s ->
    let resultA = run m s
    match resultA with
    | Failure f -> Failure f
    | Success(a, rest) ->
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
    | Success(a, rest) ->
        let bParser = f a
        run bParser rest)
```

Test it. Green! You just made `Parser` a Monad.

## Is That All, Folks?

You might not be impressed by this result (surprisingly, `Parser` did
not turn into a burrito). In fact, it's an explosive one. This little
unsuspected `bind` function, together with `return'`, is so powerful
that it could replace everything you did in the last 13 chapters. It's
such a game changer that F# provides native support for its use, which
will bring a dramatic shift to both the syntax and style of your code,
for the better.

This has been a tough chapter and you deserve some rest. If you never
enjoyed a Tamil Kootu, that's the perfect chance to give it a
try. [Chapter 15](/monadic-parser-combinators-15), here we come!


[Previous - Things You Want To Remember](/monadic-parser-combinators-13) ⁓
[Next - One Combinator to Rule Them All](/monadic-parser-combinators-15)


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

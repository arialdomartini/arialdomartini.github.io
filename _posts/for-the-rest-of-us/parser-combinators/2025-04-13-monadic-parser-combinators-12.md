---
layout: post
title: "Monadic Parser Combinators in F# - Things You Want To Remember"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
- property-based testing
include_in_index: false
---
Let's write the parser for an XML node. This is a task dressed up as a
walk in the park, but in fact a park hiding an insidious maze
inside. Let's see why.

## Opening and closing tags
To keep things simple, we assume that a node is any text surrounded by
an opening tag &mdash; such as `<pun>` &mdash; and its corresponding
closing tag &mdash; in this case `</pun>`. The parser should work
whatever the tag is, so any of the following strings should be
successfully parsed:

* `<pun>I started out with nothing, and I still have most of it</pun>`
* `<gardenPathSentence>Time flies like an arrow; fruit flies like
  bananas</gardenPathSentence>`<sup>1</sup>
* `<well>well</well>`

(<sup>1</sup>. By the way: parsing a [Garden Path Sentence][garden-path-sentence] is
really [a topic on its own][parsing-garden-path])

Let's say that we want to parse nodes as instances of this record:

```fsharp
type Node = 
    { tag: string
      content: string }
```

Fine! How hard can it be? The first recipe that comes into mind is:

- We parse the tag name between `<` and `>` using `between`.
- Then we parse the content, combining `many` and `anyOf`.
- Then, we parse again the tag name, this time betweeen `</` and
  `>`. Of course, we will use `between` again.
- Finally, we combine all that we parsed to build an instance of
  `Node`, either using the applicative functor's `<*>` or lifting the
  `Node` costructor with `lift3`.

It really seems that we have all the ingredients we need. Let's put
this down in code.

```fsharp
let tagNameP = many1 (anyOf ['a'..'Z'])

let openingTagP = tagNameP |> between (str "<")  (str ">")
let closingTag  = tagNameP |> between (str "</") (str ">")

let content = many (anyOf ['a'..'Z'] @ [' '; ';'; ','])

let buildNode openingTag content _closingTag =
    { tag = openingTag
      content = content }

let nodeP = buildNode <!> openingTagP <*> content <*> closingTagP

let ``parses an XML node`` () =
  let s = "<pun>Broken pencils are pointless</pun>"
  
  let expected = 
      { tag = "pun"
        content = "Broken pencils are pointless"}
  
  test <@ run nodeP s = Success ("", expected) @>
```

That simple, what's the big deal?  
The big deal is: this implementation is wrong. Did you spot the bug?

## semordnilap tags
If you did not, let me make it more apparent. Indulge me while I
introduce a little change in the XML grammar, in line with the
craziness of your yet-to-be-invented programming language: let's ask
the user to type the closing tag name by spelling it backward. This
will have the perplexing effect of producing tag couples like
`<stressed>...</desserts>`, `<repaid>...</diaper>`,
`<evilStar>...</RatsLive>`. Amusing!

Now: parser combinators are composable, so simply improving the
`closingTag` parser should allow the entire XML node parser to benefit
from the change. After all, that's exactly the selling point of parser
combinators, right? Reversing a string is dead easy:

```fsharp
let reverse (s: string) = new string(s.ToCharArray() |> Array.rev)
```

Therefore, creating a parser for closing tags should be a matter of
lifting this `reverse` function to the parser world. We start from:

```fsharp
let tagNameP = many1 (anyOf ['a'..'Z'])
let openingTagP = tagNameP |> between (str "<") (str ">")
let closingTag = tagNameP |> between (str "</") (str ">")
```

Maybe we could try mapping `reverse`, with `<!>`:

```fsharp
let tagNameP = many1 (anyOf ['a'..'Z'])
let PemaNgat = reverse <!> tagNameP

let openingTagP = tagNameP |> between (str "<") (str ">")
let closingTag = PemaNgat |> between (str "</") (str ">")
```

Does this work? I don't know, pal, how to say? Did we just forget to
work with TDD? Where are tests? Let's put it right at once!

```fsharp
[Fact]
let ``parses an XML tag node with semordnilap tags`` () =
  let s = "<hello>ciao ciao</olleh>"
  
  let expected = 
      { tag = "hello"
        content = "ciao ciao"}
  
  test <@ run nodeP s = Success ("", expected) @>
  


[<Theory>]
[<InlineData("foo")>]
[<InlineData("barBaz")>]
[<InlineData("evil")>]
[<InlineData("live")>]
let ``possible tag names`` (s: string) =
    test <@ run tagNameP s = Success("", s)@>

[<Theory>]
[<InlineData("oof")>]
[<InlineData("zaBrab")>]
[<InlineData("live")>]
[<InlineData("evil")>]
let ``possible closing tag names`` (s: string) =
    test <@ run PemaNgat s = Success("", s)@>
```

Yes, it works. But: did you notice that we have `evil` and `live` in
both the input sets?  And that both the tests are green? Well, that
should not come a surprise: `evil` is a legit closing tag name,
because it's the reverse of `live`. And `live` too is a legit closing
tag name, because it's the reverse of `evil`. And both are legit
*opening* tag names.  
On second thought, any string must be both a legit opening and closing
tag name, because it's the reverse of, ehm, its reverse. Let's check
this claim with a random string:

```fsharp
open System

let ```any string can be both an opening and a closing tag name` () =
    let random = Random()
    let letters = [|'a'..'z'|] @ [|'A'..'Z'|] |> Array.ofList

    let randomString =
        [| for _ in 1 .. 10 -> letters.[random.Next(letters.Length)] |]
        |> System.String

    test <@ run tagNameP randomString = Success("", s)@>
    test <@ run PemaNgat randomString = Success("", s)@>


Green. We could even ask FsCheck to generate 100 random strings, to
dispel any doubt:

```fsharp
[<Property>]
let ```any string can be both an opening and a closing tag name` () =
    test <@ run tagNameP s = Success("", s)@>
    test <@ run PemaNgat s = Success("", s)@>
```

Green.  
Does it mean that our XML node parse would just accept any
closing tag? Let's see:


```fsharp
[Fact]
let ``parses an XML tag node with an arbitrary closing tag`` () =

  let s = "<hello>ciao ciao</picture>"
  
  let expected = 
      { tag = "hello"
        content = "ciao ciao"}
  
  test <@ run nodeP s = Success ("", expected) @>
```

Uh oh! Not a good news! Is this maybe a problem we introduced with our
funny semordnilap-based syntax? Let's go back to the conventional tag
name rule. We started from this test:

```fsharp
let tagNameP = many1 (anyOf ['a'..'Z'])

let openingTagP = tagNameP |> between (str "<") (str ">")
let closingTagP = tagNameP |> between (str "</") (str ">")
let content = many (anyOf ['a'..'Z'] @ [' '; ';'])

let buildNode openingTag content _closingTag =
    { tag = openingTag
      content = content }

let nodeP = buildNode <!> openingTagP <*> content <*> closingTagP

let ``XML node test`` () =
  let s = "<pun>Broken pencils are pointless</pun>"
  
  let expected = 
      { tag = "pun"
        content = "Broken pencils are pointless"}
  
  test <@ run nodeP s = Success ("", expected) @>
```

What if we use a completely unrelated closing tag?

```fsharp
let ``XML node test`` () =
  let s = "<pun>Broken pencils are pointless</picture>"
  
  let expected = 
      { tag = "pun"
        content = "Broken pencils are pointless"}
  
  test <@ run nodeP s = Success ("", expected) @>
```

Oh, no! It's still green! So, this bug is really inherent.

## Lack of context
If you think about it, in the definitions of `openingTagP` and `closingTagP`:

```fsharp
let tagNameP = many1 (anyOf ['a'..'Z'])

let openingTagP = tagNameP |> between (str "<") (str ">")
let closingTagP = tagNameP |> between (str "</") (str ">")
```

there is no indication at all that the tag name of the closing tag
must be exactly the same tag name parsed by the opening tag.\

"How so?", you may cry, "It's using the very same `tagNameP`!  They
must be the same tag name!".  
Not quite. `openingTagP` and `closingTagP` share the same tag name
*parser*, not the same tag name value. A parser, remember, is a
function eventually returning a parsed value. It's not that
value.

`tagNameP`, as it is defined, would succeed with *any* string
of letters. `PemaNgat` would also succeed with *any* string. Possibly,
and most likely, with any unrelated string. There is really no
connection between the two.

What we would rather do, instead, is to build `closingTagP` as the
parser expecting *exactly* the *value* parsed by
`openingTagP`. Something like:

```fsharp
let tagNameP = many1 (anyOf ['a'..'Z'])

let openingTagP = tagNameP |> between (str "<") (str ">")
let closingTag openingTagName = (str openingTagName) |> between (str "</") (str ">")
```

You see the tragedy? The value of `openingTagName` is not known until
we physicall run the `openingTagP` parser. We encountered several
parsers depending on other parsers. In fact, this is the first time we
have a parser depending on *the result* of another parser. Seeing this
from another perspective: it's the first time that our grammar
requires a parser having a notion of its surrounding context.

Do you remember when I stated "We assume that a node is whatever is
surrounded by an opening tag &mdash; such as `<joke>` &mdash; and its
*corresponding* closing tag"? The notion we just forgot to take into
account is related to that *corresponding* word. It's only intuitive
that this *corresponding* has to do with some kind of relationship
between the elements of a grammar and, consequently, some kind of
binding between its parsers.

Indeed, the XML grammar, with closing tags constrained to have a name
bound to their corresponding opening tag, is a so called
Context-sensitive Grammar.

A parser for this family of grammars requires a new tool that &mdash;
it could be demonstrated &mdash; cannot be built as a composition of
the applicative parsers we have distilled so far. We need a new tools.

This new tool is indeed pretty simple: an operator similar to the
Applicative Functor's `<*>`, only a bit smarter; a function able to
pass the value successfully parsed by a parser to the next parser. So,
something that could *bind* 2 parsers in a row.

Not surprisingly, we will call this operator `bind` &mdash; or `>>=`,
because we functional programmers can't get enough of symbols &mdash;
and the resulting notion *monad*.

Implementing is is super easy &mdash; just a matter of be guided by
the type signature &mdash; but the consequence of its availability is
revolutionary.

Grab a liquorice and jump to [Chapter
13](/monadic-parser-combinators-13): we are going to write it.


# References

* [Garden Path Sentence][garden-path-sentence]
* [semordnilaP][semordnilap]
* [Parsing of Garden Path Sentences][parsing-garden-path]

[semordnilap]: https://en.wiktionary.org/wiki/semordnilap
[garden-path-sentence]: https://en.wikipedia.org/wiki/Garden-path_sentence
[parsing-garden-path]: https://en.wikipedia.org/wiki/Parsing

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

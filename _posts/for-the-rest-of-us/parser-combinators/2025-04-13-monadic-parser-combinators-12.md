---
layout: post
title: "Monadic Parser Combinators in F# - Things You Want To Remember"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
include_in_index: false
---
Let's write the parser for an XML node. This is a task dressed up as a
walk in the park, but in fact hiding an insidious maze inside. Let's
see why.

## Opening and closing tags
We assume that a node is any text surrounded by an opening tag &mdash;
such as `<pun>` &mdash; and its corresponding closing tag &mdash; in
this case `</pun>`. The parser should work with arbitrary tag names,
so any of the following strings should be successfully parsed:

* `<pun>I started out with nothing, and I still have most of it</pun>`
* `<gardenPathSentence>Time flies like an arrow; fruit flies like
  bananas</gardenPathSentence>`<sup>1</sup>
* `<well>well</well>`

For the sake of simplicity, we won't support nested nodes nor
attributes.

(<sup>1</sup> By the way: parsing a [Garden Path Sentence][garden-path-sentence] is
really a topic on its own).

Let's say that parsing nodes should give us back instances of this
record:

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

It really seems that we have all the ingredients we need. Let's write
this down into code:

```fsharp
type Node =
    { tag: string
      content: string }

let alphaChars = [ 'a' .. 'z' ] @ [ 'A' .. 'Z' ]
let punctuationMarks = [' '; ';'; ','; '.']

let tagNameP = many1 (anyOf alphaChars) |>> String.Concat

let openingTagP = tagNameP |> between (str "<")  (str ">")
let closingTagP = tagNameP |> between (str "</") (str ">")

let contentP = many (anyOf (alphaChars @ punctuationMarks)) |>> String.Concat

let buildNode openingTag content _closingTag =
    { tag = openingTag
      content = content }

let nodeP = buildNode <!> openingTagP <*> contentP <*> closingTagP

[<Fact>]
let ``parses an XML node`` () =
  let s = "<pun>Broken pencils are pointless</pun>"

  let expected =
      { tag = "pun"
        content = "Broken pencils are pointless"}

  test <@ run nodeP s = Success ("", expected) @>
```

That was really simple, indeed. What was the big deal?  
The big deal is: this implementation is wrong. Did you spot the bug?

## semordnilap tags
If you did not, let me make it more apparent. Indulge me while I
introduce a little silly change in the XML grammar, in line with the
craziness of your yet-to-be-invented programming language: let's ask
the user to type the closing tag name by spelling it backward, as a
[semordnilap][semordnilap]. This will have the delightful effect of
producing tag couples like `<stressed>...</desserts>`,
`<repaid>...</diaper>`, `<evilStar>...</RatsLive>`. Amusing!

Now: parser combinators are composable, so simply improving the
`closingTag` parser should allow the entire XML node parser to benefit
from the change. After all, that's exactly their selling point, right?
Reversing a string is dead easy:

```fsharp
let reverse (s: string) = new string(s.ToCharArray() |> Array.rev)
```

Therefore, creating a parser for closing tags should be a matter of
lifting this `reverse` function to the parser world. Maybe we could
try mapping `reverse`, with `<!>`:

```fsharp
let PemaNgat = reverse <!> tagNameP
        
let openingTagP = tagNameP |> between (str "<") (str ">")
let closingTag =  PemaNgat |> between (str "</") (str ">")
```

Does this work? I don't know, pal, how can I tell? Didn't we just
forget to work with TDD? Where are tests? Let's put it right at once!

```fsharp
[<Fact>]
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
    test <@ run PemaNgat s = Success("", reverse s)@>

```

Yes, it seems to works.  
Did you notice that we have `evil` and `live` in the test input set
for for both the opening and the closing tags? And in both cases the
tests are green?  Well, that's not surprising: `evil` is a legit
closing tag name, because it's the reverse of `live`. And `live` too
is a legit closing tag name, because it's the reverse of `evil`. And
both are also legit *opening* tag names.  
The test for the closing tag requires that a string is the reverse of
something. On second thought, it's a very loose constraint: any string
is the reverse of, ehm, its reverse. Does it mean that this test would
pass no matter the string? Let's find it out with a random string:

```fsharp
[<Fact>]
let ``a random string can be both an opening and a closing tag name`` () =
    let random = System.Random()

    let randomString =
        [| for _ in 1 .. 10 -> alphaChars.[random.Next(alphaChars.Length)] |]
        |> System.String

    test <@ run PemaNgat randomString = Success("", reverse randomString)@>
```

Wait a minute! Does it mean that our XML node parser would just accept
any closing tag? Let's see:


```fsharp
[<Fact>]
let ``accepts a wrong closing tag`` () =

  let s = "<hello>ciao ciao</picture>"

  let expected =
      { tag = "hello"
        content = "ciao ciao"}

  test <@ run nodeP s = Success ("", expected) @>
```

Uh oh! Not a good news! (Note to self: next time, not only shall I
write tests before the implementation, but I should also not forget
the red phase of the red-green-refactor TDD mantra. Also, I should
test both the happy and the failure cas). Is this a bug introduced by
the semordnilap-based syntax? Let's revert back to the conventional
tag name rule, using a completely unrelated closing tag:

```fsharp
let tagNameP = many1 (anyOf ['a'..'Z'])

let openingTagP = tagNameP |> between (str "<") (str ">")
let closingTagP = tagNameP |> between (str "</") (str ">")

[<Fact>]
let ``XML node test`` () =
  let s = "<pun>Broken pencils are pointless</picture>"

  let expected =
      { tag = "pun"
        content = "Broken pencils are pointless"}

  test <@ run nodeP s = Success ("", expected) @>
```

Oh, no! It's still green! So, this bug is really inherent.

## Lack of context
If you think about it, in the definition of `openingTagP` and
`closingTagP`:

```fsharp
let tagNameP = many1 (anyOf ['a'..'Z'])

let openingTagP = tagNameP |> between (str "<") (str ">")
let closingTagP = tagNameP |> between (str "</") (str ">")
```

there is no indication at all that the tag name of the closing tag
must match the string parsed by the opening tag.  
"How so?" I can hear you cry: "They are using the very same
`tagNameP`! They must match the same tag name! It's literally written there!"  
Not quite. `openingTagP` and `closingTagP` share the same tag name
*parser*, not the same tag name *value*. Remember? A parser is a
function eventually returning a parsed value. It's not that
value. It's like a promise of a value.

`tagNameP`, as it is defined, would succeed with *any* sequence of
letters. `PemaNgat` would also succeed with *any* sequence of
letters. Possibly, and most likely, with unrelated ones. There is
really no connection between the two.

What we would rather do, instead, is to build `closingTagP` as the
parser expecting *exactly* the *value* parsed by
`openingTagP`. Something like:

```fsharp
let tagNameP = many1 (anyOf ['a'..'Z'])

let openingTagP = tagNameP |> between (str "<") (str ">")
let closingTag (openingTagName: string) =
    (str openingTagName) |> between (str "</") (str ">")
```

You see the tragedy? The value of `openingTagName` is not known until
we physicall run the `openingTagP` parser. Until this page, we have
encountered several parsers depending on other parsers. But this is in
fact the first time we have a parser depending on *the result* of
another parser. Watching this from another perspective: it's the first
time that the elements of our grammar requires parsers having a notion
of their surrounding context.

Do you remember when I stated "We assume that a node is whatever is
surrounded by an opening tag &mdash; such as `<joke>` &mdash; and its
*corresponding* closing tag"? The notion we just forgot to take into
account is related to that *corresponding* word. It's only intuitive
that *corresponding* has to do with some kind of relationship between
the elements of a grammar and, consequently, some kind of *binding*
between its parsers.

Indeed, grammars with elements depending on each other, like in the
case of our matching opening and closing tags, are called
[Context-sensitive Grammars][context-sensitive-grammar].

A parser for this family of grammars requires a new tool that &mdash;
it could be demonstrated &mdash; cannot be built as a composition of
the applicative parsers we have distilled so far. We need a brand new
mechanism.  
This new tool is indeed pretty simple: we just need an operator
similar to the Applicative Functor's `<*>`, only a bit smarter; a
function able to pass the value successfully parsed by a parser to the
next parser. So, something that could *bind* 2 parsers in a row.  
Not surprisingly, we will call this operator `bind` &mdash; or `>>=`,
because we functional programmers can't get enough of symbols &mdash;
and the resulting notion *monad*.

Implementing it will be super easy &mdash; just a matter of following
the type signature &mdash; but the consequences will be revolutionary.

Curious? Grab a liquorice and jump to [Chapter
13](/monadic-parser-combinators-13): we are going to write it.


# References

* [Garden Path Sentence][garden-path-sentence]
* [semordnilaP][semordnilap]
* [Context-sensitive Grammar][context-sensitive-grammar]

[semordnilap]: https://en.wiktionary.org/wiki/semordnilap
[garden-path-sentence]: https://en.wikipedia.org/wiki/Garden-path_sentence
[context-sensitive-grammar]: https://en.wikipedia.org/wiki/Context-sensitive_grammar


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

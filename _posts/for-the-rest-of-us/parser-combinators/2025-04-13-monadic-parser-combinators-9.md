---
layout: post
title: "Monadic Parser Combinators in F# - Things You Don't Care About"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
include_in_index: false
---
There is another elementary way to sequence 2 parsers: to only return
the parsed value of one and just ignoring the value of the other.

WAT? Why one would possibly wish to do that? What's the point of
parsing something only to discard its value? Well, think about it: I
said "ignoring the parsed value of the other", not "ignoring the
other" altogether. I mean, you will still run the parser you are going
to ignore the result of. It will still fail if it does not like the
input you are giving it. Indeed, it is perfectly legit that, while
processing an input string, you wish something specific to be present,
and not to be interested in getting back its value.

You have already strumbled upon this case. Remember when in [Chapter
2](/monadic-parser-combinators-2) we imagined a possible (exquisite)
syntax for a record:

```
inst Person
   - Id <- *b19b8e87-3d39-4994-8568-0157a978b89a*
   - Name <- <<Richard>>
   - Birthday <- date{16/03/1953}
```

to be parsed into:

```fsharp
type Person =
    { Id: Guid
      Name: string
      Birthday: DateOnly }
```

Focus on the `Id` field. You defined that its GUID value must be
surronded by `*`. You want to make sure that exactly one leading `*`
and one trailing `*` are found, and you want your parser to raise an
error if that's not the case. Yet, in order to build a `Guid`, you are
only interested in capturing the value
`b19b8e87-3d39-4994-8568-0157a978b89a` between the stars. The `*`
values themselves don't contribute to the resulting value.

To build the parser for a GUID you needs to give it this recipe:

- Make sure there is a `*`. Raise an error if you don't find it. If
  you find one, don't even bother store the result, I don't need it.
- Capture an `xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx` string. Do
  remember its value, I do need it!
- Make sure there is another `*`. Again, either raise an error or just go ahead without storing its value.
  

You remember how in [Chapter 6](/monadic-parser-combinators-6) we
imagined to build:

```fsharp
let surroundedBy before after parser = before >>. parser .>> after
```

and how I told you to read `a >>. b` as:

- give me a parser that
- expects whatever the parser `a` expects
- then continues parsing whatever `b` is good at parsing
- and, finally, returns only the thing parsed by `b`, dropping the
result of `a`.


That's exactly the same use case. I guess it's time to build `>>.` and
`.>>`. It won't be hard.

## Keep The Right!
Let's start with `>>.`. Notice the `.` on the right side? It's the
hint that this operator runs both the parsers, but only keeps the
result of the right one. Here's how you could test it:

```fsharp
type Prefix = Prefix
type Content = Content of int

let (>>.) leftP rightP = failwith "Not yet implemented"

[<Fact>]
let ``keep right only`` () =
    let prefixP = str "the prefix/"
    let contentP = str "the content"

    let prefixedP = prefixP >>. contentP

    test <@ run prefixedP "the prefix/the content/the rest" =
        Success("the content", "/the rest") @>
```

If you think about it, the implementation must be very similar to the
one of `.>>.`. Only, rather than returning a tuple with both the
values, you can return the right value only. If you Copy/paste `.>>.`,
you will not struggle to modify it as:


```fsharp
let (>>.) leftP rightP =
    Parser(fun input ->
        let resultL = run leftP input

        match resultL with
        | Failure f -> Failure f
        | Success(_, restL) ->
            let resultR = run rightP restL

            match resultR with
            | Failure f -> Failure f
            | Success(valueR, restR) -> Success(valueR, restR))
```

Test green! Bravo!  

## Composing Parsers-based Functions
Is there a better alternative to this implementation? I argue: every
time that some code is heavily based on copypasta, you can bet your
bottom dollar that there is shorter and better alternative: most of
the times you will win. Let's think about it:

- if `>>.` is like `.>>.`, but returning the second element of
the tuple only,
- and if `snd` is the function returning the second element of a
tuple,
- then `>>.` can be thought as the composition of `.>>.` and `snd`.

Interesting. What does it mean to compose 2 functions each returning a
parser? Quick review how to compose ordinary functions. If you have:

```fsharp
val f : 'a -> 'b
val g : 'b -> 'c
```

then:

```fsharp
val fComposedG : 'a -> 'c

let fComposedG = fun a -> g(f(a))
```

You could conceive an operator for this:


```fsharp
// ('a -> 'b) -> ('b -> 'c) -> ('a -> 'c)
let (>>) f g = fun a -> g(f(a))


[<Fact>]
let ``function composition`` () =
    // string -> (string * int)
    let mkTuple (s: string) = (s, s.Length)

    // (string * int) -> int
    let snd (_, b) = b

    let composed = mkTuple >> snd

    test <@ "abcd" |> composed = 4 @>
```


Indeed, this operator is natively provided by F# ([FSharp.Core/prim-types.fs#L4546][compose-source]):

```fsharp
let inline (>>) func1 func2 x = func2 (func1 x)
```

Let's give it a try, on our Parser-returnign functions:

```fsharp
let (>>.) leftP rightP = 
    leftP .>>. rightP
    >> snd
```

Uhm, no... This does not even compile. Of course it does not! A parser
is not a plain function; it's a function wrapped in a type. You need a
different operator, with this signature:

```fsharp
val combineP : 'a Parser -> ('a -> 'b) -> 'b Parser
```

Well well well, look at that! This is our friend `|>>`, the dual of
Functor's `map` which we developed in [Chapter
7](/monadic-parser-combinators-7)! Let's see if it works:

```fsharp
let (>>.) leftP rightP = 
    leftP .>>. rightP
    |>> snd
```

Indeed, this compiles, and the test is green.  
Wait a minute! Does it mean that using `fst` instead of `snd` we will
obtain `.>>` as well? Let's see:


```fsharp
let (>>.) leftP rightP = 
    leftP .>>. rightP
    |>> fst
    

[<Fact>]
let ``keep left only`` () =
    let contentP = str "the content"
    let suffix = str "/the suffix"

    let prefixedP = contentP .>> suffix

    test <@ run prefixedP "the content/the suffix/the rest" =
        Success("the content", "/the rest") @>
```


Yes! Green! And, by the way: it's again more test code than
implementation. A very good sign!


## Feeling Surronded
I guess you see the pattern here:

- You started writing very low-level building blocks such as `|>>` and
`<<|`.
- Those combinators gave you the chance to encapsulate the structural
traits (passing unconsumed input and handling errors) once for all.
- Now you are building other higher level building blocks just
combining the existing ones, without repeating yourself, ending up
with very concise code.

Let's keep flying in this direction, building on top of `.>>` and
`>>.`: let's invent a combinator for ignoring the elements surronding
something you want to parse. You saw it already in [Chapter
6](/monadic-parser-combinators-6). Its signature is:

```fsharp
val between<'o, 'c, 'v> : 'o Parser -> 'c Parser -> 'v Parser
```

where:

- `'o Parser` detects the opening element.
- `'c Parser` detects the closing element.
- `'v Parser` parses the actual value you are interested in.

This could be used, for example, for parsing a date inside an XML tag:

```fsharp
let between opening closing content = failwith "Not yet implemented"

[<Fact>]
let ``date in tags`` () =

    let o = str "<birthday>"
    let c = str "</birthday>"
    let dateOnlyP = Parser (fun input ->
        Success(DateOnly.Parse(input[..9]), input[10..]))

    let contentInTagsP = dateOnlyP |> between o c

    test <@ run contentInTagsP "<birthday>2025-12-11</birthday>the rest" = 
                Success (DateOnly(2025,12,11), "the rest")@>
```

The implementation is straighforward:

```fsharp
let between opening closing content =
    opening >>. content .>> closing
```

That's it. Green test.  
Note the parameters of `between`: they are not simply the strings you
want to act as boundaries; instead, they are themselves parsers. You
understand what this means: they can be arbitrarily complex. If you
managed to develop a parser for a whole code block and a parser for
comments, you can easily define a parser that detects a comment
between 2 arbitarily complex code blocks:

```fsharp
let commentBetweenBlocksP = commentP |> between codeBlockP codeBlockP
```

It is that easy.  
As an outrageously useless example, here's how to parse a string
surrounded by dates:

```fsharp
[<Fact>]
let ``greeting between dates`` () =

    let helloP = str "Hello!"
    let dateOnlyP = Parser (fun input ->
        Success(DateOnly.Parse(input[..9]), input[10..]))

    let contentInTagsP = helloP |> between dateOnlyP dateOnlyP

    test <@ run contentInTagsP "2025-12-11Hello!2025-12-11 the rest" = Success ("Hello!", " the rest")@>
```

I hope this quirky example doesn't give you any wild ideas for funny
language syntax constructs. Instead, please: take a break, enjoy a
kiwi, and carry on with [Chapter
10](/monadic-parser-combinators-10). I can only recommend not to eat
much and stay light: we are going to apply functions ad nauseam.

[Previous - Here Comes The Tuple](/monadic-parser-combinators-8) ⁓
[Next - Applying Functions, Ad
Nauseam](/monadic-parser-combinators-10)


# References

- [F# source code: `>>` operator][compose-source]

[compose-source]: https://github.com/dotnet/fsharp/blob/main/src/FSharp.Core/prim-types.fs#L4552

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

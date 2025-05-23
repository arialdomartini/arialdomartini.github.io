---
layout: post
title: "Monadic Parser Combinators in F# - Things you don't care about"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
- property-based testing
include_in_index: false
---
There is another elementary way to sequence 2 parsers: to only return
the parsed value of one and just ignoring the parsed value of the
other one.

WAT? Why one would possibly want to do that? What's the point of
parsing something only to discard its value?

Well, think about it. I said "ignoring the parsed value of the other
one", not "ignoring the other one" altogether. I mean, you will still
run the parser you are going to ignore the result of. It will still
fail if it does not like the input you are giving it.  
Indeed, it is possible that while parsing an input string you require
something specific to be present, and yet you are not interested in
getting back its value.

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

Focus on the `Id`. You defined that its GUID value must be surronded
by `*`. Of course you want to make sure that during the parsing
exactly one leading `*` and one trailing `*` are found, and of course
you will raise an error if that's not the case. Yet, in order to build
a `Guid` you are only interested in capturing the value
`b19b8e87-3d39-4994-8568-0157a978b89a` between the stars. The `*`
values themselves are really of little value.

To build the parser for a GUID you needs to give it this recipe:

- Make sure there is a `*`. Raise an error if you don't find it. If
  you find one, that's fine. Don't even bother store the result, I
  don't need it.
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


If you agree that the use case can come in handy, then it's time to
build `>>.` and `.>>`.

## Keep the right!
Let's start with `>>.`. Notice the `.` on the right side? It's the
hint that this operator runs both the parsers, but only keeps the
result of the right one. Here's how you could test it:

```fsharp
type Prefix = Prefix
type Content = Content of int

let (>>.) leftP rightP = failwith "Not yet implemented"

let afterFirstSlash (s:string) = let i = s.IndexOf('/') in s.[i+1..]

[<Fact>]
let ``keep right only`` () =
    let prefixP = Parser (fun input -> Success (input |> afterFirstSlash, Prefix))
    let contentP = Parser (fun input -> Success (input |> afterFirstSlash, Content 42))

    let prefixedP = prefixP >>. contentP

    test <@ run prefixedP "the prefix/the content/something else" = Success ("something else", Content 42) @>
```

If you think about it, the implementation must be very similar to the
one of `.>>.`. Only, rather than returning a tuple with both the
values, you can return the right value only. If you Copy/paste `.>>.`,
you will not struggle to modify it as:


```fsharp
let (>>.) leftP rightP =
    Parser (fun input ->
        let resultL = run leftP input
        match resultL with
        | Failure f -> Failure f
        | Success (restL, valueA) ->
            let resultR = run rightP restL
            match resultR with
            | Failure f -> Failure f
            | Success (restR, valueR) -> Success (restR, valueR))
```

Test green! Bravo!  
Is there a better alternative? I argue: there must be! Every single
time that an implementation is based on copypasta, you can bet your
bottom dollar, thee is shorter and better alternative. Let's think
about it: 

- if `>>.` is like `.>>.`, but returning the second element of
the tuple only,
- and if `snd` is the function returning the second element of a
tuple,
- maybe `>>.` can be thought as the composition of `.>>.` and `snd`.

Quick review how to compose functions. If you have:

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

Let's give it a try:

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

Indeed, this compiles, and the test is green. So, using `fst` instead
of `snd` we should easily obtain `.>>` as well! Let's see:


```fsharp
let (>>.) leftP rightP = 
    leftP .>>. rightP
    |>> fst
    

[<Fact>]
let ``keep left only`` () =
    let contentP = Parser (fun input -> Success (input |> afterFirstSlash, Content 42))
    let suffixP = Parser (fun input -> Success (input |> afterFirstSlash, Prefix))

    let prefixedP = contentP .>> suffixP

    test <@ run prefixedP "the content/the suffix/something else" = Success ("something else", Content 42) @>
```

Indeed. Again: it's more test code than implementation. A very good sign!


## Feeling surronded
I guess you see the pattern here:

- You started writing very low-level building blocks such as `|>>` and
`<<|` &mdash;
- They gave you the chance to encapsulate the structural traits
(passing unconsumed input and handling errors) once for all.
- Now you are building other higher level building blocks just
combining the existing ones, without repeating yourself. And ending up
with super concise code.

Let's keep flying in this direction. Let's build a combinator that
builds on top of `.>>` and `>>.` for ignoring the elements surronding
something you want to parse. Its signature can be:

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

    let o = str "<foobar>"
    let c = str "</foobar>"
    let dateOnlyP = Parser (fun input ->
        Success(input[10..], DateOnly.Parse(input[..9])))

    let contentInTagsP = dateOnlyP |> between o c

    test <@ run contentInTagsP "<foobar>2025-12-11</foobar>the rest" = Success ("the rest", DateOnly(2025,12,11))@>
```

The implementationis is straighforward, once you have `.>>` and `>>.`:

```fsharp
let between opening closing content =
    opening >>. content .>> closing
```

Green test. Nice.  
Notice the parameters of `between`: they are not just the strings you
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
let ``salutation between dates`` () =

    let helloP = str "Hello!"
    let dateOnlyP = Parser (fun input ->
        Success(input[10..], DateOnly.Parse(input[..9])))

    let contentInTagsP = helloP |> between dateOnlyP dateOnlyP

    test <@ run contentInTagsP "2025-12-11Hello!2025-12-11 the rest" = Success (" the rest", "Hello!")@>
```

Don't worry about how basic and fragile the `dateOnlyP` parser and the
other parsers are in these tests: they are mock parsers, created for
testing purposes only. Eventually, you'll learn how to build real
parsers in a robust way, by composing lower-level building blocks.
Just like everything else in FP.


# References

- [F# source code: `>>` operator][compose-source]

[compose-source]: https://github.com/dotnet/fsharp/blob/main/src/FSharp.Core/prim-types.fs#L4552

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

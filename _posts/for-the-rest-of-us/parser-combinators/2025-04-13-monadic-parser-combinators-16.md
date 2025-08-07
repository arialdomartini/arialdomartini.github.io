---
layout: post
title: "Monadic Parser Combinators in F# - A Programmable Semicolon"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
include_in_index: false
---
Monads are beautiful, and so is F#. No wonders that the latter
natively supports the former. There a little trick to extend the F#
syntax to support monadic parser combinators. Indeed, since F#
natively knows how to deal with Monads, via [Computation
Expressions][computation-expression], it's a matter telling it
which Monad implementation to use.  
The implementations of `bind` and `return'` will suffice:

```fsharp
type ParseBuilder() =
    member this.Bind(m, f) = m >>= f
    member this.Return(v) = return' v

let parse = ParseBuilder()
```

Here we go! From now on, whenever F# finds a piece of code inside a
`parser { }` code block, it knows it can rely on the `Parser`'s `bind`
and `return'` implementation. Even better, we won't even need to
explicitly mention `bind` and `return'` in that block: F# will let us
use some very sweet syntactic sugar. For whatever expression like:

```fsharp
parser >>= (fun value -> f value)
```

we can just write:

```fsharp
let! value = parser
f value
```

It helps me to imagine these movements. If you have an expression such
as:


![from a bind invocation to do-notation](static/img/parser-combinators-for-the-rest-of-us/do-notation.png){:width="75%"}

first, you move `value` to the left:
  
![from a bind invocation to do-notation-1](static/img/parser-combinators-for-the-rest-of-us/do-notation-2.png){:width="75%"}

  
That is, instead of using `value` as a lambda parameter, you promote
it to be a simple variable. Just use `let!` instead of `let`. Then,
you move the body of the lambda `doSomethingWith value` to the next
line:

![from a bind invocation to do notation-1](static/img/parser-combinators-for-the-rest-of-us/do-notation-3.png){:width="75%"}

And that's it. You can just remove all the boilerplate syntax
elements, such as the `>>=` operator, `fun`, etc.
    
![from a bind invocation to do notation-1](static/img/parser-combinators-for-the-rest-of-us/do-notation-result.png){:width="75%"}


So, keep in mind this transformation:

```fsharp
parser >>= (fun value -> doSomethingWith value)

let! value = parser
doSomethingWith value
```

Only, remember to wrap the latter in a `parse { }` block.  
I find the first step amusing, because it reminds me that [variables
are syntactic sugar for lambda expressions][variables-sugar].


This style was first introduced Haskell 1.3, under the name of *do
notation* (see [Changes from Haskell 1.2 to Haskell 1.3 - Monad
Syntax][haskell-1.3]). That's often what I call it too, although the
correct name for F# is Computation Expression.

## `map` In Do Notation

Here's `map`, in its initil implementation, from [Chapter
7](/monadic-parser-combinators-7):


```fsharp
let map (f: 'a -> 'b) (aP: 'a Parser) : 'b Parser =
    Parser (fun input ->
        let ar : 'a ParseResult = run aP input
        match ar with
        | Success (rest, a) -> Success (f a, rest)
        | Failure s -> Failure s )
```

Here is it how we defined it, in [Chapter
10](/monadic-parser-combinators-10), in terms of `<*>` and `pure'`:

```fsharp
let map = (<<|)
let map (f: 'a -> 'b) (aP: 'a Parser) : 'b Parser =
    pure' f <*> aP
```

And, finally, the version of [Chapter
15](/monadic-parser-combinators-15), implemented with `bind` and `return'`:

```fsharp
let map (f: 'a -> 'b) (aP: `a Parser) = 
    bind aP (f >> return')
```

If we wanted to use the `parser` computation expression, we could
mechanically apply the syntactic sugar movements listed
before. Otherwise, we could always think to `let!` as a convenient and
almost magic way to put our hands on the parsed value. Either way will
lead us to:


```fsharp
let map f aP = parse {
    let! a = aP
    let b = f a
    return b
}
```

Read it as:

* mapping `f` over the parser `aP`
* generates another parser (`parse { ... }`)
* working like this:
* it takes the value `a` parsed by `aP` (`let! a = aP`)
* and it applies `f` to it (`let b = f a`)
* and this is the value the new parser would return (`return
  b`). Notice that we are directly using the native `return`, not our
  custom `return'`.


Does it work? Let us update the original test we had for `map`:

```fsharp
[<Fact>]
let ``parser-powered function application`` () =
    let twice x = x * 2
    
    let p42: int Parser =
        Parser (fun _ -> Success(42, "rest"))

    let expr = parse {
        let! v = p42
        let twiceTheValue = twice v
        return twiceTheValue
    }

    test <@ run expr "some input" = Success(84, "rest") @>
```

Green.  
It is worth to reflect on the difference between `let!` and `let` in:

* `let! a = aP`
* `let b = f a`

In `let! a = aP`, you basically run the parser `aP` and you get back
the parsed value.  
Instead, the `let` in `let b = f a` is the conventional `let` you have
always used.


Of course, you can make the whole implemenetation a bit shorter:

```fsharp
let map f aP = parse {
    let! a = aP
    return' f a
}
```

As far as I know, when using this style there is no way to obtain a
Point Free style.


## `ap` In Do Notation
Do you remember how we defined the Applicative Functor's `ap` in [Chapter
10](/monadic-parser-combinators-10)?

```fsharp
// ap : ('a -> 'b) Parser -> 'a Parser -> 'b Parser
let ap fP aP = Parser (fun input ->
    match run fP input with
    | Failure e ->  Failure e
    | Success (r, rf) ->
        match run aP rf with
        | Failure s -> Failure s
        | Success (a, ra) -> Success (f a, ra))
```

The 2 parameters of `ap` are both wrapped in a Parser. Indeed, the
implementation revolves around running both parsers to get to the
contained value, then applying the function to the value. We can
really translate this literally, using the `parser` computation
expression:


```fsharp
let ap fP aP =
    parse {
        let! f = fP
        let! a = aP

        return f a
    }
```

Isn't it sweet?

## Do Notation Everywhere
Computation Expressions are particularly effective at capturing the
meaning of a parser and at making the intent clear. In [Chapter
9](/monadic-parser-combinators-9) we defined `between` using `>>.` and
`.>>`:

```fsharp
let between opening closing content =
    opening >>. content .>> closing
```

If you wanted to implement the same in do notation, you could ask
yourself: "What is `between` supposed to do?"

- First, it should parse the opening tag, ignoring the result.
- Then, it should parse the content.
- Then, the closing tag, ignoring the result.
- And, finally, it should return the content.

Well, here is a literal translation:

```fsharp
let between openingP closingP contentP =
    parse {
        let! _ = openingP
        let! content = contentP
        let! _ = closingP

        return content
    }
```

Straighforward and readable, isn't it?

Here's a rewarding exercise to do: to go through all the parser
combinators we have written until this point and to reimplement them
in do notation style. Not only will you find this very easy &mdash;
almost a matter of translating the requirements word by word &mdash;
but likely you will find the resulting expression more eloquent and
expressive.

Here are some examples.

## Old Wine In New Bottles

### `.>>.`
Apply 2 parsers, returning both results in a tuple.  
This is a literal translation:

```fsharp
let (.>>.) aP bP =
    parse {
        let! a = aP
        let! b = bP

        return (a, b)
    }
```

### `.>>`
Apply 2 parsers, returning the result of the first one only.  
Here the trick is to ignore the result of the second parser.

```fsharp
let (.>>) firstP secondP =
    parse {
        let! first = firstP
        let! _ = secondP

        return first
    }
```
      
### `>>.`
Apply 2 parsers, returning the result of the second one only.  
That's trivial! This time we just need to ignore the first result:

```fsharp
// 'a Parser -> 'b Parser -> 'b Parser

let (>>.) firstP secondP = 
    parse {
        let! _ = firstP
        let! second = secondP

        return second
    }
```

### `many`
Repeatedly apply a parser until it fails, returning a list of parsed values.  
An idea could be to implement it as recursive function. We parse a first element, then we rely on recursion to parse the rest of the elements. Returning the result is a matter of building a list:

```fsharp
let rec many1 p =
    parse {
        let! x = p
        let! xs = many1 p
        
        return x :: xs
    }
```

Notice that this is the implementation of `many1`, requiring at least
1 element. `many` is easily obtained combining `many1` with the empty
sequence case, by the use of `<|>`:


```fsharp
let rec many p = 
    many1 p
    <|> (pure' [])
```

Compare this with what we obtained in [Chapter 11](/monadic-parser-combinators-11):

```fsharp
let many<'a> (parser: 'a Parser): 'a list Parser = Parser (fun input ->
    let rec zeroOrMore input =
        match run parser input with
        | Failure _ -> (input, [])
        | Success (result, rest) ->
            match (zeroOrMore rest) with
            | [], rest -> (result :: [], rest)
            | others, rest -> (result :: others, rest)

    Success(zeroOrMore input))
```

and:

```fsharp
let rec many parser = 
    (cons <!> parser <*> (many parser)) <|> (pure' [])
```

With Computation Expression we obtained an astoundingly easier
formulation, don't you think?

### `skipMany`
Parse zero or more occurrences of something, discarding the result.
That's easy! We just need to parse many elements, only to ignore them:

```fsharp
let skipMany p =
    parse {
        let! _ = many p
        return ()
    }
```


### `sepBy`
Parse a list of elements separated by a separator.  
Finally, a more challenging one! Here's a possible implementation. A list of elements separated by a separator is an element followed by many groups "separator + element". We could capture the idea of "separator + element" with a parser on its own, to be used with `many`.

```fsharp
let rec sepBy separator parser =
    let sepThenP =
        parse {
            do! separator
            let! element = parser
            return element
        }

    parse {
        let! first = parser
        let! rest = many sepThenP
        return first :: rest
    }
```

### `lift3`
Elevate a 3-parameter function into the Parsers world.  
It's the combinator with this signature:

```fsharp
val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a Parser -> 'b Parser -> 'c Parser -> 'd Parser
```

This is easy to implement if you interpret the signature as:

Get a 3-parameter function and the 3 arguments, each wrapped in a
parser. Parse each argumement then apply the function to the parsed
values.

```fsharp
let lift3 f aP bP cP =
    parse {
        let! a = aP
        let! b = bP
        let! c = cP
        
        return f a b c
    }
```



## A Programmable Semicolon
`<|>`

# References

- [Computation Expression][computation-expression]
- [Variables are syntactic sugar for lambda expressions][variables-sugar]
- [Changes from Haskell 1.2 to Haskell 1.3 - Monad Syntax][haskell-1.3]

[computation-expression]: https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions
[variables-sugar]: https://arialdomartini.github.io/sicp-let-syntactic-sugar-csharp
[haskell-1.3]: https://www.haskell.org/definition/from12to13.html#do

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

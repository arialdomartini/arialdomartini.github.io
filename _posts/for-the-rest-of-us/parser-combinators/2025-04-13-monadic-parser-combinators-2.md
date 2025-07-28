---
layout: post
title: "Monadic Parser Combinators in F# - 5 Shades Of Composability"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
include_in_index: false
---
You have surely noted that people into functional programming have an
obsession with the notion of composition. For example, you could have
stumbled into sentences such as:

- Exceptions don't compose well.
- Locks are bad because they don't compose.
- Monads compose nicely.

and the like.

This installment will try to help you develop an intuition about what
composition is, why you want your parsers to be *composable* and what
Applicative Functors and Monads have to do with all of this.

## What's The Fuss About Composition?
You want your esoteric language to support a new groundbreaking
serialization format, surely destined to eradicate JSON and YAML. It
will let developers express a record as:

```
inst Person
   - Id <- *b19b8e87-3d39-4994-8568-0157a978b89a*
   - Name <- <<Richard>>
   - Birthday <- date{16/03/1953}
```

That's what I call a gorgeous syntax!  
To deserialize such a string into the F# record:

```fsharp
type Person =
    { Id: Guid
      Name: string
      Birthday: DateOnly }
```

you need a parsing function of type:


```fsharp
val parsePerson: string -> Person
```

Let's reflect how to implement it. Again: we are more interested in
the journey of *decomposing* the problem into smaller problems and
then of *combining* them to generate a parser, rather than in writing
this specific parser by hand.  
`parsePerson` must address parsing the syntax specific to a record
structure, such as that initial `inst` keyword and the series of `- fieldName <- value` strings.  
As for the field values, though, `parsePerson` could smartly delegate
parsing of GUIDs, strings and dates to some specialized
sub-functions. Each would have a specific signature:

```fsharp
val parseGuid: string -> Guid
val parseString: string -> string
val parseDateOnly: string -> DateOnly
```

`parseGuid` knows how to get a GUID value from a string surrounded by
`*`, `parseString` would extract strings from texts surronded by `<<`
and `>>`, and so on. `parsePerson` will not need to worry about those
details, so the skeleton of its implementation can be something like:


```fsharp
open System
open Xunit
open Swensen.Unquote

let inline __<'T> : 'T = failwith "Not implemented yet"

type Person =
    { Id: Guid
      Name: string
      Birthday: DateOnly }


let parseGuid: string -> Guid = __
let parseString: string -> string = __
let parseDateOnly: string -> DateOnly = __

let parsePerson: string -> Person =
    fun input ->
        let parseRecordStructure: string -> string * string * string = __

        let guidPart, namePart, birthdayPart = parseRecordStructure input

        { Id = parseGuid guidPart
          Name = parseString namePart
          Birthday = parseDateOnly birthdayPart }


[<Fact(Skip = "incomplete example")>]
let ``it parses a Person`` () =

    let input =
        """inst Person
   - Id <- *b19b8e87-3d39-4994-8568-0157a978b89a*
   - Name <- <<Richard>>
   - Birthday <- date{16/03/1953}
"""

    let expected =
        { Id = Guid.Parse("b19b8e87-3d39-4994-8568-0157a978b89a")
          Name = "Richard"
          Birthday = DateOnly(1953, 03, 16) }

    test <@ parsePerson input = expected @>
```

In other words, `parsePerson` is a *composition* of:

- some logic specific to the syntax of a record.
- and some lower level parsers.

Is this what functional programmers mean with *composition*? Well,
kind of. It's less black and white than this.


## 5 Shades Of Composability
First of all, there is no clear consensus about what "to compose well"
means. Search for "*monads are composable*" and "*monads don't
compose*": you will find plenty of articles supporting either the
claims.

I like to think that the line separating *composable* and
*non-composable* is blurry. Given 2 instances of `X`, whatever `X` is,
you can either have that:

1. They just **cannot** be combined together.
2. They **can** be combined together, but the result is **not an `X`**
   anymore.
3. They **can** be combined and they even **form another `X`**; but
  the result might **behave differently** from expected.
4. They **can** be combined together to **form another `X`**, **100%
preserving** all the expected properties. But combining them is
**hard** and not scalable.
5. They **can** be combined together to form **another `X`**, **100%
  preserving** all the expected properties. And combining them is
  **easy** (and *elegant*, for some definition of *elegant*).


If you will, you can see these levels as follows:


| Level | They can be combined | forming another `X` | preserving their properties | It is easy |
|:-----:|:--------------------:|:-------------------:|:---------------------------:|:----------:|
| 1     | No                   | -                   | -                           | -          |
| 2     | Yes                  | No                  | -                           | -          |
| 3     | Yes                  | Yes                 | No                          | -          |
| 4     | Yes                  | Yes                 | Yes                         | No         |
| 5     | Yes                  | Yes                 | Yes                         | Yes        |

Of course, for your esoteric language and your serialization format,
you aim to write parsers proudly fitting the last level.

To clarify each level, let me give you some examples.

### Case 1: things that do not compose

Surprisingly, the building blocks of most programming languages just
don't compose.

Take expressions and statements, for example. Expressions can be
composed via operators (like in `a * b` and `list1 ++ list2`);
statements can be composed sequencing them, like in:

```fsharp
use writer = new StreamWriter(filename)
writer.WriteLine("Hello, world!")
```

possibly in combination of control flow structures such as `if`, `for` and `while`.  
However, this creates asymmetry. Control structures like `if` can use
expressions:

```fsharp
if(condition) { ...  }
```

`if`, a statement, gets `condition`, an expression.  
The opposite is not true. Expressions can't use control
structures. This:

```c
int myList = for(int i=0; i<10; i++) { ... };
```

does not even compile.

Similarly, you can pass the expression `sqrt(42)` as an argument to a
function. You cannot pass a `for` statement as an argument to a
function. This just doesn't make sense, right?  
So, in a sense, "expressions and statements don't compose".

By the way, that's one of the appealing traits of some functional
languages: they treat control structures as first-class objects,
unifying the 2 worlds. They offer greater composability by allowing
control logic to be manipulated just like any other value. For
example, this is valid F# code:

```fsharp
let squares = [for x in 1..10 do yield x*x]
```

### Case 2: composing `X`s results in something other than `X`.

Or, more concisely: some things are not closed under composition.  
The canonical example is with integer numbers: they compose via
division, but they result in float numbers.

Objects are another notable case. You can compose `Wheel` and
`Engine`, but you want the result to be `Car`, not something that is
both a `Wheel` and an `Engine`.

### Case 3: Things that compose in surprising ways
The canonical example is again with numbers. In many languages'
floating-point arithmetic: `0.1 + 0.2` computes to
`0.30000000000000004`, not exactly to `0.3`. You can say that float
numbers compose via the sum operation, but not so nicely.

Possibly, another more interesting example is with multi-threading
functions using locks. They *do compose*, but in a surprising and
unsafe way. Imagine that you have the guarantee that every process
requesting locks eventually releases them. Given that you can count on
this property for every process in isolation, does the composition of
2 processes hold the same guarantee?  
Unfortunately, no. Consider 2 functions acquiring 2 locks `x` and `y`,
in opposite order:

```fsharp
open System.Threading
open System.Threading.Tasks
open Xunit
open Swensen.Unquote

let x = obj ()
let y = obj ()

let threadA =
    async {
        return
            lock x (fun () ->
                Thread.Sleep(1000)

                lock y (fun () -> 21))
    }

let threadB =
    async {
        return
            lock y (fun () ->
                Thread.Sleep(1000)

                lock x (fun () -> 21))
    }

let combined () =
    task {
        let taskA = Async.StartAsTask threadA
        let taskB = Async.StartAsTask threadB

        let! a = taskA
        let! b = taskB

        return a + b
    }

[<Fact>]
let ``threadA only`` () =
    task {
        let! b = threadA
        test <@ b = 21 @>
    }

[<Fact>]
let ``threadB only`` () =
    task {
        let! a = threadB
        test <@ a = 21 @>
    }


[<Fact(Skip = "Never terminates because of a deadlock")>]
let ``thread A and B combined cause a deadlock`` () =
    task {
        let! ab = combined ()
        test <@ ab = 42 @>
    }
```

Although when run separately each async function is guaranteed to
successfully return, their combination might generate a deadlock. So,
function using locks do compose into other functions using locks, but
*not nicely*: you cannot guarantee all the invariants still hold.

### What about our manual parser?
Getting back to our fictional Parser:

```fsharp
let parsePerson: string -> Person =
    fun input ->
        let parseRecordStructure: string -> string * string * string = __

        let guidPart, namePart, birthdayPart = parseRecordStructure input

        { Id = parseGuid guidPart
          Name = parseString namePart
          Birthday = parseDateOnly birthdayPart }
```


in which slot does it &mdash; and other similarly written parsers
&mdash; fall?

I hope that the next installment will manage to convince you that it's
a case for the 4th level: indeed, imperative parsers like this *do
compose*, and mostly without unexpected suprises. But the code you
need to write would not scale. It will easily explode from convoluted
to crazy unmaintanable.

Did I already tell you that by moving to Applicative and Monadic
Parser Combinators you will reach the 5th level, the complete zen
illumination and probably a couple of other super-powers?

OK, let's have a break here. You deserve a hot infusion and some
relax. When ready, jump to [the next
chapter](/monadic-parser-combinators-3).

[Previous - Intro](/monadic-parser-combinators) ⁓ [Next -
That's a Combinator!](/monadic-parser-combinators-3)


# References


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

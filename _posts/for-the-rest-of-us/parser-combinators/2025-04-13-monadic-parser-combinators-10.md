---
layout: post
title: "Monadic Parser Combinators in F# - Applying functions, ad nauseam"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
- property-based testing
include_in_index: false
---
There is something magic about the native F# function application:
once you apply a function to an argument, if the result is another
function you can just run function application again. It will work.
And if you get another function, you can do the same, ad nauseam.  
See this example:

```fsharp
[<Fact>]
let ``function application with 2 parameters`` () =

    let fa: int  -> (bool -> string) = fun a -> fun b -> $"{a}, {b}"
    let fb:          bool -> string  = fa 42
    let c:                   string  = fb true

    test <@ c = "42, True" @>
```

- `f` is a function that takes `a`,  an `int`, and that returns
  another function, `bool -> string`.
- If you use the native function application to apply `f` to `42`, you
  get that `bool -> string` function back.
- Now, thare's nothing special in that `bool -> string` function. So,
  you can keep using the F# native function application to pass it the
  next argument, a `true` value.
  
It's easy to see how this does not stop with 2-parameter functions.
Here's a test for a 3-parameter function:

```fsharp
[<Fact>]
let ``function application with 3 parameters`` () =

    let fa: int  -> (bool -> (string -> string)) = fun a -> fun b -> fun c -> $"{a}, {b}, {c}"
    let fb:          bool -> (string -> string)  = fa 42
    let fc:                   string -> string = fb true
    let d:                              string = fc "foobar"

    test <@ d = "42, True, foobar" @>
```


Now:

```fsharp
let f:  int  -> (bool -> (string -> string)) = fun a -> fun b -> fun c
-> $"{a}, {b}, {c}"
```

is a very verbose way to define a function returning a function, in
turn, returning a function. F# lets you:

- remove the parenthesis from the signature, since function
  application associates to the right:


```fsharp
let f:  int  -> bool -> string -> string = fun a -> fun b -> fun c
-> $"{a}, {b}, {c}"
```

- skip that `fun a -> fun b -> fun c` boiler plate and just write `f a b
c`, since all the functions are automatically curried:


```fsharp
let f a b c = $"{a}, {b}, {c}"
```

- mentally see this as a function of 3 parameters.

- apply it to multiple parameters in a single shot, with:

```fsharp
let d = f 42 true "foobar"
```

Ah, much better! But, notice: this is somehow just syntactic sugar.
This is still a function returning a function &mdash; in turn,
returning a function.

## A crocked Function Application
What about the Parser-Powered Function Application `<<|` that we have
so proudly distilled in [Chapter 7](/monadic-parser-combinators-7)?
Can we also apply it repeatedly?

Let's see. We take a generic 3-parameter function `'a -> 'b -> 'c ->
'd`. In this context, we are not concerned how it is implemented, we
can just focus on the signature:


```fsharp

[<Fact>]
let ``Parser-powered function application with 3 parameters`` () =

    let f (a: int) (b: 'b) (c: 'c) = 
        failwith "Not yet implemented"
    ...

```

Then, we apply it to an argument `'a Parser`, of course using `map`.
Let me use the symbol `<!>` instead of `<<|` or `map`; we mentioned
that they are all synonyms:

```fsharp
let (<!>) = map

[<Fact>]
let ``Parser-powered function application with 3 parameters`` () =

    let a: 'a Parser = failwith "Not yet implemented"

    let f (a: int) (b: 'b) (c: 'c) =
        failwith "Not yet implemented"
    
    let fa: ('b -> 'c -> 'd) Parser = f <!> a
    
    ...
```

Oh no! Look the signature of `fa`! The result is not just another
function with 1 parameter less, like it happened before . It's not
even a function anymore: it's a function wrapped inside a Parser. This
means that we cannot apply `<!>` again... We definitely need a
different operator.

If `map` is of little help, it's time to invent a more powerful
version of Functors: enter Applicative Functors.










# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

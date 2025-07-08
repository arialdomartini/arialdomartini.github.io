---
layout: post
title: "Monadic Parser Combinators in F# - Two Shades of Coupling"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- fsharp
- functional programming
include_in_index: false
---

## A tale of 2 coupling types 

`parsePerson` delegates the parsing of GUIDs, strings and dates to
external functions. We think that this makes it decoupled from the
parsing logic of the specif fields. And this is so true.

Yet, the code we just obtained clearly shows that some problems still
exist. You should have grasped why by now, and realized that there are
in fact 2 types of coupling:

- A function can be coupled with *the logic* of other components.  
This cannot be our case: `parsePerson` does not even know how GUIDs
are represented; this logic is completely delegated to `parseGuid`.

- A function could be coupled with *the mechanics* of glueing things
  together, what we called the *effectful logic*.  
  This means that even if it is *functionally isolated*, the code
  structure still depends on this *glueing mechanism*. This must be
  our case.
  

Now, if you are not into functional programming, it is likely that you
never heard of the second form of coupling. After all, in OOP
"*glueing things together*" is rarely a big deal. In OOP there are a
few of techniques for gluing things together &mdash; such as sending
messages to objects in a sequence, or passing values around or
applying values to functions &mdash; and all of them are natively
implemented by the programming language. And all boil down to the
notion of *function application*.

## Dumb and smart Function Applications

The native function application works just fine as long as we operate
within the simple case of things with compatible signatures:

```fsharp
f : 'a -> 'b
g : 'b -> 'c
```

Languages natively know how to glue `f` with `g` because the output of
`f` can be passed, just as it is, to `g`.

This leads to 2 traits in the OOP's function application:

- We rarely have to worry about it.  
  Even more: we intentionally design our methods so their signature
  makes the compiler happy. When things have incompatible signatures,
  we write wrappers and adapters to work around the incompatibility.

- We often assume function application is dumb.  
  It just passes a value to the next function, doing nothing else
  meanwhile, and we are happy with that. There are few exceptions:
  indeed, design patterns like Decorator and Aspect Oriented
  Programming are a way to add some logic to method calls.


The farest we go with making things intentionally incompatible and
with adding new functionalities to function application, in OOP, is
with Async calls:


```fsharp
f : 'a -> Task<'b>
g : 'b -> Task<'c>
```

Those functions just don't combine natively. We dare to go this
direction only because it is an easy problem to solve: the language
reserves a special treatment to this case, providing us with the
dedicated keywords `async` and `await`.

In a sense, exceptions are also an example of this. If your language
did not implement exceptions, you would need to handle errors like Go
does:

- Checking every and each call for returned errors.
- Propagating the error upstream.
- Passing the call stack too.

etc.

Your domain code would be horribly polluted by this error handling stuff.  
A way out of this could be to extend the native function application
so that, other than just passing a value from a function to the next
one, it would *also* tackle the error handling responsibility.
Exceptions are so convenient to use only because the native function
application does all of this, under the hood.

## Breaking the rules

But both exceptions and `async`/`await` are ad-hoc, built-in
solutions. We cannot expect that the native F# function application
provided a special treatment for parser functions returning `Result`s
of tuples. This is too specific to our peculiar use case.  

In fact, in FP it's often the case that we intentionally design the
function signatures ignoring the native gluing mechanism. We take the
freedom to design functions that don't fit together because function
application is easy to extend. And because this gives us the chance to
put some custom logic in the gluing mechanism.

As an FP programmer you don't settle with the dumb native function
application. You want fancier ones: you want them to log each call; or
to deal with errors via a `Result` instance, as in our case. Or to do
some combinatorial calculation. I'm using the plural, here, because
really, you want a family of function applications, one for each of
your specific use case.  

FP techniques provide a way more generic solution than special
keywords like `async` and `await`.  If you read [Monads for The Rest
of Us](/monads-for-the-rest-of-us), the notion of Applicative Functors
and Monads as an extension of function application should not be new
to you.

Here's the takeaway: if in OOP the signature incompatibility is *a
problem* to be avoided or to be solved by the means of wrappers and
adapters, in FP the same incompatibility is *a design tool* to be
leveraged.

So, let's see how to fix the pyramid of doom we wrote in `parsePerson`
by distilling a new function application. And let's see how this leads
us to re-invent &mdash; yet another time &mdash; monads.

Take a break, bite an apple, then jump to [the next installment](/monadic-parser-combinators-6).

# References


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/33)



{% include fp-newsletter.html %}

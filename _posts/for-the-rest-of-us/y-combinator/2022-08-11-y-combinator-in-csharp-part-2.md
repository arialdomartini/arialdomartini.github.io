---
layout: post
title: "Deriving the Y Combinator in C# - Part 2 - The code problem"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
include_in_index: false
tags:
- C#
- Functional Programming
- Lambda Calculus
---
## Index
* [Part 1 - Recursive anonymous functions][part-1]
* **Part 2 - The code problem**
* [Part 3 - A recursive Y Combinator][part-3]
* [Part 4 - Non-recursive Y Combinator][part-4]


# The Y Combinator in C#
In the [previous installment](y-combinator-in-csharp) we got stucked with a hideous:

```csharp
var result = new List<int>{1, 2, 3}.Select(n => n == 0 ? 0 : n + ???(n - 1));
```


The smart C# programmer will observe that the undefined `???` can be seen as a [continuation][continuation], and that nothing prevents us from providing the anonymous lambda with an extra parameter, with which to feed the proper continuation:

```csharp
var quasi_sum = 
    (continuation, n) =>
        n == 0 ? 0 : n + continuation(n - 1)
```

or, in a [curried form][currying]:

```csharp
var quasi_sum =
    continuation => 
        n =>
            n == 0 ? 0 : n + continuation(n - 1)
```

This function would happily calculate the sum, if only it is given the proper continuation. In other world, generating the actual function `sum` is a matter of invoking:

```csharp
var sum = quasi_sum( proper_continuation );
```


Neither `sum` nor `quasi_sum` are recursive.

We have a couple of little problems here.<br/>
First: `proper_continuation` is still undefined.<br/>
Second, `quasi_sum` does not even compile: the poor C#'s type inference is not able to workout the type. We need to give the compiler a hand:

```csharp
Func<Func<int, int>, Func<int, int>> quasi_sum =
    continuation => 
        n =>
            n == 0 ? 0 : n + continuation(n - 1)
```

Ok, now it compiles. But, wow, that's a mouthful.<br/> If only C# supported type aliases we could write:

```csharp
type Sum = Func<int, int>;

Func<Sum, Sum> quasi_sum =
    continuation => 
        n =>
            n == 0 ? 0 : n + continuation(n - 1)

```

Much better.<br/>
Actually, we can partially simulate type aliasing with delegates:

```csharp
delegate int Sum(int n);

Func<Sum, Sum> quasi_sum =
    continuation => 
        n =>
            n == 0 ? 0 : n + continuation(n - 1)

```

Cool. This `quasi_sum` is kind of a sum-function maker: when it is given a proper continuation and a non-recursive `sum` implemented in [Continuation Passing Style][continuation-passing-style], it generates the final `sum` function.


## Feeding itself with itself
Problem solved?<br/>
Not really: we have just shifted it. Now we are stuck with the question what the right continuation is.

The ideal continuation must be a function able to continue the sum calulation. So, a function equivalent to `sum` itself. That is, exactly the function  `quasi_sum` is supposed to create.<br/>
This gives the smart reader the idea of feeding `quasi_sum` with itself. Which, in turn, screams recursion.

That's the basic idea to build on top of. Sounds more a dog chasing its tail than a solution, an egg and chicken problem variant, right?

If you feel confused, don't desperate: untangling this knot is what makes this problem fun. We will get soon to the solution. But first, we need to add a last little layer of abstraction.

## What's this Y Combinator, anyway?
Let's imagine a function `Y` which, given the `quasi_sum`, is able to generate `sum`:

```csharp
var Y = ???

Func<Sum, Sum> quasi_sum =
    continuation => 
        n =>
            n == 0 ? 0 : n + continuation(n - 1)

Sum sum = Y(quasi_sum);
```

Its type is:

```csharp
Func<Func<Sum, Sum>, Sum>
```

which expanded becomes:

```csharp
Func<Func<Func<int, int>, Func<int, int>>, Func<int, int>>
```

making clear why we decided to have a type alias.


This imaginary function would save us from the necessity of finding the *right continuation* to feed `quasi_sum` with. Internally, it will take care of feeding `quasi_sum` with its own result.

This function is all but imaginry: it does exist, and it is called Fixed Point Combinator or (surprise, surprise) Y Combinator. Deriving it is a bit challenging, and its implementation is kind of obscure:

```csharp
private static Sum Y(Func<Sum, Sum> f) =>
    new Func<Rec, Sum>(f =>
        f(f))
    (self =>
        n =>
            f(self(self))(n));
```

But don't despair: deriving it is not that hard, after all. In the [next installment][part-3] we will first warm up deriving a Y Combinator which is itself recursive. [Next][part-4], we will distill a more sophisticated Y Combinator, making it non-recursive.

[Let's go][part-3].

<hr/>

References:

* [Continuation - Wikipedia][continuation]
* [Continuation Passing Style][continuation-passing-style]
* [Curried functions: optimized for partial application - Functional Programming in C# - Enrico Buonanno][currying]

[continuation]: https://en.wikipedia.org/wiki/Continuation
[continuation-passing-style]: https://en.wikipedia.org/wiki/Continuation-passing_style
[currying]: https://livebook.manning.com/#!/book/functional-programming-in-c-sharp/chapter-7/ch07lev1sec3

[part-1]: y-combinator-in-csharp
[part-3]: y-combinator-in-csharp-part-3
[part-4]: y-combinator-in-csharp-part-4

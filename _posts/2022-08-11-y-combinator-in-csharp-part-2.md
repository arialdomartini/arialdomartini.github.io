---
layout: post
title: "Implementing the Y Combinator in C# - Part 2 - The code problem"
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
* Part 4 - Non-recursive Y Combinator


# The Y Combinator in C#
In the [previous installment](y-combinator-in-csharp) we got stucked with a hideous:

{% highlight csharp %}
var result = new List<int>{1, 2, 3}.Select(n => n == 0 ? 0 : n + ???(n - 1));
{% endhighlight %}


The smart C# programmer will observe that the undefined `???` can be seen as a [continuation][continuation], and that nothing prevents us from providing the anonymous lambda with an extra parameter, with which to feed the proper continuation:

{% highlight csharp %}
var quasi_sum = 
    (continuation, n) =>
        n == 0 ? 0 : n + continuation(n - 1)
{% endhighlight %}

or, in a curried form:

{% highlight csharp %}
var quasi_sum =
    continuation => 
        n =>
            n == 0 ? 0 : n + continuation(n - 1)
{% endhighlight %}

This function would happily calculate the sum, if only it is given the proper continuation. In other world, generating the actual function `sum` is a matter of invoking:

{% highlight csharp %}
var sum = quasi_sum( proper_continuation );
{% endhighlight %}


Neither `sum` nor `quasi_sum` are recursive.<br/>
Don't try to compile `quasi_sum`, though: C# would complain that it is not able to infer its type. We need to give the compiler a hand:

{% highlight csharp %}
Func<Func<int, int>, Func<int, int>> quasi_sum =
    continuation => 
        n =>
            n == 0 ? 0 : n + continuation(n - 1)
{% endhighlight %}

Wow. That's a mouthful.<br/> If only C# supported type aliases we could write:

{% highlight csharp %}
type Sum = Func<int, int>;

Func<Sum, Sum> quasi_sum =
    continuation => 
        n =>
            n == 0 ? 0 : n + continuation(n - 1)

{% endhighlight %}

Much better.<br/>
Actually, we can simulate (some) type aliases with delegates:

{% highlight csharp %}
delegate int Sum(int n);

Func<Sum, Sum> quasi_sum =
    continuation => 
        n =>
            n == 0 ? 0 : n + continuation(n - 1)

{% endhighlight %}

Cool. This `quasi_sum` is kind of a sum-function maker: when it is given a proper continuation and a non-recursive `sum` implemented in [Continuation Passing Style][continuation-passing-style], it generates the final `sum` function.


## Feeding itself with itself
Problem solved?<br/>
Not really: we have just shifted it. Now we are stuck with the question what the right continuation is.<br/>
The ideal continuation must be a function able to continue the sum calulation. So, a function equivalent to `sum` itself. That is, exactly the function  `quasi_sum` is supposed to create.<br/>
This gives the smart reader the idea of feeding `quasi_sum` with itself. Which, in turn, screams recursion.

That's the basic idea to build on top of. Sounds more a dog chasing its tail than a solution, an egg and chicken problem variant, right?

If you feel confused, don't desperate: untangling this knot is what makes this problem fun. We will get soon to the solution. But first, we need to add a last little layer of abstraction.

## What's this Y Combinator, anyway?
Let's imagine a function `Y` which, given the `quasi_sum`, is able to generate `sum`:

{% highlight csharp %}
var Y = ???

Func<Sum, Sum> quasi_sum =
    continuation => 
        n =>
            n == 0 ? 0 : n + continuation(n - 1)

Sum sum = Y(quasi_sum);
{% endhighlight %}

Its type is:

{% highlight csharp %}
Func<Func<Sum, Sum>, Sum>
{% endhighlight %}

which expanded becomes:

{% highlight csharp %}
Func<Func<Func<int, int>, Func<int, int>>, Func<int, int>>
{% endhighlight %}

making clear why we decided to have a type alias.


This imaginary function would save us from the necessity of finding the *right continuation* to feed `quasi_sum` with. Internally, it will take care of feeding `quasi_sum` with its own result.

This function is all but imaginry: it does exist, and it is called Fixed Point Combinator or (surprise, surprise) Y Combinator. Deriving it is a bit challenging, and its implementation (without type aliases) is kind of obscure:

{% highlight csharp %}
Func<Func<Func<int, int>, Func<int, int>>, Func<int, int>> Y = 
    f => n => new Func<Func<Func<int, int>, Func<int, int>>, Func<int, int>>
                (p => p(p))( self => f(i => self(self)(i)))(n);
{% endhighlight %}

But don't despair: deriving it is not that hard, after all. In the [next installment][part-3] we will first warm up deriving a Y Combinator which is itself recursive. Next, we will distill a more sophisticated non-recursive Y Combinator.

[Let's go][part-3].


References:

* [Continuation - Wikipedia][continuation]
* [Continuation Passing Style][continuation-passing-style]


[continuation]: https://en.wikipedia.org/wiki/Continuation
[continuation-passing-style]: https://en.wikipedia.org/wiki/Continuation-passing_style

[part-1]: y-combinator-in-csharp
[part-3]: y-combinator-in-csharp-part-3

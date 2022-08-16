---
layout: post
title: "Implementing the Y Combinator in C#"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- C#
- Functional Programming
- Lambda Calculus
---
![A tattoo with the Y Combinator](static/img/y-combinator/y-combinator.jpg)

**What the heck is that?** That's the Y Combinator!<br/>
**Uh?** A higher-order function with which anonymous lambdas can be made recursive.<br/>
**Sounds useful, I guess?** No, it's not. It's a purely intellectual challenge.<br/>
**Any practical application for my C# developer daily life?** Not at all.<br/>
**So, what's the point, why?** Because it's fun.


<!--more-->
<hr/>
## Index
This a 4 parts article. You are reading the first installment.

* **Part 1 - Recursive anonymous functions**
* [Part 2 - The code problem][part-2]
* [Part 3 - A recursive Y Combinator][part-3]
* Part 4 - Non-recursive Y Combinator

## Recursive named functions
Consider a generic 1-parameter, recursive function. Let's use for example `sum(n)`, that returns the sum of all numbers between `0` and `n`. Its implementation is trivial.

{% highlight csharp %}
static int Sum(int n) =>
    n == 0 ? 0 : n + Sum(n - 1);
{% endhighlight %}

C# decently supports Functional Programming, so `Sum` can be passed as an argumento to high-order functions, even in [point-free style][point-free], like in:

{% highlight csharp %}
var result = new [] { 0, 1, 2, 3, 4 }.Select(Sum);

result.Should().BeEquivalentTo(new [] { 0, 1, 3, 6, 10 });
{% endhighlight %}

So far so good.

## Recursive anonymous functions
Here comes a puzzling quiz: can we inline `Sum`? That is, can we replace `Sum` in `Select` with its own definition, as an anonymous lambda?

**Spoiler**: No, we can't. If we try, we will miserably fail ending up with a disappointing:


{% highlight csharp %}
var result = new []{0, 1, 2, 3, 4}.Select(n => n == 0 ? 0 : n + ???(n - 1));
{% endhighlight %}


What should the value of `???` be?<br/>
The fact is, a function can invoke itself only by name. No name, no party. It seems that there are no chances for anonymous lambdas to be recursive.

Or are there?

## The Y Combinator
It might not seem immediately obvious, but this problem has to do with [Fixed Points][fixed-point] and [λ-calculus][lambda-calculus]. Its solution was invented ([or discovered][propositions-as-types]?) back in the 1940s, way before C# was a thing, by [Haskell Curry][haskell-curry].<br/>

Haskell Currry is the man the [Haskell][haskell] and the [Curry][curry] functional programming languages and the [currying][currying] coding techniques are named after. And indeed, although a [mathematical concept][y-combinator-ncatlab], the Y Combinator is also a topic that can be purely tackled with code, no maths involved.

And that's exactly what we are going to do in the [second installment][part-2]

<hr />

References:

* [Fixed Point - Wikipedia][fixed-point]
* [Lambda Calculus - Wikipedia][lambda-calculus]
* [Haskell Curry - Wikipedia][haskell-curry]
* [Haskell (Programming Language) - Wikipedia][haskell]
* [Curry (Programming Language) - Wikipedia][curry]
* [Currying - Wikipedia][currying]
* [Y Combinator - nLab][y-combinator-ncatlab]
* [Point-free or Tacit Programming - Wikipedia][point-free]
* ["Propositions as Types" by Philip Wadler][propositions-as-types]


[fixed-point]: https://en.wikipedia.org/wiki/Fixed_point_%28mathematics%29
[lambda-calculus]: https://en.wikipedia.org/wiki/Lambda_calculus
[haskell-curry]: https://en.wikipedia.org/wiki/Haskell_Curry
[haskell]: https://en.wikipedia.org/wiki/Haskell
[Curry]: https://en.wikipedia.org/wiki/Curry_(programming_language)
[currying]: https://en.wikipedia.org/wiki/Currying
[y-combinator-ncatlab]: https://ncatlab.org/nlab/show/fixed-point+combinator
[point-free]: https://en.wikipedia.org/wiki/Tacit_programming
[propositions-as-types]: https://www.youtube.com/watch?v=IOiZatlZtGU

[part-2]: y-combinator-in-csharp-part-2
[part-3]: y-combinator-in-csharp-part-3

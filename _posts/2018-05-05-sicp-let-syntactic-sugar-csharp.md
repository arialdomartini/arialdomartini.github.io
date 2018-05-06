---
layout: post
title: Variables are syntatic sugar for lambda expressions (C#)
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- SICP
- Lisp
- C#
---

While reading [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sites/default/files/sicp/index.html), I got hit by this sentence (p. 65):

> "A `let` expression is simply syntatic sugar for the underlying lambda expression"

It claims that variables definition is not a necessary language feature, and that it has been introduced only as a convenient alternative to lambda expressions.

WAT? Seriously?

<!--more-->

[Scala version](sicp-let-syntactic-sugar-scala.html) \|  **C# version**

## Lisp's `let`

To provide you with a context: the sentence refers to Lisp's `let`; you can think to `let` as the equivalent of C#'s `var` for variables initialization.<br />
Lisp's:

```
(let ((x 10)) ...
```

has more or less the same meaning of C#'s:

```
var x = 10;
```

That's not strictly true, but bear with me.<br />
Also, the same book defines "*syntatic sugar*" as: 

> "convenient alternative surface structures for things that can be written in more uniform ways"

It should be clearer now why the sentence:

> "A let expression is simply syntatic sugar for the underlying lambda expression."

if applied to C#, really would mean that the reserved word `var` wouldn't be a strictly necessary language construct, as a variable initialization could be easily replaced by lambdas.<br />
Although the explaination provided by the book revolves around Scheme, a Lisp dialect, and might not necessarily be valid with other programming languages, I found it fun enough to translate it to Scala and C#.<br />

## Guess yourself before reading
What variable initialization and lambda expressions have in common? How could one replace a variable with a lambda? They are so distant topics to me that I don't see how one could be the syntatic sugar of the other.

I've got a suggestion: before reading the next paragraph, challenge yourself, and try to get an answer to the question: how could you replace local variables with lambda expressions, without using any `var`?

# From an algebraic function to a lambda expression

SICP starts from this algebraic function:

![algebraic function](static/img/sicp-let/algebraic-function.png)

You can easily translate it to C# as:

{% highlight csharp %}
static int F(int x, int y)
{
    return x * (1 + x * y) + y * (1 - y) + (1 + x * y) * (1 - y);
}
{% endhighlight %}


Since `1 + x * y` and `1 - y` are repeated twice, you could write the same function as:

![algebraic function simplified](static/img/sicp-let/algebraic-function-simplified.png)

where `a` and `b` are defined as:

![value of a](static/img/sicp-let/a.png)<br />
![value of b](static/img/sicp-let/b.png)
<br />

In C#, this could be implemented introducing an auxiliary function `Helper`:

{% highlight csharp %}
static int Helper(int x, int y, int a, int b)
{
    return x * a + y * b + a * b;
}

static int F(int x, int y)
{
    return Helper(x, y, 1 + x * y, 1 - y);
}
{% endhighlight %}


Notice how the auxiliary function's body is the function in the desired, simplified form, while the value of `a` and `b` is provided by `F`.<br />
Also notice that, since the auxiliary function `Helper` is only used by `F`, it could be defined as a local function, i.e. as a nested function:

{% highlight csharp %}
static int F(int x, int y)
{
    int Helper(int a, int b)
    {
        return x * a + y * b + a * b;
    }
    return Helper(1 + x * y, 1 - y);
}
{% endhighlight %}

We had to remove `x` and `y` from `Helper`'s parameters, since `Helper` is defined within the scope of `x` and `y`. So, `Helper` is actually a closure.


Now, ask ourselves why we are providing `Helper` with a name: it is a local, internal function of `F` and we are not going to reuse it in any other place. We might as well define it as an anonymous function, by the means of a lambda expression, and then immediately invoke it:

{% highlight csharp %}
static int F(int x, int y)
{
    return ((Func<int, int, int>) (
        (a, b) => 
            x * a + y * b + a * b))
        (1 + x * y, 1 - y);
}
{% endhighlight %}

The resulting code is definetely weird and not idiomatic, but it's correct.<br />
Notice how we could distinguish 3 different areas in it: 

* an initial segment where `a` and `b` -- the arguments of the former function `Helper` -- are declared
* the function's body, `x*a + y*b + a*b`
* a last part, where two values for `a` and `b` are provided (that is, their initialization):

{% highlight csharp %}
static int F(int x, int y)
{
    return ((Func<int, int, int>) (
        (a, b) =>                     // arguments declaration
            x * a + y * b + a * b))   // body
        (1 + x * y, 1 - y);           // arguments value initialization
}
{% endhighlight %}

Observe how we are not dealing with variables, but with function arguments.<br />
It's at this point that the *Wizard book* claims:

> This construct is so useful that there is a special form called `let` to make its use more convenient.
>
> Using `let` the `f` procedure could be written as:

{% highlight csharp %}
static int F(int x, int y)
{
    var a = 1 + x * y;             // variables declaration + initialization
    var b = 1 - y;
            
    return x * a + y * b + a * b;  // body
}
{% endhighlight %}

This is more conventional and familiar!<br/>
Consider the last 2 snippets. In Lisp, the latter is just syntatic sugar of the former: whenever you define a variable with `let` and you assign it a value, you are in fact defining a closure, whose parameter has got the provided name and value, and whose body is the code in that variable's scope.

Woah, it makes sense... They are just the same concept, with two different syntaxes.

## Implicit and explicit scopes

About scopes, I think it is worth mentioning that, in Lisp, the special form `let` requires the developer to explicitly define the scope of the variable. Notice the following:

{% highlight lisp %}
(let ((b (- 1 y)))    # we are assigning 1 - y to b, and also beginning b's scope

  ..                  # body, where b is used

  )                   # b's scope end

{% endhighlight %}

The very first and the very last parenthesis in this snippet delimit the variable's scope.<br />
In C#, scopes are rarely defined likewise explicitly. Whenever a symbol is defined with `var`, a scope is implicitily defined. So, when we write: 

{% highlight csharp %}
static int F(int x, int y)
{
    var a = 1 + x * y;
    var b = 1 - y;
            
    return x * a + y * b + a * b;
}
{% endhighlight %}

2 scopes are implicitily defined, as we would have written:

{% highlight csharp %}
static int F(int x, int y)
{
    {                                        // beginning of a's scope
        var a = 1 + x * y;
        {
            var b = 1 - y;                   // beginning of b's scope
            
            return x * a + y * b + a * b;    // body
        }                                    // end of b's scope
    }                                        // end of a's scope
}
{% endhighlight %}

I find it so sweet how a 1958's programming language can still provide insights and interesting perspectives to modern developers. By the way, get a copy of SICP: it is a very challenging book, but it is definitely worth a read.

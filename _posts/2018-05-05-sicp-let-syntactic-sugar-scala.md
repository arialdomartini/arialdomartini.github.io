---
Layout: post
title: Variables are simply syntatic sugar for lambda expressions (Scala)
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- SICP
- Lisp
- Scala
---

While reading [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sites/default/files/sicp/index.html), I got hit by this sentence (p. 65):

> "A `let` expression is simply syntatic sugar for the underlying lambda expression"

It claims that variables definition is not a necessary language feature, and that it has been introduced only as a convenient alternative to lambda expressions.

WAT? Seriously?

<!--more-->

**Scala version** \|  [C# version](sicp-let-syntactic-sugar-csharp.html)

## Lisp's `let`

To provide you with a context: the sentence refers to Lisp's `let`; you can think to `let` as the equivalent of Scala's `val`/`var` for variables initialization.<br />
Lisp's:

```
(let ((x 10)) ...
```

has more or less the same meaning of Scala's:

```
val x = 10
```

That's not strictly true, but bear with me.<br />
Also, the same book defines "*syntatic sugar*" as:

> "convenient alternative surface structures for things that can be written in more uniform ways"

It should be clearer now why the sentence:

> "A let expression is simply syntatic sugar for the underlying lambda expression."

if applied to Scala, really would mean that the reserved word `val` wouldn't be a strictly necessary language construct, as a variable initialization could be easily replaced by lambdas.<br />
Although the explaination provided by the book revolves around Scheme, a Lisp dialect, and might not necessarily be valid with other programming languages, I found it fun enough to translate it to Scala and C#.<br />

## Guess yourself before reading
What variable initialization and lambda expressions have in common? How could one replace a variable with a lambda? They are so distant topics to me that I don't see how one could be the syntatic sugar of the other.

I've got a suggestion: before reading the next paragraph, challenge yourself, and try to get an answer to the question: how could you replace local variables with lambda expressions, without using any `val` or `var`?

# From an algebraic function to a lambda expression

SICP starts from this algebraic function:

![algebraic function](static/img/sicp-let/algebraic-function.png)

You can easily translate it to Scala as:

{% highlight scala %}
def f(x: Int, y: Int ) : Int =
  x * (1 + x*y) + y * (1 - y) + (1 + x*y) * (1 - y)
{% endhighlight %}


Since `1 + x * y` and `1 - y` are repeated twice, you could write the same function as:

![algebraic function simplified](static/img/sicp-let/algebraic-function-simplified.png)

where `a` and `b` are defined as:

![value of a](static/img/sicp-let/a.png)<br />
![value of b](static/img/sicp-let/b.png)
<br />

In Scala, this could be implemented introducing an auxiliary function `helper`:

{% highlight scala %}
def helper(x: Int, y: Int, a: Int, b: Int): Int =
  x*a + y*b + a*b

def f(x: Int, y: Int ) : Int = helper(x, y, 1 + x*y, 1-y )
{% endhighlight %}


Notice how the auxiliary function's body is the function in the desired, simplified form, while the value of `a` and `b` is provided by `f`.<br />
Also notice that, since the auxiliary function `helper` is only used by `f`, it could be defined as a local function, i.e. as a nested function:

{% highlight scala %}
def f(x: Int, y: Int) : Int = {
  def helper(x: Int, y: Int, a: Int, b: Int): Int =
    x*a + y*b + a*b
  helper(x, y, 1 + x*y, 1-y)
}
{% endhighlight %}

There's no need to explicitly pass `x` and `y` to `helper`, since `helper` is defined within the scope of `x` and `y`. So, we can simplify its signature removing `x` and `y`, and making `helper` a closure:


{% highlight scala %}
def f(x: Int, y: Int) : Int = {
  def helper(a: Int, b: Int): Int =
    x*a + y*b + a*b
  helper(1 + x*y, 1-y)
}
{% endhighlight %}

Now, ask ourselves why we are providing `helper` with a name: it is a local, internal function of `f` and we are not going to reuse it in any other place. We might as well define it as an anonymous function, by the means of a lambda expression, and then immediately invoke it:

{% highlight scala %}
def f(x: Int, y: Int): Int = {
  (
    (a: Int, b: Int) =>
    x*a + y*b + a*b
  ) (1+x*y, 1-y)
}
{% endhighlight %}

The resulting code is definetely weird and not idiomatic, but it's correct.<br />
Notice how we could distinguish 3 different areas in it: 

* an initial segment where `a` and `b` -- the arguments of the former function `helper` -- are declared
* the function's body, `x*a + y*b + a*b`
* a last part, where two values for `a` and `b` are provided (that is, their initialization):

{% highlight scala %}
def f(x: Int, y: Int): Int = {
  (
    (a: Int, b: Int) =>     // arguments declaration
    x*a + y*b + a*b         // body
  ) (1+x*y, 1-y)            // arguments value initialization
}
{% endhighlight %}

Observe how we are not dealing with variables, but with function arguments.<br />
It's at this point that the *Wizard book* claims:

> This construct is so useful that there is a special form called `let` to make its use more convenient.
>
> Using `let` the `f` procedure could be written as:

{% highlight scala %}
def f(x: Int, y: Int): Int = {
  val a: Int = 1 + x*y      // variables declaration + initialization
  val b: Int = 1-y

  x*a + y*b + a*b           // body
}
{% endhighlight %}

This is more conventional and familiar!<br/>
Consider the last 2 snippets. In Lisp, the latter is just syntatic sugar of the former: whenever you define a variable with `let` and you assign it a value, you are in fact defining a closure, whose parameter has got the provided name and value, and whose body is the code in that variable's scope.

Woah, it makes sense...  They are just the same concept, with two different syntaxes.

## Implicit and explicit scopes

About scopes, I think it is worth mentioning that, in Lisp, the special form `let` requires the developer to explicitly define the scope of the variable. Notice the following:

{% highlight lisp %}
(let ((b (- 1 y)))    # we are assigning 1 - y to b, and also beginning b's scope

  ..                  # body, where b is used

  )                   # b's scope end

{% endhighlight %}

The very first and the very last parenthesis in this snippet delimit the variable's scope.<br />
In Scala, scopes are rarely defined likewise explicitly. Whenever a symbol is defined with `val`, a scope is implicitily defined. So, when we write: 

{% highlight scala %}
def f(x: Int, y: Int): Int = {
  val a: Int = 1 + x*y
  val b: Int = 1-y

  x*a + y*b + a*b
}
{% endhighlight %}

2 scopes are implicitily defined, as we would have written:

{% highlight scala %}
def f(x: Int, y: Int): Int = {
  val a: Int = 1 + x*y; {   // beginning of a's scope
    val b: Int = 1-y; {     // beginning of b's scope

      x*a + y*b + a*b       // body
        
    }                       // end of b's scope
  }                         // end of a's scope
}
{% endhighlight %}

I find it so sweet how a 1958's programming language can still provide insights and interesting perspectives to modern developers. By the way, get a copy of SICP: it is a very challenging book, but it is definitely worth a read.

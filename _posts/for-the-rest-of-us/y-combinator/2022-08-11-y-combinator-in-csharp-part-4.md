---
layout: post
title: "Deriving the Y Combinator in C# - Part 4 - Non-recursive Y Combinator"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
include_in_index: false
tags:
- C#
- Functional Programming
- Lambda Calculus
---
## Index
* [Part 1 - Recursive anonymous functions][part-1]
* [Part 2 - The code problem][part-2]
* [Part 3 - A recursive Y Combinator][part-3]
* **Part 4 - Non-recursive Y Combinator**

**TL;DR: [just show me the code][just-show-me-the-code]**.

# Non-recursive Y Combinator

Enjoyed your beer? Let's get started.

There are several ways to get to the non-recursive Y.<br/>
An approach, magnificently covered in Scheme by professor [Michael Vanier][michael-vanier] in [The Y Combinator (Slight Return)][y-combinator-michael-vanier], is to start over from the original recursive `sum` function, to extract `mkSum` and then to inject `mkSum` into itself. In C# it does not translate to the easiest path, but if you feel up for the challenge, go down the rabbit hole reading [Recursive Y Combinator in C# - alternative approach][part-4-alternative].

A shorter and possibly more intuitive plan of attack is to directly play with the recursive Y we got in [Part 3][part-3]

```csharp
static Func<Func<Sum, Sum>, Sum> Y =
  f =>
    n =>
      f(Y(f))(n);
```

refactoring the recursion away. This will require injecting a function into itself, just like in Michael Vanier's approach, only at an earlier stage. <br/>
Let's get started.

* [Step 1 - Extract Y to a local function][step-1]
  * [Make `n => f(Y(mkSum))(n)` a variable](#make-n-fymksumn-a-variable)
  * [Extract a local function](#extract-a-local-function)
  * [Get rid of the temporary variable](#get-rid-of-the-temporary-variable)
* [Step 2 - Replace `Y(mkSum)` with `sub`](#step-2---replace-ymksum-with-sum)
* [Step 3 - Inject self](#step-3---inject-self)
  * [Replace `sub` with `self`](#replace-sub-with-self)
* [Step 4 - Replace variable with lambda](#step-4---replace=variable-with-lambda)
  * [Variables are just syntactic sugar of lambda expressions](#variables-are-just-syntactic-sugar-of-lambda-expressions)
  * [Replace `sub(sub)` with lambda](#replace-subsub-with-lambda)
* [Step 5 - Inline `sub`](#step--5--inline-sub)

[step-1]: #step-1---extract-y-to-a-local-function

## Step 1 - Extract `Y` to a local function
As we anticipated, in order to eliminate the recursion, in our refactoring journey we will eventually inject a function into itself. From `f()` to `f(f)`. We cannot do this directly with `Y`, though, because its signature must be preserved.<br/>
So, we'd better create a bottleneck, extracting `Y` to a local function.

It's easy to see that R# is not able to do that: if you select `n => f(Y(mkSum))(n)` and you apply [Extract Local Function][extract-method], R# would default to [Extract Method][extract-method] instead. It's probably a bug.<br/>
We have to do this manually, in 3 steps, using a temporary variable:

* [Introduce a temporary variable](#make-n--fymksumn-a-variable)
* [Extract a local function](#extract-a-local-function)
* [Get rid of the temporary variable](#get-rid-of-the-temporary-variable)

### Make `n => f(Y(mkSum))(n)` a variable
Select `n => f(Y(f))(n)` and apply [Introduce Variable][introduce-variable]:

```csharp
private static Sum Y(Func<Sum, Sum> f)
{
    Sum sum1 = n => f(Y(f))(n);
    return sum1;
}
```

### Extract a local function
Select `n => f(Y(mkSum))(n)` and [Extract a Local Function][extract-method] out of it (funny enough, now R# does not complain):

```csharp
private static Sum Y(Func<Sum, Sum> f)
{
    Sum sub() =>
        n => f(Y(f))(n);

    Sum sum1 = sub();
    return sum1;
}
```

### Get rid of the temporary variable
Target `sum1` and apply [Inline Variable][inline-variable] to get rid of it:

```csharp
private static readonly Sum sum =
    Y(mkSum);

private static Sum Y(Func<Sum, Sum> f)
{
    Sum sub() =>
        n => f(Y(f))(n);

    return sub();
}
```

Voilà! We have `Y` in a local function. We can now play with `sub()` and safely change its signature with no impacts on the `Y(mkSum)` calling site.<br/>


## Step 2 - Replace `Y(f)` with `sub()`
Focus on `f(Y(f))(n)`.<br/>
By definition, `Y(f)` returns `sub()`. So, we can easily replace one with the other. Wherever you see `Y(f)`, replace it with `sub()`:

```csharp
private static Sum Y(Func<Sum, Sum> f)
{
    Sum sub() =>
        n => f(sub())(n);

    return sub();
}
```

Is the test still green? Yes, it is! That's because `sub()` is lazy, and it does not fall into any stack overflow. This should clarify why in [Step 1][step-1] we extracted the lazy `n => f(Y(mkSum))(n)` rather than the eager `f(Y(mkSum))`.

Is this a non-recursive Y? Not yet. Indeed, `sub()` is still recursive.<br/>
But we are almost there! Get ready, we are going to perform the most crucial step: removing the recursion.

## Step 3 - Inject self
Let's inject a continuation into `sub()`. What would the perfect continuation be? But of course, `sub` itself! Here we are: that's the announced step where we inject a function into itself.

*Injecting* a function into itself is not like *calling* a function from itself: the latter is the classical recursion; the former is *almost* recursion, it's [Continuation Passing Style][continuation-passing-style].<br/>
So, our next goal is to make `sub` a parameter of itself, which will make `sub` not recursive, just *almost*.

Again, R# will not help us. This time, as we will see, we can forgive it.

Let's focus on `sub`:

```csharp
    Sum sub() =>
        n => f(sub())(n);
```

`sub` is of type `Func<Sum>`. Ideally, selecting `sub` we should be able to [make it a variable][introduce-variable], getting something like:


```csharp
    Sum sub()
    {
        var v = sub;
        return n => f(v())(n);
    }
        
```


If you try, though, you will get a depressing:

![error message by ReSharper that it's not able to introduce a variable](static/img/y-combinator-in-csharp/inject-self.png)

Can you guess why? It will be clear very soon.<br/>
Trying to make `sub` a parameter of itself with [Introduce Parameter][introduce-parameter] fails even more cryptically: R# would just ignore the keybinding. Definitely, we are taking R# to its limit. We have to do this manually.


```csharp
private static Sum Y(Func<Sum, Sum> f)
{
    Sum sub(??? self) =>
        n => f(sub(self))(n);

    return sub(sub);
}
```

Ok, this one is tricky. Let's comment what we have written:

* `Sub sub() => ...` gets a new parameter, which we called `self`. So it's now `Sum sub(??? self) => ...`.<br/>Besides the unknown type we marked with `???`, this makes sense, doesn't it?
* In `n => f(sub(self)(n)`, we are correctly feeding `sub` with the continuation `self`; 
* `return sub()` becomes `return sub(sub)` because we are providing `sub` with itself as a continuation.

All good.

The only big deal is: what's the type of `self`? What to replace `???` with?<br/>
By definition, `self = sub`, and `sub` *was* of type `Func<Sum>`. Notice we say "*it was*". Indeed, now `sub` takes a parameter of (still unknown) type `???`, so its type changed to from `Func<Sum>` to `Func<???, Sum>`.<br/>
`???` is the type of `sub`. So, inlining, we get to `Func<Func<???, Sum>, Sum>`; if we continue inlining, we get `Func<Func<Func<???, Sum>, Sum>, Sum>`, the `Func<Func<Func<Func<???, Sum>, Sum>, Sum>, Sum>` and so on, ab nauseam.<br/>
It's `Func` all the way down...

Smells like recursion, doesn't it?

In fact, `sub`'s type *is* recursive. No surprises, after all, if R# was not able to realize that.<br/>
Let's define a recursive type manually:

```csharp
private delegate Sum Rec(Rec self);
```

With this we can define:

```csharp
private static Sum Y(Func<Sum, Sum> f)
{
    Sum sub(Rec self) =>
        n => f(sub(self))(n);

    return sub(sub);
}
```
Compilation's OK.<br/>
Test's green.<br/>
Wow, that's a result.

### Replace `sub` with `self`
`self` is `sub`. Then `sub(self) = self(self)`:

```csharp
private delegate Sum Rec(Rec self);

private static Sum Y(Func<Sum, Sum> f)
{
    Sum sub(Rec self) =>
        n => f(self(self))(n);

    return sub(sub);
}
```

Take your time to meditade on the last snippet:

* It is non-recursive
* It's a [combinator][combinator]: that is, it's a pure function, only depending on its own parameters.
* More thatn this, is has got the same signature of our ideal Y Combinator
* It passes our tests

You know the saying "If it walks like a duck"?<br/>
Is that, maybe, the non-recursive Y Combinator, already? **Oh, yes it is!**<br/>
But it's not yet in a form worth to be tattooed on your forearm. Let's just perform a last little effort to refactor it to its canonical shape. Then you are ready to schedule an appointment to your preferred tattoo shop.
    
## Step 4 - Replace variable with lambda
The last snippet has got 2 little problems.

The first one is:  we would like to have a single, fluent expression, with no local functions. That's easy: we just have to [inline][inline-method] `sub`.

The second problem is: `sub` is mentioned twice in `return sub(sub)`. Inlining `sub` would produce an insulting:

```csharp
private static Sum Y(Func<Sum, Sum> f) =>
    n1 => f(((Rec)(self => n => f(self(self))(n)))(self => n => f(self(self))(n)))(n1);
```

You would never tattoo this: you don't have enough long forearms, do you?<br/>
Ok. We need a last little trick. We need to learn how to replace variables with lambda expressions. We have to convince ourselves that: 

```csharp
sub(sub)
```

is equivalent to

```csharp
new Func<Rec, Sum>(f => f(f))(sub)
```

where `sub` is mentioned only once.<br/>Now, this deserves a little explanation, right?

### Variables are just syntactic sugar of lambda expressions
This refactoring move is easily done once we realize that [variables are just syntactic sugar for lambda expressions][variables-are-syntactic-sugar-for-lambdas]. This will not surprise our fellow Lisp programmer. In C# we could observe that:

```csharp
var foo = 42;
```

can be written with a lambda expression followed by its invocation:

```csharp
Func<int> f = x => x;

var foo = f(42);
```

that inlined becomes

```csharp
var foo =
    ((Func<int>)(x => x))
        (42)
```

This also works with more complex expressions:

```csharp
int F(int i)
{
    var foo = 42;
    return (foo + i)*2;
}
```

becomes

```csharp
int F(int i)
{
    return 
        ((Func<int>)(foo => 
            (foo + i)*2*))
        (42);
}
```


Finally, this also works with multiple parameters:


```csharp
static int F()
{
    var name = "John";
    var surname = "Doh";
            
    return $"Hi, {name} {surname}!";
}

F().Should().Be("Hi, John Doe!");
```

can be converted to:

```csharp
static int F()
{
    return
        ((Func<string, string, string>)(
            (name, surname) =>
                $"Hi {name} {secondName}!"))
        ("John", "Doh")
}
```

Pretty neat, isn't it? If you want to learn how to perform these transformations only using refactoring moves, go read [Refactoring variables to lambda expressions ][refactoring-variables-to-lambda-expressions]

### Replace `sub(sub)` with lambda
Good, let's apply this to `return sub(sub)`.

```csharp
private static Sum Y(Func<Sum, Sum> f)
{
    Sum sub(Rec self) =>
        n => f(self(self))(n);

    return sub(sub);
}
```

becomes:

```csharp
private static Sum Y(Func<Sum, Sum> f)
{
    Sum sub(Rec self) =>
        n => f(self(self))(n);

    return Func<Rec, Sum>(f => f(f))(sub);
}
```

where `sub` is mentioned only once.

## Step 5 - Inline `sub`
Now [inlining][inline-method] `sub` we get a nice looking:

```csharp
private static Sum Y(Func<Sum, Sum> f) =>
    new Func<Rec, Sum>(f =>
        f(f))
    (self =>
        n =>
            f(self(self))(n));
```

This is the Y Combinator. Feel proud of yourself, we made it.

# Conclusion
If we only could get rid of the boiler plate code C# requires, this would be: 

```csharp
var Y = f =>
    (f => f(f))(self => f(self(self));

var sum = Y(mySum);
```

which is equivalent to the original Lisp's Y Combinator:

```scheme
(define Y
  (lambda (f)
    ((lambda (x) (x x))
     (lambda (x) (f (lambda (y) ((x x) y)))))))
```


Cool. Mission accomplished. You deserve a second beer.

Prosit!

## References

* [JetBrains Refactorings](https://www.jetbrains.com/help/resharper/Main_Set_of_Refactorings.html)
  * [Extract Method][extract-method]
  * [Introduce Parameter][introduce-parameter]
  * [Inline Variable][inline-variable]
  * [Inline Method][inline-method]
  * [Context Actions][context-actions]
  * [Move Variable To Outer Scope][context-actions]
  * [Move Local Function To Outer Scope][context-actions]
* [Continuation Passing Style][continuation-passing-style]
* [Refactoring variables to lambda expressions][variables-are-syntactic-sugar-for-lambdas]
* [Variables Just Are Syntactic Sugar For Lambda Expressions][sicp-let-syntactic-sugar-csharp]
* [Continuation Passing Style][continuation-passing-style]
* [Combinator - Haskell Wiki][combinator]

[Extract-method]: https://www.jetbrains.com/help/resharper/Refactorings__Extract_Method.html
[introduce-variable]: https://www.jetbrains.com/help/resharper/Refactorings__Introduce_Variable.html
[introduce-parameter]: https://www.jetbrains.com/help/resharper/Refactorings__Introduce_Parameter.html
[inline-variable]: https://www.jetbrains.com/help/resharper/Refactorings__Inline_Variable.html
[inline-method]: https://www.jetbrains.com/help/resharper/Refactorings__Inline_Method.html
[context-actions]: https://www.jetbrains.com/help/rider/Reference__Options__Languages__CSharp__Context_Actions.html
[continuation-passing-style]: https://en.wikipedia.org/wiki/Continuation-passing_style
[sicp-let-syntactic-sugar-csharp]: sicp-let-syntactic-sugar-csharp 
[y-combinator-michael-vanier]: https://mvanier.livejournal.com/2897.html
[michael-vanier]: https://www.cms.caltech.edu/people/mvanier
[refactoring-variables-to-lambda-expressions]: refactoring-variables-to-lambda-expressions
[variables-are-syntactic-sugar-for-lambdas]: sicp-let-syntactic-sugar-csharp
[continuation-passing-style]: https://en.wikipedia.org/wiki/Continuation-passing_style
[combinator]: https://wiki.haskell.org/Combinator

[just-show-me-the-code]: y-combinator-in-csharp-code-only#non-recursive-y-combinator
[part-1]: y-combinator-in-csharp
[part-2]: y-combinator-in-csharp-part-2
[part-3]: y-combinator-in-csharp-part-3
[part-4-alternative]: y-combinator-in-csharp-part-4-alternative

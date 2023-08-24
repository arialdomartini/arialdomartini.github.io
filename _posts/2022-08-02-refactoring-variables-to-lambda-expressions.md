---
layout: post
title: Refactoring variables to lambda expressions
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- SICP
- C#
- Functional Programming
---
Variables are just syntactic sugar for lambda expressions: they are not stricly necessary and ideally C# could happily live without them. Even the reseved word `var` could be eliminated.

In this post we will use ReSharper for refactoring variables away, transforming them to anonymous lambda expressions.

That's a trick that will come in handy for deriving the Y Combinator.
<!--more-->
I wrote about this topic [back in 2018][variables-syntactic-sugar-csharp], in the excitement of reading [Structure and Interpretation of Computer Programs][sicp].

SICP claims that: 

> "[A variable] is simply syntatic sugar for the underlying lambda expression"

where "*syntatic sugar*" is defined as: 

> "convenient alternative surface structures for things that can be written in more uniform ways"

In other words, the claim is that variables make code sweeter to write and read, but they add no expressive power.<br/>
Let's go straight to the code.

## An example
Here's a snippet using 3 variables:

```csharp
[Fact]
void variables_are_used()
{
    var amount = 7;
    var product = "beers";
    var price = 3;

    Assert.Equal(
        "7 beers cost 21 EUR",
        $"{amount} {product} cost {amount * price} EUR");
}
```

Squeezing your eyes, you could see how this is equivalent to the (more convoluted):

```csharp
[Fact]
void variables_are_replaced_with_lambdas()
{
    Assert.Equal(
        "7 beers cost 21 EUR",
        new Func<int, string, int, string>(
            (price, product, amount) =>
                $"{amount} {product} cost {amount * price} EUR")
        (3, "beers", 7)
    );
}
```

where there are no variables at all.<br/>
By the end of this post you will learn how to get from the former to the latter implementation, only applying [refactoring][refactoring] moves.

Let me be clear: the latter form is way less readable than the original one, and by no means *better*.<br/>
Yet, the transformation to derive it is a not obvious refactoring move that can come in handy, especially when playing with functional programming.

Also, it's fun to derive. So, why not?

## Variables and named methods
Let's start from a very prosaic equality: any variable can be replaced by a parameterless [constant function][constant-function].

In:

```csharp
[Fact]
void variable_is_used()
{
    var foo = "bar";

    Assert.Equal(
        "bar",
        foo);
}
```

`var foo = "bar"` can be replaced with `string foo() { return "bar"; }`, getting to:

```csharp
[Fact]
void variable_is_replaced_with_method()
{
    string foo() =>
        "bar";

    Assert.Equal(
        "bar",
        foo());
}
```

That's insultingly trivial, isn't it?<br/>
Bear with me. Let's see what happens if we use R# to make that `foo()` function an anonymous lambda.

By the way: getting from `var foo = "bar";` to `string foo() => "bar"` is by itself a refactoring; it's indeed a matter of applying [Extract Method][extract-method] on `"bar"` and of inlining `foo` with [Inline Variable][inline-variable].

## From Named Local Function to Anonymous Lambda
It's much more interesting to see what happens when we convert `foo()` to an anonymous lambda.<br/>
We can proceed as follows:

### Move value away
Select `"bar"` and apply [Extract Method][extract-method]

```csharp
[Fact]
void variable_is_replaced_with_method()
{
    var foo = Temp();

    Assert.Equal(
        "bar",
        foo);
}

private string Temp() =>
    "bar";
```

Ideally, we could have done the same with [Extract Local Function][extract-method]: for some reasons, though, R# would not be able to perform the next step.

### Create the `Func`
Extract `Temp` (not `Temp()`, only `Temp`, without the `()`) in the expression `var foo = Temp()` as a variable with [Introduce Variable][introduce-variable]. This is the crucial step: it will convert a method to an equivalent variable of type `Func<T>`:


```csharp
[Fact]
void variable_is_replaced_with_method()
{
    Func<string> temp = Temp;
    var foo = temp();

    Assert.Equal(
        "bar",
        foo);
}

private string Temp() =>
    "bar";
```

### Inline everything
Apply [Inline Variable][inline-variable], in order, to `Temp`, to `temp` and finally to `foo`

```csharp
[Fact]
void variable_is_replaced_with_anonymous_lambda()
{
    Assert.Equal(
        "bar",
        ((Func<string>)(() => 
            "bar"))
         ());
}
```

Here we are. Both the variable and the named function have disappeared, leaving us with just an anonymous lambda.

Notice the trailing `()`: this invokes the parameterless anonymous lambda, feeding it no value, and getting back `"bar"`.

## Two steps back

To understand why we got `()`, it is useful to review what we got when we extracted `Temp()`:

```csharp
[Fact]
void variable_is_replaced_with_method()
{
    var foo = Temp();

    Assert.Equal(
        "bar",
        foo);
}

private string Temp() =>
    "bar";
```

`Temp()` is parameterless, which generates a parameterless lambda. If it had a parameter, also the final anonymous lambda would have had one.<br/>
Let's try.


### Add a parameter
We can promote `"bar"` to a parameter, applying [Introduce Parameter][introduce-parameter]:


```csharp
[Fact]
void variable_is_replaced_with_method()
{
    var foo = Temp("bar");

    Assert.Equal(
        "bar",
        foo);
}

private string Temp(string value) =>
    value;
```

### Create the `Func`
Like before, extract `Temp` (without the `("bar")` part) in the expression `var foo = Temp("bar")`

```csharp
[Fact]
void variable_is_replaced_with_method()
{
    Func<string, string> converter = Temp;
            
    var foo = converter("bar");

    Assert.Equal(
        "bar",
        foo);
}

private string Temp(string value) =>
    value;
```

Interesting! We get a `Func<string, string>` instead of a simple `Func<string>`.

### Inline everything
Just like before, inline  `Temp`, `temp` and `foo`. We get:

```csharp
[Fact]
void variable_is_replaced_with_method()
{
    Assert.Equal(
        "bar",
        
        ((Func<string, string>)(value =>
            value))
        ("bar"));
}
```

As is turned out, we get `("bar")` instead of `()`.

## In short
That's basically it. Any:

```csharp
VariableType variable = some_value;

{
  [body_using_variable]
}
```

can be replaced with

```csharp
Func<VariableType, BodyType>(variable =>
  [body_using_variable]
)
(some_value)
```

where the original variable name is replaced with an anonymous lambda parameter, and its value with the argument the lambda is fed with.

Notice that the body of the `Func` in 

```csharp
[Fact]
Assert.Equal(
    "bar",
    ((Func<string, string>)(value =>
        value))
    ("bar"));
```


is the identity function `value => value`: this is because we converted the constant function.

Let's go one level up, starting from a more complex function and using more variables. 

## Multiple variables
Replacing multiple variables with a lambda is equally mechanical. Let's use the initial sample snippet:

```csharp
[Fact]
void variables_are_used()
{
    var amount = 7;
    var product = "beers";
    var price = 3;

    Assert.Equal(
        "7 beers cost 21 EUR",
        $"{amount} {product} cost {amount * price} EUR");
}
```

### Isolate the body
Apply [Extract Method][extract-method] to the body, that is to `$"{amount} {product} cost {amount * price} EUR")`

```csharp
[Fact]
void variables_are_used()
{
    var amount = 7;
    var product = "beers";
    var price = 3;

    Assert.Equal(
        "7 beers cost 21 EUR",
        Temp(amount, product, price));
}

private static string Temp(int amount, string product, int price) =>
    $"{amount} {product} cost {amount * price} EUR";
```

### Create the `Func`
Convert `Temp` (without its arguments) to an anonymous lambda, with [Introduce Variable][introduce-variable]:

```csharp
[Fact]
void variables_are_used()
{
    var amount = 7;
    var product = "beers";
    var price = 3;

    Func<int, string, int, string> temp = Temp;
            
    Assert.Equal(
        "7 beers cost 21 EUR",
        temp(amount, product, price));
}

private static string Temp(int amount, string product, int price) =>
    $"{amount} {product} cost {amount * price} EUR";
```

Notice how type we get, `Func<int, string, int, string>`, it's basically `Func<type_var_1, type_var_2, type_var_3, type_body>``

### Inline
Inline `Temp` with [Inline Method][inline-method]

```csharp
[Fact]
void variables_are_used()
{
    var amount = 7;
    var product = "beers";
    var price = 3;

    Func<int,string,int,string> temp = (amount, product, price) => $"{amount} {product} cost {amount * price} EUR";
            
    Assert.Equal(
        "7 beers cost 21 EUR",
        temp(amount, product, price));
}
```

### Get rid of the variables
Apply [Inline Variable][inline-variable] to to get rid of all the variables:

```csharp
[Fact]
void variables_are_used()
{
    Func<int,string,int,string> temp = 
        (amount, product, price) => 
            $"{amount} {product} cost {amount * price} EUR";
            
    Assert.Equal(
        "7 beers cost 21 EUR",
        temp(7, "beers", 3));
}
```

### Inline the rest
Finally, [inline][inline-variable] `temp`:

```csharp
[Fact]
void variables_are_used()
{
    Assert.Equal(
        "7 beers cost 21 EUR",
        ((Func<int, string, int, string>)(
            (amount, product, price) =>
                $"{amount} {product} cost {amount * price} EUR"))
        (7, "beers", 3));
}
```

## Conclusion
Is that an end in itself?<br/>
Just a bit, but not quite. This is indeed one of the moves for replacing procedural code with functional expressions.<br/>

The equivalence between `let` expressions and lambdas is very well known in the Lisp's circles. It's not in C#'s, but sooner or later the experienced Functional Programmer will bump into it.

So what's next? Go and use it for deriving Y. Have fun!


# Comments

[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/13)

## References

* [Structure and Interpretation of Computer Programs][sicp]
* [Variables Are Syntactic Sugar For Lambda Expressions (C#)][Variables-Syntactic-Sugar-Csharp]
* [Variables Are Syntactic Sugar For Lambda expressions (Scala)][variables-syntactic-sugar-scala]
* [Refactoring - Martin Fowler][refactoring]
* [Constant Function - Wikipedia][constant-function]
* [JetBrains Refactorings][jetbrains-refactorings]
  * [Extract Method][extract-method]
  * [Introduce Variable][introduce-variable]
  * [Introduce Parameter][introduce-parameter]
  * [Inline Variable][inline-variable]
  * [Inline Method][inline-method]
  * [Extract Local Function To Regular Method][context-actions]

[sicp]: https://web.mit.edu/6.001/6.037/sicp.pdf
[variables-syntactic-sugar-csharp]: sicp-let-syntactic-sugar-csharp
[variables-syntactic-sugar-scala]: sicp-let-syntactic-sugar-scala
[refactoring]: https://martinfowler.com/tags/refactoring.html
[constant-function]: https://en.wikipedia.org/wiki/Constant_function
[jetbrains-refactorings]: https://www.jetbrains.com/help/resharper/Refactorings__Index.html
[extract-method]: https://www.jetbrains.com/help/resharper/Refactorings__Extract_Method.html
[introduce-variable]: https://www.jetbrains.com/help/resharper/Refactorings__Introduce_Variable.html
[inline-variable]: https://www.jetbrains.com/help/resharper/Refactorings__Inline_Variable.html
[inline-method]: https://www.jetbrains.com/help/resharper/Refactorings__Inline_Method.html
[introduce-parameter]: https://www.jetbrains.com/help/resharper/Refactorings__Introduce_Parameter.html
[context-actions]: https://www.jetbrains.com/help/resharper/Reference__Options__Languages__CSharp__Context_Actions.html

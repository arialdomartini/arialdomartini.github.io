---
layout: post
title: "Implementing the Y Combinator in C# - Part 4 Alternative - Non-recursive Y Combinator"
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

## Step 1 - An ordinary recursive function
Let's start over from our recusive `sum` function:


```csharp
public class YCombinator
{
    private const int Max = 12_000;
    private readonly Arbitrary<int> PositiveNumbers = Arb.From(Gen.Choose(0, Max));
    
    private delegate int Sum(int n);

    private static readonly Sum sum =
        n =>
            n == 0 ? 0 : n + sum(n - 1);

    [Property]
    Property it_meets_the_gauss_formula() =>
        ForAll(PositiveNumbers, n =>
            sum(n) == n * (n + 1) / 2);
}
```


## Step 2 - Inject `self(self)`
### Extract `MkSum`
Just like before, we define `MkSum` applying [Extract Method][extract-method] to `n => n == 0 ? 0 : n + sum(n - 1)`.

```csharp
static readonly Sum sum =
    MkSum();
    
static Sum MkSum() =>
    n =>
        n == 0 ? 0 : n + sum(n-1);
```


In the [previous installment][part-3] we proceded injecting `sum` as a continuation. Let's try something different now.<br/>

### Replace `sum` with `MkSum`
The current implementation of `MkSum` is (not surprisingly) very similar to the original `sum`. Let's make it directly recursive, letting it call itself:

```csharp
static readonly Sum sum =
    MkSum();
    
static Sum MkSum() =>
    n =>
        n == 0 ? 0 : n + MkSum()(n-1);
```

This is slightly different from what we obtained before. As a comparison:

```csharp
static Sum MkSum(Sum continuation) =>
    n =>
        n == 0 ? 0 : n + continuation(n-1);
```

Notice how `continuation` was of type `Sum`, while `MkSum` is of type `Func<Sum>`.


### Make `MkSum` a Parameter
The idea is to feed `MkSum` with itself. This will require some refactoring moves that R# is not able to automate.<br/>
In the expression:

```csharp
        n == 0 ? 0 : n + MkSum()(n-1);
```

`MkSum()` is a value of type `Sum`. `MkSum` -- without `()` -- is of type `Func<Sum>`.<br/>
R# is perfectly able to make `MkSum` a variable with [Introduce Variable][introduce-variable], which would get to:

```csharp
static readonly Sum sum =
    MkSum();
    
static Sum MkSum() =>
    n =>
    {
        Func<Sum> self = MkSum;
        n == 0 ? 0 : n + self()(n-1);
    };
```

But if you try to make it a parameter instead, with [Introduce Parameter][introduce-parameter], it would ruinously fail:

```csharp
static readonly Sum sum =
    MkSum(MkSum);
    
static Sum MkSum(Func<Sum> self) =>
    n =>
    {
        n == 0 ? 0 : n + self()(n-1);
    };
```

This does not even compile.<br/>
In the calling site in `sum`, R# correctly added a new argumento to `MkSum`, switching from `MkSum()` to `MkSum(MkSum)`.<br/>
It was also smart enough to realize that `self` is actually `MkSum` itself, changed `self()` to `self(MkSum)`.<br/>
Unfortunately, it failed to realize that, adding a new parameter to `MkSum`, it cannot be anymore of type  `Func<Sum>`. Its new type is non trivial, as it the recursive `MkSum :: Func<MkSum, Sum>`. Infering recursive types is too much for R#<br/>
We need to manually define it with a delegate:

```csharp
delegate Sum MkSum(MkSum mkSum);
```

and to fix the signature:

```csharp
static Sum MkSum(MkSum self) =>
    n =>
        n == 0 ? 0 : n + self(MkSum)(n-1);
```

Now the code compiles successfully.

### Replace `MkSum` with `self` to get `self(self)`
Since it is injected, `self` is now a continuation. It makes sense to completely remove the recursion by replacing `self(MkSum)` with `self(self)`:

```csharp
delegate int Sum(int n);
delegate Sum MkSum(MkSum mkSum);

static readonly Sum sum =
    MkSum(MkSum);

static Sum MkSum(MkSum self) =>
    n =>
        n == 0 ? 0 : n + self(self)(n-1);
```


### Replace `self(self)` with lambda
The next refactoring move is easily done once figured out that [variables just are syntactic sugar for lambda expressions][sicp-let-syntactic-sugar-csharp]. This will not surprise our Lisp fellow programmers. In C# we could observe that:

```csharp
var foo = 42;
```

can be written with a lambda expression, followed by its invocation:

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

Of course, this also works with more complex expressions:

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


and with multiple parameters:


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

Pretty neat, isn't it? Good, let's apply this to `self(self)` in

```csharp
delegate int Sum(int n);
delegate Sum MkSum(MkSum mkSum);

static readonly Sum sum =
    MkSum(MkSum);

static Sum MkSum(MkSum self) =>
    n =>
        n == 0 ? 0 : n + self(self)(n-1);
```


#### Introduce Variable for `self(self)`
We apply [Introduce Variable][introduce-variable] to `self(self)`:

```csharp
static Sum MkSum(MkSum self) =>
    n =>
    {
        var f = self(self);
        return n == 0 ? 0 : n + f(n - 1);
    }
```

#### Move variable `f` to outer scope
Let `f` float up beyond `n =>` with [Move Variable to Outer Scope][move-to-outer-scope]:

```csharp
static Sum MkSum(MkSum self)
{
    Sum f = self(self);
    return n => 
        n == 0 ? 0 : n + f(n - 1);    n =>
}
```

Run the test: it will fail. Again because C# is strict. We have to make `self(self)` lazy, from

```csharp
Sum f = self(self);
```

to 

```csharp
Sum f = x => self(self)(x);
```

Here we go:

```csharp
static Sum MkSum(MkSum self)
{
    Sum f = x => self(self)(x);
    return n => 
        n == 0 ? 0 : n + f(n - 1);    n =>
}
```

Run the test: green. Cool.<br/>
We are ready for the refactoring move From Variable to Lambda.

```csharp
static Sum MkSum(MkSum self)
{
    Sum f = x => self(self)(x);
    return n => 
        n == 0 ? 0 : n + f(n - 1);    n =>
}
```


#### Convert Variable to Lambda
Poor F# will not help here. This must be done manually:

```csharp
static readonly Sum mkSum(MkSum self) =>
    new Func<Sum, Sum>(
        f =>
            n =>
                n == 0 ? 0 : n + f(n - 1))
    (x => self(self)(x));
}
```

It's a pity that the C# type inference is not powerful enough to let us write:

```csharp
static readonly Sum mkSum(MkSum self) =>
    (f =>
        n =>
            n == 0 ? 0 : n + f(n - 1))
    (x => self(self)(x));
```

omitting the function type.

### Extract `sum` away
Can you see the [Continuation Passing Styled] `sum` function in `mkSum`'s body'? It's this part:

```csharp
    f =>
        n =>
            n == 0 ? 0 : n + f(n - 1))
```

We can make `mkSum` independent from `sum`'s logic'. Let's start with extracting it away, with [Extract Method][extract-method]:

```csharp
static readonly Func<Sum, Sum> mySum = 
    f =>
        n =>
            n == 0 ? 0 : n + f(n - 1);

static readonly Sum mkSum(MkSum self) =>
    mySum(i => self(self)(i))

static readonly Sum sum =
    n =>
        mkSum(mkSum)(n);
```

## Step 8: extract mkSum(mkSum) as a lambda
Let's modify `sum` no, applying again the refactoring move of converting a variable to a lambda:

We will go from:

```csharp
static readonly Sum sum =
    n =>
        mkSum(mkSum)(n);
```

to

```csharp
static readonly Sum sum =
    n =>
        new Func<MkSum, Sum>(p => p(p))(MkSum)(n);
```

We are almost there: the last 2 steps.

## Inline `MkSum`
We apply [Inline Method][inline-method] to `MkSum` 

```csharp
static readonly Sum sum =
    n =>
        new Func<MkSum, Sum>(p => p(p))(self =>
            MySum(i => self(self)(i)))(n);
```

This really looks like Y, already.


## Extract Y
```csharp
static readonly Func<SumC, Sum> Y =
    f =>
        n =>
            new Func<MkSum, Sum>(p => p(p))(
                self =>
                    f(i => 
                        self(self)(i)))(n);

static readonly Sum sum = Y(mySum);
```

which is equivalent to the original Lisp's Y Combinator:

```scheme
(define Y
  (lambda (f)
    ((lambda (x) (x x))
     (lambda (x) (f (lambda (y) ((x x) y)))))))
```

## References

* [JetBrains Refactorings](https://www.jetbrains.com/help/resharper/Main_Set_of_Refactorings.html)
  * [Extract Method][extract-method]
  * [Introduce Parameter][introduce-parameter]
  * [Inline Method][inline-method]
  * [Move Variable To Outer Scope][move-to-outer-scope]
* [Continuation Passing Style][continuation-passing-style]
* [Variables Just Are Syntactic Sugar For Lambda Expressions][sicp-let-syntactic-sugar-csharp]

[Extract-method]: https://www.jetbrains.com/help/resharper/Refactorings__Extract_Method.html
[introduce-variable]: https://www.jetbrains.com/help/resharper/Refactorings__Introduce_Variable.html
[introduce-parameter]: https://www.jetbrains.com/help/resharper/Refactorings__Introduce_Parameter.html
[inline-method]: https://www.jetbrains.com/help/resharper/Refactorings__Inline_Method.html
[move-to-outer-scope]: xxx
[continuation-passing-style]: https://en.wikipedia.org/wiki/Continuation-passing_style
[sicp-let-syntactic-sugar-csharp]: sicp-let-syntactic-sugar-csharp 

[just-show-me-the-code]: y-combinator-in-csharp-code-only
[part-1]: y-combinator-in-csharp
[part-2]: y-combinator-in-csharp-part-2
[part-3]: y-combinator-in-csharp-part-3

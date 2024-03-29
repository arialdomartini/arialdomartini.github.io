---
layout: post
title: "Deriving the Y Combinator in C# - Part 3 - A recursive Y Combinator"
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
* **Part 3 - A recursive Y Combinator**
* [Part 4 - Non-recursive Y Combinator][part-4]

**TL;DR: [just show me the code][just-show-me-the-code]**.

# Recursive Y Combinator

* [Step 1 - An ordinary recursive function](#step-1---an-ordinary-recursive-function)
  * [Type Alias](#type-alias)
* [Step 2 - Inject a continuation](#step-2---inject-a-continuation)
  * [Define `MkSum` with Extract Method](#define-mksu-with-extract-method)
  * [Let `sum` float up](#let-sum-float-up)
  * [Make `sum` lazy](#make-sum-lazy)
* [Step 3 - Define Y with Extract Method](#step-3---define-y-with-sxtract-method)
  * [Inject `MkSum` as a parameter](#inject-mksum-as-a-parameter)
  * [Move laziness from `sum` to `Y`](#move-laziness-from-sum-to-y)
  * [Replace `sum` with `Y(MkSum)`](#replace-sum-with-ymksum)
  
## Step 1 - An ordinary recursive function
We will get to Y progressively, transforming the original `sum` function with a series of refactoring moves. This means that, along the way, we will neither break the compilation nor the observable behavior of `sum`.<br/>

As a safety net, we can use a [FsCheck][fscheck] property test: it will make sure that, while refactoring, `sum` will always keep satisfying the well known [Gauss Formula][gauss-formula]:

![Gauss Formula](static/img/y-combinator-in-csharp/gauss.png)

```csharp
public class YCombinator
{
    private const int Max = 12_000;
    private readonly Arbitrary<int> PositiveNumbers = Arb.From(Gen.Choose(0, Max));
        
    private static readonly Func<int, int> sum =
        n =>
            n == 0 ? 0 : n + sum(n - 1);
    
    [Property]
    Property it_meets_the_gauss_formula() =>
        ForAll(PositiveNumbers, n =>
            sum(n) == n * (n + 1) / 2);
}
```

(For a fun introduction to Property Testing, I can only recommend the brilliant [The lazy programmer's guide to writing thousands of tests][scott-wlaschin-property-testing] by Scott Wlaschin)

Notice that we have to limit the test input space to the first `12.000` positive numbers: differently from F#, C# does not have any tail-recursion optimization; exceeding that value would overflow the stack, crashing the test with an exception.

### Type alias
Let's replace `Func<int, int>` with the type alias `Sum`, using a delegate:

```csharp
private delegate int Sum(int n);

private static readonly Sum sum =
    n =>
        n == 0 ? 0 : n + sum(n - 1);
```

Be aware though that, while delegates can make the code a bit less verbose, they get in the way of the compiler type inference.

## Step 2 - Inject a continuation
As we saw before, we can remove the recursion from `sum` by letting it invoke a continuation instead of itself.

The goal is to define a sum-generating function, `MkSum`, of type `Func<Sum, Sum>` that, given a continuation of type `Sum`, generates a sum function of type `Sum`.<br/>
Why are we doing this? Remember what we commented in [part 2 - Feeding itself with itself][feeding-itself-with-itself]: `MkSum` is able to create `sum`, if only it is given a sound continuation, and `sum` *is* by definition a sound continuation. The idea is then to feed `MkSum` with its own result.<br/>
Of course, in order to do that, we have to make `MkSum` a thing, we need to *extract* it.


### Define `MkSum` with Extract Method
Let's apply [Extract Method][extract-method] to `n => n == 0 ? 0 : n + sum(n - 1)` to define `MkSum`:
```csharp
delegate int Sum(int n);

static readonly Sum sum =
    MkSum();
    
static Sum MkSum() =>
    n =>
        n == 0 ? 0 : n + sum(n-1);
```

This creates what [Matteo Baglini][matteo-baglini] calls a bottleneck: a single place in the source code from which to operate a refactoring change.<br/>


### Let `sum` float up
We have to replace `sum` in `MkSum` with a continuation. Let's pull it up applying [Introduce Parameter][introduce-parameter]:

```csharp
delegate int Sum(int n);

static readonly Sum sum =
    MkSum(sum);
    
static Sum MkSum(Sum continuation) =>
    n =>
        n == 0 ? 0 : n + continuation(n-1);
```

We still have recursion, but somehow we highlighted that we are using `sum` as a continuation.<br/>

### Make `sum` lazy
Surprisingly, running the test we get a `NullReferenceException`. Ideally, an automated refactoring move performed by the IDE should be guaranteed to preserve the behavior. As we see, this is not always the case.<br/>
The problem here is the way `sum` is defined: it's a field, not a method.

```csharp
static readonly Sum sum =
    MkSum(sum);
```

When the value of `sum` is being evaluated, `MkSum` is invoked with its parameter `sum`, which is fatally still `null`. That's a consequence of C#'s eager evaluation of statements. We can make `sum` lazy defining it as a method or, alternatively, as a lambda, abandoning the point-free style:

```csharp
delegate int Sum(int n);

static readonly Sum sum = 
    n =>
        MkSum(sum)(n);
    
static Sum MkSum(Sum continuation) =>
    n =>
        n == 0 ? 0 : n + continuation(n-1);
```

Unfortunately, the test will still fail, this time for a stack overflow. In fact, we added one extra layer of indirection, which increased the stack usage. We need to compensate the problem shrinking the range of numbers used by the property test:


```csharp
public class YCombinator
{
    private const int Max = 8_000;
    private readonly Arbitrary<int> PositiveNumbers = Arb.From(Gen.Choose(0, Max));
        
    [...]
```

This passes the test.<br/>

OK. It's time to finally define `Y`.

## Step 3 - Define Y with Extract Method
Let's create again a bottleneck, applying [Extract Method][extract-method] to `MkSum(sum)`:


```csharp
delegate int Sum(int n);
delegate Sum MkSum(Sum continuation);
    
static Sum MkSum(Sum continuation) =>
        n =>
            n == 0 ? 0 : n + continuation(n-1);

static Sum Y() =>
    MkSum(sum);

static readonly Sum sum =
    n =>
        Y()(n);
```

### Inject `MkSum` as a parameter
In the [second installment][part-2] we described `Y` as a function to be used as:

```csharp
Sum sum = Y(quasi_sum);
```

We can get to this applying [Introduce Parameter][introduce-parameter] to `MkSum`:

```csharp
static Sum MkSum(Sum continuation) =>
    n =>
        n == 0 ? 0 : n + continuation(n-1);

static Sum Y(Func<Sum, Sum> f) =>
    f(sum);

static readonly Sum sum =
    n =>
        Y(MkSum)(n);
```

### Move laziness from `sum` to `Y`
We can make `Y` lazy and revert `sum` to eager and point-free:

```csharp
static Sum MkSum(Sum continuation) =>
        n =>
            n == 0 ? 0 : n + continuation(n-1);

static Sum Y(Fun<Sum, Sum> f) =>
    n =>
        f(sum)(n);

static readonly Sum sum =
    Y(MkSum);
```



### Replace `sum` with `Y(MkSum)`
We are almost there. Look closely to `Y`. It's defined as:

```csharp
static Sum Y(Func<Sum, Sum> f) =>
    n =>
        f(sum)(n);
```

We have to eliminate that `sum`. This is trivial, once we notice that `sum` is: 

```csharp
static readonly Sum sum =
    Y(MkSum);
```

If `sum = Y(MkSum)` and `Y = f(sum)`, then `Y = f(Y(MkSum))`, and since `f = MkSum`, this can be finally simplified as `Y = f(Y(f))`.

Ideally, we could do this applying [Inline Method][inline-method]. Unfortunately, we have to do this manually: R# has got [a bug][jetbrains-bug] and does not allow inlining only one specific usage.

```csharp
delegate int Sum(int n);
        
static Sum MkSum(Sum continuation) =>
    n =>
        n == 0 ? 0 : n + continuation(n-1);

static Sum Y(MkSum f) =>
    n =>
        f(Y(f))(n);

static readonly Sum sum =
    Y(MkSum);
```

As a field, `Y` becomes:


```csharp
static Func<Func<Sum, Sum>, Sum> Y = f => n => f(Y(f))(n);
```

We made it. This is our coveted (recursive) Y Combinator.

## Conclusion
Does it work? The tests are still green.

Let's see if it allows us to use a recursive sum function as an anonymous lambda.<br/>
Remember when we got stuck with the following?

```csharp
var result = new [] { 0, 1, 2, 3, 4 }.Select(n => n == 0 ? 0 : n + ???(n - 1));

result.Should().BeEquivalentTo(new [] { 0, 1, 3, 6, 10 });
```

Let's verify how our brand new recursive Y Combinator helps here. Our `sum` is:

```csharp
Sum sum =  Y(MkSum);
```

or, expanded:

```csharp
Sum sum =  Y( f => n => n == 0 ? 0 : n + f(n-1) );
```

Used as an anonymous function:

```csharp
var result = 
    new [] { 0, 1, 2, 3, 4 }
        .Select(i =>
            Y(f => n => n == 0 ? 0 : n + f(n-1))(i));

result.Should().BeEquivalentTo(new [] { 0, 1, 3, 6, 10 });
```

Cool. We are actually using a recursive-like function (defined with [Continuation Passing Style][continuation-passing-style]) as an anonymous lambda.

If you want to simplify to:

```csharp
    new [] { 0, 1, 2, 3, 4 }.Select(
        Y(f => n => n == 0 ? 0 : n + f(n-1));
```

you have to give up the `Sum` type alias, which makes it clear that C# delegates are a poor way to aliasing types.


## Non-recursive Y Combinator
Is that all?<br/>
Meh. I don't know you, but I feel like I cheated. Afterall, we extracted the recursion away from the original function, only to push it inside `Y`. It *is* a result. But it stinks, it's sweeping the dust under the carpet. We can surely do better: we can remove the recursion altogether, distilling a stricly non-recursive Y Combinator.

This will be a bit more challenging. Take a deep breath, have a beer and when you are ready, jump to [the forth and last installment][part-4].



## References


* [JetBrains Refactorings](https://www.jetbrains.com/help/resharper/Main_Set_of_Refactorings.html)
  * [Extract Method][extract-method]
  * [Introduce Parameter][introduce-parameter]
  * [Inline Method][inline-method]
* [FsCheck][fscheck]
* [Matteo Baglini - Twitter][matteo-baglini]
* [Continuation Passing Style - Wikipedia][continuation-passing-style]
* [JetBrains Bug RSRP-439279:  Inlining fields in usage causes all usages to be inlined][jetbrains-bug]
* [Scott Wlaschin - The Lazy Programmer's Guide To Writing Thousands Of Tests][scott-wlaschin-property-testing]
* [Carl Friedrich Gauss - Anecdotes - Wikipedia][gauss-formula]
* [Tacit Programming - Wikipedia][point-free]

[part-1]: y-combinator-in-csharp
[part-2]: y-combinator-in-csharp-part-2
[feeding-itself-with-itself]: y-combinator-in-csharp-part-2#feeding-itself-with-itself


[extract-method]: https://www.jetbrains.com/help/resharper/Refactorings__Extract_Method.html
[introduce-parameter]: https://www.jetbrains.com/help/resharper/Refactorings__Introduce_Parameter.html
[inline-method]: https://www.jetbrains.com/help/resharper/Refactorings__Inline_Method.html
[fscheck]: https://fscheck.github.io/FsCheck/
[matteo-baglini]: https://twitter.com/matteobaglini
[continuation-passing-style]: https://en.wikipedia.org/wiki/Continuation-passing_style
[jetbrains-bug]: https://youtrack.jetbrains.com/issue/RSRP-439279/Inlining-fields-in-usage-causes-all-usages-to-be-inlined
[scott-wlaschin-property-testing]: https://www.youtube.com/watch?v=IYzDFHx6QPY&t=9s
[gauss-formula]: https://en.wikipedia.org/wiki/Carl_Friedrich_Gauss#Anecdotes
[point-free]: https://en.wikipedia.org/wiki/Tacit_programming

[just-show-me-the-code]: y-combinator-in-csharp-code-only#recursive-y-combinator
[part-1]: y-combinator-in-csharp
[part-2]: y-combinator-in-csharp-part-2
[part-4]: y-combinator-in-csharp-part-4
[feeding-itself-with-itself]: y-combinator-in-csharp-part-2#feeding-itself-with-itself




---
layout: post
title: "Deriving the Y Combinator in C# - Just the code"
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
* [Part 4 - Non-recursive Y Combinator][part-4]

# Recursive Y Combinator

## Step 1 - An ordinary recursive function
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

### Type alias
```csharp
private delegate int Sum(int n);

private static readonly Sum sum =
    n =>
        n == 0 ? 0 : n + sum(n - 1);
```

## Step 2 - Inject a continuation
### Define `MkSum` with Extract Method
```csharp
delegate int Sum(int n);

static readonly Sum sum =
    MkSum();
    
static Sum MkSum() =>
    n =>
        n == 0 ? 0 : n + sum(n-1);
```

### Let `sum` float up

```csharp
delegate int Sum(int n);

static readonly Sum sum =
    MkSum(sum);
    
static Sum MkSum(Sum continuation) =>
    n =>
        n == 0 ? 0 : n + continuation(n-1);
```

### Make `sum` lazy
```csharp
delegate int Sum(int n);

static readonly Sum sum = 
    n =>
        MkSum(sum)(n);
    
static Sum MkSum(Sum continuation) =>
    n =>
        n == 0 ? 0 : n + continuation(n-1);
```

### Reduce recursion
```csharp
public class YCombinator
{
    private const int Max = 8_000;
    private readonly Arbitrary<int> PositiveNumbers = Arb.From(Gen.Choose(0, Max));
        
    [...]
```

## Step 3 - Define Y with Extract Method
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
```csharp
delegate int Sum(int n);
delegate Sum MkSum(Sum continuation);
    
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
```csharp
delegate int Sum(int n);
delegate Sum MkSum(Sum continuation);
    
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

### Convert `Y` to field

```csharp
delegate int Sum(int n);

static Sum MkSum(Sum continuation) =>
    n =>
        n == 0 ? 0 : n + continuation(n-1);

static Func<Func<Sum, Sum>, Sum> Y = 
    f => 
        n => f(Y(f))(n);

static readonly Sum sum =
    Y(MkSum);
```


[part-1]: y-combinator-in-csharp
[part-2]: y-combinator-in-csharp-part-2
[part-3]: y-combinator-in-csharp-part-3


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

### Type alias
```csharp
private delegate int Sum(int n);

private static readonly Sum sum =
    n =>
        n == 0 ? 0 : n + sum(n - 1);
```
## Step 2 - Inject a continuation

### Define `MkSum` with Extract Method
```csharp
delegate int Sum(int n);

static readonly Sum sum =
    MkSum();
    
static Sum MkSum() =>
    n =>
        n == 0 ? 0 : n + sum(n-1);
```

### Let `sum` float up

```csharp
delegate int Sum(int n);

static readonly Sum sum =
    MkSum(sum);
    
static Sum MkSum(Sum continuation) =>
    n =>
        n == 0 ? 0 : n + continuation(n-1);
```

### Make `sum` lazy

```csharp
private const int Max = 8_000;

private readonly Arbitrary<int> PositiveNumbers = Arb.From(Gen.Choose(0, Max));
    
delegate int Sum(int n);

static readonly Sum sum = 
    n =>
        MkSum(sum)(n);
    
static Sum MkSum(Sum continuation) =>
    n =>
        n == 0 ? 0 : n + continuation(n-1);
```

## Step 3 - Define Y with Extract Method
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
```csharp
delegate int Sum(int n);
delegate Sum MkSum(Sum continuation);
    
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
```csharp
delegate int Sum(int n);
delegate Sum MkSum(Sum continuation);
    
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



[part-1]: y-combinator-in-csharp
[part-2]: y-combinator-in-csharp-part-2
[part-3]: y-combinator-in-csharp-part-3
[part-4]: y-combinator-in-csharp-part-4


# Non-recursive Y Combinator

* [Step 1 - Extract Y to a local function](#step-1---extract-y-to-a-local-function)
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


## Step 1 - Extract `Y` to a local function
### Make `n => f(Y(mkSum))(n)` a variable
```csharp
private static Sum Y(Func<Sum, Sum> f)
{
    Sum sum1 = n => f(Y(f))(n);
    return sum1;
}
```

### Extract a local function
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

## Step 2 - Replace `Y(f)` with `sub()`
```csharp
private static Sum Y(Func<Sum, Sum> f)
{
    Sum sub() =>
        n => f(sub())(n);

    return sub();
}
```

## Step 3 - Inject self
```csharp
private delegate Sum Rec(Rec self);

private static Sum Y(Func<Sum, Sum> f)
{
    Sum sub(Rec self) =>
        n => f(sub(self))(n);

    return sub(sub);
}
```


### Replace `sub` with `self`
```csharp
private delegate Sum Rec(Rec self);

private static Sum Y(Func<Sum, Sum> f)
{
    Sum sub(Rec self) =>
        n => f(self(self))(n);

    return sub(sub);
}
```
    
## Step 4 - Replace variable with lambda
### Replace `sub(sub)` with lambda
```csharp
private static Sum Y(Func<Sum, Sum> f)
{
    Sum sub(Rec self) =>
        n => f(self(self))(n);

    return Func<Rec, Sum>(f => f(f))(sub);
}
```

## Step 5 - Inline `sub`
```csharp
private static Sum Y(Func<Sum, Sum> f) =>
    new Func<Rec, Sum>(f =>
        f(f))
    (self =>
        n =>
            f(self(self))(n));
```


[part-1]: y-combinator-in-csharp
[part-2]: y-combinator-in-csharp-part-2
[part-3]: y-combinator-in-csharp-part-3
[part-4]: y-combinator-in-csharp-part-4

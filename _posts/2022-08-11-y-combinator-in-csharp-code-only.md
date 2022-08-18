---
layout: post
title: "Implementing the Y Combinator in C# - Just the code"
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
* Part 4 - Non-recursive Y Combinator

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

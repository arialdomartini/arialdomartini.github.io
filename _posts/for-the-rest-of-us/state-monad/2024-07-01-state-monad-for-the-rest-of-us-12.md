---
layout: post
title: "State Monad For The Rest Of Us - Part 7"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- F#
- Functional Programming
include_in_index: false
---
Source code:
[github.com/arialdomartini/state-monad-for-the-rest-of-us][source-code].

Here's what you got in [Chapter
11](state-monad-for-the-rest-of-us-11):

```fsharp
let rec index =
    function
    | Leaf(v: string) ->
        getCount
        >>= (fun count ->
            let leaf = Leaf(v, count)
            putCount (count + 1)
            >>= (fun _ -> pure' leaf))
    | Node(l, r) ->
        index l >>= (fun ll ->
            index r
             >>= (fun rr ->
                 pure' (buildNode ll rr)))
```

Remember again what `>>=` does:

* You feed it with a `WithCount v`.
* It continues the execution with a lambda just taking a naked `v`.

`>>=` is that magic function that moves `WithCount` out of sight,
while still accounting for it under the scenes.

In fact, the source of mess is the necessary presence of that lambda.
If you had a function `foo` returning a `WithCount String`:

```fsharp
// () -> WithCount String
let foo () = WithCount "hello"
```

how beautiful would it be if you could replace:

```fsharp
foo() >>= fun (v -> ...)
```

with just:

```fsharp
let! v = foo()
```

with a special `let!` binding able to unwrap any `WithCount` value,
and directly assigning the naked value `"hello"` to `v`?  
If only you had that `let!` binding, the whole `index` function would
drammatically simplify. Instead of:

```fsharp
let rec index =
    function
    | Leaf(v: string) ->
        getCount
        >>= (fun count ->
            let leaf = Leaf(v, count)
            putCount (count + 1)
            >>= (fun _ -> pure' leaf))
    | Node(l, r) ->
        index l >>= (fun ll ->
            index r
             >>= (fun rr ->
                 pure' (buildNode ll rr)))
```

you could operate the following transformations:

| Original expression                 | Special form                    |
|-------------------------------------|---------------------------------|
| `getCount >>= fun count ->`         | `let! count = getCount`         |
| `putCount (count + 1) >>= fun _ ->` | `let! _ = putCount (count + 1)` |
| `index l >>= fun ll ->`             | `let! ll = index l`             |
| `index r >>= fun rr ->`             | `let! rr = index r`             |


and the whole `index` function would look like:

```fsharp
let rec index =
    function
    | Leaf(v: string) ->
        let! count = getCount
        let leaf = Leaf(v, count)
        let! _ = putCount (count + 1)
        pure' leaf
    | Node(l, r) ->
        let! ll = index l
        let! rr = index r
        pure' (buildNode ll rr)
```

Oh, this would be really a game changer, wouldn't it? It would allow
you to *think imperatively*, while *being purely functional*.

Of course, if that `let!` binding existed, we would expect that under
the hood the F# compiler would automatically convert any:

```fsharp
let! vv = v
...
```

into:

```fsharp
v >>= (fun vv -> ...)
```

Being a mechanical, deterministic transformation, that should not be an impossible dream.

## Enter Computation Expressions
It turns out that F# does support that syntax. All you have to do is
to teach it which `>>=` and `pure'` functions to use for those
tranformations. You need to create a custom type and to implement a
couple of methods:

```fsharp
type WithCountExpression() =
    member this.Return v = failwith "Not yet implemented" 
    member this.Bind v f = failwith "Not yet implemented" 
```

`Return` is an alias name for `pure'`. `Bind`, as you already know, is
the name of `>>=`. So, after all, you should already know how to
implement both:

```fsharp
type WithCountExpression() =
    member this.Return(v) = pure' v 
    member this.Bind(v, f) = v >>= f 
```

Now, just create an instance of it:

```fsharp
let withCount = WithCountExpression()
```

and, voilà!, you can use the new syntax:

```fsharp
let rec index =
    function
    | Leaf v ->
        withCount {
            let! count = getCount
            let leaf = Leaf (v, count)
            let! _ = putCount (count + 1)
            return leaf
        }
    | Node(l, r) ->
        withCount {
            let! ll = index l
            let! rr = index r
            return buildNode ll rr
        }
```

This is *mostly* what we initially desired, besides the extra
`withCount { }` wrapping the whole epxression. The reason why this is
needed is because you can have multiple monads, and for each of them
`let!` and `return` must be transformed to a specific implementation
of `>>=` and `pure'`. This is a problem Haskell does not have: it
provides you with one single `do` expression, covering all the
possible existing and future monads.

## Computation Expression
What you just implemented is called [Computation
Expression][computation-expressions]. It is equivalent to (and
possibly more powerful than) the Haskell [do notation][do-notation],
which would allow writing `index` as:

```haskell
index (Leaf v) =
    do {
        count <- getCount
        let leaf = Leaf (v, count)
        putCount (count + 1)
        return leaf
    }
index (Node l r) =
    do {
        let! ll = index l
        let! rr = index r
        return buildNode ll rr
    }
```

You see the similarity, I hope!

Computation Expressions are super powerful, and they are pervasive in
F#: they can handle async code, error management, optional values,
sequence generations, logging etc. After all, they are syntactic sugar
around arbitrary monads.

## More sugar, please!
A little trick: when you want to ignore a value, you can always
replace `let! _ = ...` with `do! ...`. So, you can write `index` as:


```fsharp
let rec index =
    function
    | Leaf v ->
        withCount {
            let! count = getCount
            let leaf = Leaf (v, count)
            do! putCount (count + 1)
            return leaf
        }
    | Node(l, r) ->
        withCount {
            let! ll = index l
            let! rr = index r
            return buildNode ll rr
        }
```

Naturally, it is again just syntactic sugar. Any:

```fsharp
do! foo()
...
```

is replaced with:

```fsharp
foo() >>= (fun _ -> ...)
```

## It's all fake imperative style
Let me stress that, although expressions like:

```fsharp
withCount {
  do! putCount 42
  do! putCount 0
  do! putCount 99
  return "Hello, world!"
}
```

look like a series of *statements*, under the hood each `do!` like is
an expression *bound` to the next one: the whole is a unique
expression, in a chain of lambdas:

```fsharp
putCount 42 >>= (fun _ ->
      putCount 0 >>= (fun _ ->
          putCount 99 >>= (fun _ ->
              pure' "Hello, world!")))
```

Whatever you write inside a `withCount` is a single, purely functional
expression. 

## What about LINQ?
In the index I claimed that this chapter would help you see LINQ for
what it is: a monadic engine. It you always saw LINQ as an embedded
SQL language, consider this implementation of `index`, converted from
F# to C#:

```csharp
private static WithCount<Tree<(A, int)>> IndexLINQ<A>(Tree<A> tree) =>
    tree switch
    {
        Tree<A>.Leaf(var v) =>
            from count in GetCount
            from _ in PutCount(count + 1)
            select BuildLeaf(v, count),
            
        Tree<A>.Node(var l, var r) =>
            from ll in Index(l)
            from rr in Index(r)
            select BuildNode(ll, rr)
    };
```

It should not be hard to see these mappings:

| F# syntax                    | C# LINQ syntax                |
|------------------------------|-------------------------------|
| `let! v = monadicFunction()` | `from v in monadicFunction()` |
| `return v`                   | `select v`                    |

Just like F#, in C# you can teach LINQ how to monadically handle
`WithCount` values, instructing it which `>>=` / `Bind` methods to
use. In C# the way to go is with Extension Methods:

```csharp
internal static class WithCountExtensions
{
    internal static WithCount<T> Pure<T>(T value) => new(count => (value, count));

    internal static (T, int) Run<T>(WithCount<T> value, int count) => value.F(count);

    internal static WithCount<TResult> Bind<T, TResult>(WithCount<T> a, Func<T, WithCount<TResult>> f) =>
        new(count =>
        {
            var (va, ca) = Run(a, count);
            var result = f(va);
            return Run(result, ca);
        });

    internal static WithCount<TResult> SelectMany<T, TIntermediate, TResult>(
        this WithCount<T> withCount,
        Func<T, WithCount<TIntermediate>> intermediateSelector,
        Func<T, TIntermediate, TResult> resultSelector) =>
        new(count =>
        {
            var (value, intermediateCount) = withCount.F(count);
            var intermediateWithCount = intermediateSelector(value);
            var (intermediateValue, finalCount) = intermediateWithCount.F(intermediateCount);
            return (resultSelector(value, intermediateValue), finalCount);
        });
}
```

Yes, it's way less readable than the equivalent F# version. There's a
reason why functional programmers prefer F# over C#.  
You can find the complete working C# implementation in
[StateMonadTest.cs][csharp-porting].

Anyway, here's the take away: LINQ is much more than a tool for
embedding SQL and manipulating lists. LINQ is syntactic sugar for
monads, any monad, just like the Haskell's do notation and F#
Computation Expressions. LINQ is indeed a monad engine.

You can read more about it in [Thinking Functionally: What is LINQ
really?][thinking-functionally], from the
[language-ext][language-ext]'s wiki.

# References
* [State Monad For The Rest Of Us - source code][source-code]
  * [Porting in C#][csharp-porting]
* [Computation Expressions - Microsoft][computation-expressions]
* [Haskell's *do notation*][do-notation]
* [Thinking Functionally: What is LINQ really?][thinking-functionally]
* [language-ext][language-ext]

# Comments
[GitHub Discussions][discussions]


[discussions]: https://github.com/arialdomartini/arialdomartini.github.io/discussions/30
[source-code]: https://github.com/arialdomartini/state-monad-for-the-rest-of-us
[computation-expressions]: https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions
[do-notation]: https://en.wikibooks.org/wiki/Haskell/do_notation
[csharp-porting]: https://github.com/arialdomartini/state-monad-for-the-rest-of-us/blob/master/src/StateMonadCSharp/StateMonadTest.cs
[thinking-functionally]: https://github.com/louthy/language-ext/wiki/Thinking-Functionally:-What-is-LINQ-really%3F
[language-ext]: https://github.com/louthy/language-ext

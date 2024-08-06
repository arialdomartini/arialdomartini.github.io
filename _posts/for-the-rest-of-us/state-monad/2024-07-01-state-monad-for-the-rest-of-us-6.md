---
layout: post
title: "State Monad For The Rest Of Us - Part 6"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- F#
- Functional Programming
include_in_index: false
---
[Index](state-monad-for-the-rest-of-us)  
Source code:
[github.com/arialdomartini/state-monad-for-the-rest-of-us][source-code].

## Red pill, blue pill

Welcome back! I hope your tea (or your whiskey and cigar) inspired
you. We left each other in [chapter
5](state-monad-for-the-rest-of-us-5) with an existential question: 

Where does `count` come from in this hypothetical, scaffold implementation?

```fsharp
let rec index =
    function
    | Leaf v -> Leaf (v, count)
    | Node(l, r) -> failwith "Not yet implemented"
```

There are in fact 2 legit answers:

1. It is a **global variable**

    ```fsharp
    let count = 1

    let rec index =
        function
        | Leaf v -> Leaf (v, count)
        | Node(l, r) -> failwith "Not yet implemented"
    ```

2. It is a **function parameter**:

    ```fsharp
    let rec index count =
        function
        | Leaf v -> Leaf (v, count)
        | Node(l, r) -> failwith "Not yet implemented"
    ```

This is a very strong design decision.

* If you go with a global variable (or just a variable defined in an
outer scope), your code is destined to be imperative.
* If you go with a function parameter, you have taken the direction of
FP, and you are likely to stumble on monads.

## Outer Scope Variable
The implementation with a mutable variable is deliciously simple:

```fsharp
let mutable count = 1

let rec imperativeIndex =
    function
    | Leaf v ->
        let indexedLeaf = Leaf (v, count)
        count <- count + 1
        indexedLeaf
    | Node(l, r) -> Node(imperativeIndex l, imperativeIndex r)
```

It really reminds the impure mapping function we wrote in [chapter
3](state-monad-for-the-rest-of-us-3), it's basically the same idea.

We could say a lot about this style, for example:

* It leads to thread-unsafe code. Two concurrent invocations will race
  for the out-of-scope, shared state.
* Like every algorithm based on state mutation, reasoning about it
  requires the reader to unroll mentally the execution, keeping track
  of the state. This specific code is simple enough, but clearly this
  approach does not scale.
  
But this is the old litany every functional militant would repeat over
and over. What I would like you to notice, instead, is:

* F# is not a pure functional language and if you opt for mutability,
  it is tollerant.
* But its powerful Type Sytem will abandon you to your fate, and will
  not do much more to help you.
  
Throughout this series, every time we managed to have a compiling
code, that was a reliable sign that the functionality was complete and
correct, and the green test was there to testify it. It's not the case
here anymore. The very moment you resolve the dependency to
`count` with an shared mutable variable, as far as the compiler is
concerned, the code is alredy complete and correct.

I like to interpret the apathy we are getting from the Type Sytem as
if it is telling us:

"So you are not giving me any hints about the types at play. Fine,
your loss, go ahead and do it your way".

Is there a way to give the Type Sytem a hint about the types at play?
Yes, there is! And it is exactly the second option you found: passing
`count` as a function parameter. 

## Function Parameter
Here we go.

```fsharp
let rec index count =
    function
    | Leaf v -> Leaf (v, count)
    | Node(l, r) -> failwith "Not yet implemented"
```

You might have missed it, but you just changed the `index` signature,
that is, its type. Exactly what the Type Sytem was moaning about.

| Original signature        | New signature                    |
|---------------------------|----------------------------------|
| `Tree a -> Tree (a, Int)` | `Tree a -> Int -> Tree (a, Int)` |

Although the change is as simple as before (you just added a humble
`count`), the Type Sytem will not let you down. On the contrary, it
immediately highlights all the lines where your code fails to compile.

Notice: a compilation error is not a harm. On the contrary: it's a
gift from Heaven, it's a lighthouse to follow. Instead of being
left to sink or swim, the Type Sytem is there to suggest you how to
complete your implementation. Beautiful.

Before going ahead, notice that there were 3 positions where you could
have placed `count` as a function parameter:

1. As the first `index` parameter:

    ```fsharp
    // Int -> Tree a -> Tree (a, Int)
    let rec index count tree=
        match tree with
        | Leaf v -> Leaf (v, count)
        | Node(l, r) -> failwith "Not yet implemented"
    ```

2. As the second `index` parameter:

    ```fsharp
    // Tree a -> Int -> Tree (a, Int)
    let rec index tree count =
        match tree with
        | Leaf v -> Leaf (v, count)
        | Node(l, r) -> failwith "Not yet implemented"
    ```

3. As the parameter of a nested lambda:

    ```fsharp
    // Tree a -> (Int -> Tree (a, Int))
    let rec index tree =
        match tree with
        | Leaf v -> fun count -> Leaf (v, count)
        | Node(l, r) -> failwith "Not yet implemented"
    ```


I would like to convince you that the 3rd option is the most
convenient, because it better exhibits the mental metaphor of monads.


## Learn to procrastinate
In [Monads for the Rest of Us][monads-for-the-rest-of-us] I postulate
that an intuitive interpretation of (some) monads is that they
encapsulate the act of postponing an execution of some (side) effects.
This viewpoint stands also in this case.  
Focus on the 3rd option signature and on its leaf branch
implementation:

```fsharp
// Tree a -> (Int -> Tree (a, Int))
let rec index =
    function
    | Leaf v -> fun count -> Leaf (v, count)
    ...
```

I like to interpret it as it's telling me:

> Give me a Tree and I *promise* I will index it. I cannot index it
> just yet, because I have no idea which value `count` has. So, I give
> you back a function for you to feed with that value. As soon as you
> do that, I will complete my indexing task.

It's really like this: `index` is not returning an indexed leaf but a
function that *eventually* will create it. `index` is blatantly
tricking you, procrastinating its duties. This is often the case with
monads.

Of course, now you cannot invoke `index` anymore with:

```fsharp
[<Fact>]
let ``indexes a tree`` () =
    let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))
    
    let indexed = index tree

    test <@ indexed = Node(Leaf ("one", 1), Node(Leaf ("two", 2), Leaf ("three", 3))) @>
```

You need to pass it the first `count` value:


```fsharp
let indexed = index tree 1
("three", 3))) @>
```

So far so good. Let's see how to complete the node branch.

## Node Branch
The node branch is in the same dilemma. It wants to complete its task
but it does not know the value of `count`. So, it uses the same trick:
it returns a function that eventually will perform the indexing:

```fsharp
let rec index =
    function
    | Leaf v -> fun count -> Leaf (v, count)
    | Node (l, r) ->
        fun count ->
            ...
```			

This is a very common trick in Functional Programming: if you need a
value and it's not there, you just create it from thin air, returning
a function that requires it. It will be your caller's duty to provide
it. Doing so, you are propagating up to the call site, and through the
Type Sytem, your need. The Type Sytem will make sure that this need
is not ignored.

Cool. You have `count` out of thin air. You can index the left branch,
then the right one. Then you can build a `Node` with the resuls:

```fsharp
let rec index =
    function
    | Leaf v -> fun count -> Leaf (v, count)
    | Node (l, r) ->
        fun count ->
            let li = index l count
            let ri = index r count
            Node (li, ri)
```			

`li` and `ri` stand for `left, indexed` and `right, indexed`.

Wait a sec. There something not quite correct. The code compiles, but
the test is red. For the first time, "if it compiles, it works" failed
us.

Can you spot the problem? We are never incrementing the `count`. This
is an issue at value level, not at type level, and the poor type
system cannot infer anything but at type level. (I argue: this is the
sign we should have improved the design using more fine tuned types,
such as Dependent Types. But this is a very advanced topic deserving
its own series).

Maybe we can increment `count` right after having indexed the left
branch:

```fsharp
let rec index =
    function
    | Leaf v -> fun count -> Leaf (v, count)
    | Node (l, r) ->
        fun count ->
            let li = index l count
            let ri = index r (count + 1)
			Node (li, ri)
```			

But this is wrong. This assumes that the left branch only had 1 leaf.
We should increment `count` with the exact number of leaves in the
left branch:


```fsharp
let rec index =
    function
    | Leaf v -> fun count -> Leaf (v, count)
    | Node (l, r) ->
        fun count ->
            let li = index l count
            let ri = index r (count + numberOfLeavesInLeftBranch)
			Node (li, ri)
```			

Shall we write a function to calculate it? Shall we add another
recursive call?  

There is a way simpler solution. In the mutable implementation we wrote before:

```fsharp
let mutable count = 1

let rec imperativeIndex =
    function
    | Leaf v ->
        let indexedLeaf = Leaf (v, count)
        count <- count + 1
        indexedLeaf
    | Node(l, r) -> Node(imperativeIndex l, imperativeIndex r)
```

we rightly incremented `count` in the leaf node. Let's try to follow
the same path.

The question is: how to increment `count` in the immutable version?
After all, it must be immutable...

## Inherit from the past, pass it on to the future
Another useful perspective on returning a `fun count ->` is to view it
as a communication channel. it's a way for the code to send
information back to its caller, back to the past. It says:

> I don't know who used to run before me,
> but it had to pass me the correct value of `count`

You can use the same trick to send information forward to the future
code. You can send it the updated value of `count`. Since you have to
return both the indexed tree *and* the updated value of `count`, a
tuple comes to mind:

```fsharp
// Tree a -> (Int -> (Tree (a, Int), Int))
let rec index =
    function
    | Leaf v -> fun count -> (Leaf (v, count), count + 1)
    | Node (l, r) ->
        fun count ->
            let li = index l count
            let ri = index r count
            Node (li, ri)
```

Notice how the leaf branch is returning 2 information:

| Indexed tree      | Future value for `count` |
|-------------------|--------------------------|
| `Leaf (v, count)` | `count +1`               |

Also notice that, again, the whole signature of `index`, so its type!,
changed. And this immediately sets off alarms bells for the type
system. That, ideed, signals that the node branch fails to compile. Of
course! 

```fsharp
// li :: Tree (a, Int), Int
let li = index l count
```

`li` now is not an indexed tree anymore. It's an indexed tree *plus*
the updated value for `count`, calculated by traversing the left
branch. Useful. Let's decompose it:

```fsharp
// Tree a -> (Int -> (Tree (a, Int), Int))
let rec index =
    function
    | Leaf v -> fun count -> (Leaf (v, count), count + 1)
    | Node (l, r) ->
        fun count ->
            let li, lc = index l count
            let ri, rc = index r count
            Node (li, ri)
```

`lc` stands for `left count`; `rc` for `right count`.  
Still not compiling. *Good!* There are 2 problems:

1. Why are we indexing the right branch starting from the value
   `count`?
2. The return value is wrong.


```fsharp
// Tree a -> (Int -> (Tree (a, Int), Int))
let rec index =
    function
    | Leaf v -> fun count -> (Leaf (v, count), count + 1)
    | Node (l, r) ->
        fun count ->
            let li, lc = index l count
            let ri, rc = index r lc
            Node (li, ri)
```


As for 1, this is very simple. After indexing the left branch, do we
have the updated value of `count`? Yes we have, it's `lc`, the left
count:

As for 2, the Type Sytem complains that we should return a tuple, not
an indexed tree only. Sure, let's return the count too. Which is the
most updated value? `rc`, of course, the value the indexing of the
right branch conveniently sent to its future clients:

```fsharp
// Tree a -> (Int -> (Tree (a, Int), Int))
let rec index =
    function
    | Leaf v -> fun count -> (Leaf (v, count), count + 1)
    | Node (l, r) ->
        fun count ->
            let li, lc = index l count
            let ri, rc = index r lc
            Node (li, ri), rc
```

There is still a last compilation error (thank you, Type System, for
being so vigilant and meticulous):

```fsharp
[<Fact>]
let ``indexes a tree`` () =
    let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))

    let indexed = index tree 1

    test <@ indexed = Node(Leaf ("one", 1), Node(Leaf ("two", 2), Leaf ("three", 3))) @>
```

Yes! `index tree 1` returns the indexed tree *and* a new counter,
which we can safely ignore:


```fsharp
[<Fact>]
let ``indexes a tree`` () =
    let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))

    let indexed, _ = index tree 1

    test <@ indexed = Node(Leaf ("one", 1), Node(Leaf ("two", 2), Leaf ("three", 3))) @>
```

It compiles. It's green. We did it! Look how beautiful: you did a
stateful algorithm without any single variable.

## And?
And it's inherently threadsafe. It was guided by the type system.

And best of all, it's so readable!

## Arialdo, go home, you are drunk
You are right: the result is horrible. It's a contrived, convoluted
gimmick.

Let me enumerate its ugliest parts:

1. Pass it to your fellow Java colleague. I bet it will take at least
   10 minutes for them to grok it. It's not readable: it's abominable.
2. Its complexity is doomed to increase. Any real-world application
   has tens, hundreds of states to handle. Handling a single state
   made our simple algorithm convoluted and it is likely that with 10
   additional states the complexity would increase more than linearly.
   This approach just does not scale.
3. The signature too is complex and does not convey a clear intent.

## Is there a way out?
Yes, there is! And it can be found as soon as you identify the root
cause:

* The domain logic and the logic for handling the state are
  interleaved.
  
Sometimes coding is so predictable, isn't it? It's always the same old
problems. With approving smiles and nods, the Venerable Sages observe
us contentedly, implying: "it's all about the Single Responsibility ,
the Open Closed , the Dependency Inversion Principles . It's always a
matter of Low Coupling and High Cohesion".

So, you know how to get out of this mess: you have to separate the
domain logic from the state-handling logic.  
Once you separate them, what you will end up with is:

* An imperative-like, uncluttered and readable domain code.
* A magic State Monad, handling the statefulness nature of the problem
  behind the scenes.
  
Here's a spoiler alert:

```fsharp
let rec index =
    function
    | Leaf v ->
        withCount {
            let! count = getCount
            do! setCount (count + 1)
            return Leaf(v, count)
        }
    | Node(l, r) ->
        withCount {
            let! li = index l
            let! ri = index r
            return Node(li, ri)
        }

```

* Focus on the code enclosed by `{ }`: besides some weird `!` here and
  there, this is the old imperative code that you find easy to write.
* Notice the `withCount` expression: treat is as a new F# keyword that
  tells the compiler to treat the enclosed code as stateful.

That `withCount` is the State Monad (hidden behind a Computation
Expression).  
Notice that, although the code *seems* imperative, it is purely
functional. Even if:

```fsharp
let! li = index l
let! ri = index r
```

seem 2 sequential statements, they are in fact 2 functions bound
together with the `bind` operator. Puzzling, isn't it? I'm sure you
understand that, to fully get there, we stilllq have to walk a bit. I
promise that it will be an interesting journey!

You need a bit of energy. So stretch your fingers, take a little rest,
have a sorbet and then we will get going!

See you in [Chapter 7](state-monad-for-the-rest-of-us-7).


# References
* [Microsoft - Computation Expressions][computation-expressions]

[computation-expressions]: https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions
[source-code]: https://github.com/arialdomartini/state-monad-for-the-rest-of-us

# Comments
[GitHub Discussions][discussions]


[monads-for-the-rest-of-us]: https://arialdomartini.github.io/monads-for-the-rest-of-us
[discussions]: https://github.com/arialdomartini/arialdomartini.github.io/discussions/30


{% include fp-newsletter.html %}

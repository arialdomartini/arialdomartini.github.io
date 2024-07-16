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

[Two chapters ago](state-monad-for-the-rest-of-us-9), you wrote the
applicative version of `index` using `<*>` and `<*`. If you remember,
in [chapter 8](state-monad-for-the-rest-of-us-8) we said that there
are 2 series of moves:

* One leading to Applicative Functors.
* One leading to Monads.

You performed the former, which were:

1. Make the fact that each branch is invoking a function explicit.
2. Start moving that function inside a `WithCount`.

Now we try another approach:

1. Make the fact each branch is invoking a function explicit.
2. Take each argument and find a way to apply it to that function.

## Make Monads, not War
Easier said that done. You cannot apply those arguments to the
function. The compiler will not let you. Let us see why. This is the
starting point:

```fsharp
let rec index =
    function
    | Leaf v ->
        WithCount(fun count ->
            (buildLeaf v count, count + 1))
    | Node(l, r) ->
        WithCount(fun count ->
            let li, lc = run (index l) count
            let ri, rc = run (index r) lc
            (buildNode li ri), rc)
```

Focusing on the `Node` branch, the function we want to apply is
`buildNode`. Again: the goal is to get rid of the `count`-handling
code, and only have to work with domain logic. Ideally, you would like
to have:

```fsharp
let rec index =
    function
    | Node(l, r) ->
        buildNode (index l) (index r)
```

The problem is apparent as soon as you analize the types:


```fsharp
let rec index =
    function
    | Node(l, r) ->
	    // li :: WithCount (Tree v)
	    let li = index l
		
		// ri :: WithCount (Tree v)
		let ri = index t
	
	    // buildNode :: Tree v -> Tree v -> Tree v
        buildNode li ri
```

The compiler complains because you cannot feed a `WithCount (Tree v)`
to a function getting a naked `Tree v`. F#'s built-in function
application is not smart enough to handle `WithCount` instances. But
you can easily teach it.  
Just like you did before with the Applicative Functor, it's a matter
of implementing another on-steroids function appliction.

## Bind
Here's the challenge:




# References
* [State Monad For The Rest Of Us - source code][source-code]
  

# Comments
[GitHub Discussions][discussions]


[discussions]: https://github.com/arialdomartini/arialdomartini.github.io/discussions/30
[source-code]: https://github.com/arialdomartini/state-monad-for-the-rest-of-us

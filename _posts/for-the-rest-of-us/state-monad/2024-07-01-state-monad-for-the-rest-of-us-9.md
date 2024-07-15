---
layout: post
title: "State Monad For The Rest Of Us - Part 9"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- F#
- Functional Programming
include_in_index: false
---
Source code:
[github.com/arialdomartini/state-monad-for-the-rest-of-us][source-code].

Here's the starting point you got to in * [Chapter 8](state-monad-for-the-rest-of-us-8):

## State manipulation
We would like the `Leaf` branch to look more or less like this:

```fsharp
    let leaf = Leaf (v, getCount)
    writeCount (getCount + 1)
    leaf
```

We can try to implement `getCount` and `writeCount`. Remember that this is the starting point:

```fsharp
let rec index =
    function
    | Leaf v ->
        WithCount (fun count -> (Leaf (v, count), count + 1))
    | Node (l, r) ->
        pure' build <*> index l <*> index r
```

It is worth to repeat that the `Leaf` branch does not directly return
a `Leaf` instance: instead, it returns a `Int -> (a, Int)` function
wrapped in a `WithCount`. Most likely, also `getCount` and
`writeCount` need to deal with a `WithCount`.

### Reading the state








# References
* [State Monad For The Rest Of Us - source code][source-code]
  
[source-code]: https://github.com/arialdomartini/state-monad-for-the-rest-of-us


# Comments
[GitHub Discussions][discussions]


[discussions]: https://github.com/arialdomartini/arialdomartini.github.io/discussions/30


{% include fp-newsletter.html %}

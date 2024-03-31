---
layout: post
title: "Emacs: Registers"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- emacs
- lisp
---
In our exploration of the ways to navigate back to previous buffer
positions, Registers can be seen as mark ring items with an assigned
name, which can be accessed in an arbitrary order. But they are much,
much more:

- They can be persisted so they survive reboots.
- Other than buffer positions, they can contain snippets of code.
  keyboard macros, windows layouts and numbers.
- They can resurrect killed buffers.

Playing Hansel and Gretel is just the excuse to happily slip into
another rabbit hole. Let's go!
<!--more-->
# Table of Contents
* [Mark Ring](/emacs-mark-ring)
* Registers
* Bookmarks

(Thanks to [Protesilaos][prot] for the kind review).

# References
* [Protesilaos Stavrou][prot]


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/29)

[prot]: https://protesilaos.com/coach/

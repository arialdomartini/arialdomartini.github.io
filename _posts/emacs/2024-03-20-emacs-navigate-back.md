---
layout: post
title: "Emacs: Playing Hansel and Gretel with Marks, Registers and Bookmarks"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- emacs
- lisp
---
A cool trick not everybody knows with Visual Studio and JetBrains'
IDEs is that it is possible to navigate back to previous positions
with `Ctrl -`.

A similar trick also works with Bash:

```bash
cd -
```

and with Git:

```bash
git checkout -
```

Neat, right?  
Of course, there are *way* more powerful tools built around this idea
(such as [z][z], [autojump][autojump] and [zoxide][zoxide]). And
&mdash; did you doubt? &mdash; similar powerful features in Emacs
too. Let's explore them.

<!--more-->
# Leaving a trail of breadcrumbs
So you want to leave a track while you move around buffers and you
want to retrace your steps, right?  
As far as I know, Emacs offers 3 tools:

| Tool      | Main characteristics                                           | When it is the best fit     |
| **mark ring** | - buffer local and global<br/>- unnamed<br/>- both manual and automated<br/>- volatile<br/>- It's related to the region | - Quick, impromptu navigation |
| **registers** | - global<br/>- short-named<br/>- manual<br/>- not persistent by default | - Within a working session<br/>- Can store more than positions  |
| **bookmarks** | - named<br/>- manual<br/>- persisted                                   | - The longterm cornerstones for organizing your work environment<br/>- Can store more than positions        |

In this series, we will explorer them all!

# Table of Contents
* [Mark Ring](/emacs-mark-ring)
* [Registers](/emacs-registers)
* Bookmarks

# References
* [z][z]
* [autojump][autojump]
* [zoxide][zoxide]

[z]: https://github.com/rupa/z
[autojump]: https://github.com/wting/autojump
[zoxide]: https://github.com/ajeetdsouza/zoxide


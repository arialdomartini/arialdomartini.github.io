---
layout: post
title: "Emacs: Mark Ring"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
include_in_index: false
tags:
- emacs
- lisp
---
<!--more--> Rings &mdash; fixed sized variables acting as circular
buffers &mdash; are a beautiful idea: one day I will eventually write
something about how undoing changes is handled in Emacs with the
[undo-ring][undo]. I find outrageous that other editors have not
followed the same idea.

The mark ring it self is an amazingly simple idea: it is just a
variable storing positions. Around it, there are of course functions
for pushing and pulling data, functions for browsing the stored
positions, packages for making all of this even more convenient and the like. But the foundation is just that: a variable.  
While I'm writing this post, the content of my `mark-ring` is:

```emacs-lisp
M-x describe-variable <RET> mark-ring <RET>

(#<marker at 2080 in 2024-03-17-emacs-navigate-back.md>
 #<marker at 2080 in 2024-03-17-emacs-navigate-back.md>
 #<marker at 2074 in 2024-03-17-emacs-navigate-back.md>
 #<marker at 2074 in 2024-03-17-emacs-navigate-back.md>
 #<marker at 2074 in 2024-03-17-emacs-navigate-back.md>
 #<marker at 1696 in 2024-03-17-emacs-navigate-back.md>
 #<marker at 1195 in 2024-03-17-emacs-navigate-back.md>
 #<marker at 1516 in 2024-03-17-emacs-navigate-back.md>
 #<marker at 1447 in 2024-03-17-emacs-navigate-back.md>
 #<marker at 1392 in 2024-03-17-emacs-navigate-back.md>
 #<marker at 1379 in 2024-03-17-emacs-navigate-back.md>
 #<marker at 1056 in 2024-03-17-emacs-navigate-back.md>
 #<marker at 1042 in 2024-03-17-emacs-navigate-back.md>
 #<marker at 1206 in 2024-03-17-emacs-navigate-back.md>
 #<marker at 1237 in 2024-03-17-emacs-navigate-back.md>
 #<marker at 1224 in 2024-03-17-emacs-navigate-back.md>
 #<marker at 1056 in 2024-03-17-emacs-navigate-back.md>
 #<marker at 904 in 2024-03-17-emacs-navigate-back.md>)
```

As you see, it is just a list of objects referencing a buffer and
a position.  
The mark ring is used as a storage for the historical values of
marks. Marks, on their side, are positions that you intentionally
asked Emacs to keep a memory of &mdash; or that Emacs, before some
operations, decided to mark on its initiative.  
There is a notion of the *current* mark, which you can inspect evaluating
`(mark)`. Whenever the mark changes, its old value is pushed in the
mark ring.  
If your head already spins, don't despair. It's easier than it
seems. Think of Git.

## Like Git
I find the relation between the cursor, the mark and the mark ring
similar to what happens with Git with the worktree, the index and the
repository. This is wonderfully represented in this interactive
[Git Cheatsheet][git-cheatsheet]. For Emacs we have something similar
(and way simpler):

![Change me](static/img/emacs/mark-ring/mark-ring.svg "No title")


## Just Going Back
So you have a history of marked position. The basic idea is: 

* You store a position for future use.
* You move around.
* When you want to get back on your tracks, you pull the position out
  of the history.
  
Here is the basic usage:

* Whenever you want to leave a breadcrumb, use `C-SPC
  (set-mark-command)`. Hit it twice: the first time it will start
  selecting text (in Emacs lingo: it will *activate the mark*). Here
  we don't want to select anything: we just want to write a position
  down in the history.
  `C-SPC (set-mark-command)` updates the current mark and *pushes* the
  previous a value in the mark ring, in fact creating a history of
  positions.
* If `C-SPC` saves a position, it should not come as a surprise that
  prefixing it with `C-u` reverses the behavior. After all, that's a
  common pattern in Emacs.
* Indeed: whenever you want to move back to the previous position,
  hit `C-u C-SPC`. Your cursor will be moved where the mark is, and
  the previous position will be popped out from the mark ring. In
  other words, with `C-u C-SPC` you will be consuming the position history.

Follow the diagram. It's really easier done than said.

`C-u C-SPC` acts like VisualStudio's and IntelliJ's `Ctrl -` with a
big difference: with VS and Idea, it is not that clear *when* new
values are pushed in the position history; in Emacs you are in full
control.  
But this is, anyway, just the tip of the icebearg.

## Oh, My Consult!
The position history is just a variable. You could expect that someone
would eventually write packages to manipulate that variable in fancy
ways. Amongst others, [Daniel Mendler][minad] did that, with the
amazing [consult.el][consult] library. To me, the most convenient way
to operate with the mark ring is, hands-down, consult.el. Install it with:

```emacs-lisp
(use-package consult
  :ensure t)
```

It will provide you with the command `consult-mark` which you can use
to *browse* all the positions in the mark rink &mdash; not only the
last one in a LIFO fashion &mdash; and to have a *real-time* preview
of the content you would jump to.

Do this experiment:

- Open a large file.
- Move around, and from time to time hit `C-SPC C-SPC` wherever
  you want to leave a breadcrumb.
- When you have enough of this, run `M-x consult-mark`.
- Try to move up and down, back and forth the history.
- Search for a position in the mark ring by content.

Never. Again. Without.


## Multi Cursors
Once you have some positions stored in the mark ring, there are other
interesting things you can do.

* Navigate around in a buffer and push some marks here and there
 (`C-SPC C-SPC`).
* Run `M-x mc/mark pop`. This will pop the last mark from the
  mark ring and add a second cursor there.
* Run `M-x mc/mark pop` again. You will get a 3rd cursor, in the
  second to last position.
* Stop when you wish.
* Happy editing with multi-cursors!

End your multi-cursors session with `C-g`.


## Setting the mark elsewhere
So, `set-mark-command (C-SPC)` is the way to manually set a mark
exactly where the cursor is. Is that all?  
Of course not. There are many other functions that help setting the
mark to other specific positions, for example after a word, at the end
of the page or at the end of a function.

In most of the applications, though, the mark is set for other reasons
than just keeping a history of positions. In fact, in Emacs the mark
is the foundation for defining a region, that is, for selecting
text. In a sense, this ability to move back to previous positions is a
byproduct of handling the region.

## The Region
In the majority of editors, the *region*, or the *selection*, is the
part of text that is temporarily highlighted, usually by the means of
the mouse or using the arrows plus the Shift key.  
In Emacs the region is way more powerful. As it often happens, this
greater powers come from simpler foundations. Indeed, in Emacs the
region is a trivial notion: it is just the part of buffer between the
mark and the cursor.

Why do I say this is more powerful? Think how to do the following
tasks with an editor other than Emacs or Vim. Start selecting some
text, then:

* Use any of the editor's search capabilities to find where to end the
  selection.
* Consult another file while you are selecting.
* Move the focus to another application, get back to your editor and
  expect to find the selection where you left it.
* Complete the selection. Then, change idea where the selection
  should start.

Those are all trivial tasks with Emacs. Possibly, just no possible
with other editrs. If the other editors had to copy *one*
functionality from Emacs, it should be this, hands down.

Getting back to those commands that somehow save a position in the
mark: given what we said about the region, it should come with no
surprises that the majority of them also *activate* the region. For
example, `mark-defun (C-M-h)` sets the mark at the end of a function
*and* moves the point at its beginning. The final effect is, the whole
function will be selected.  
You can find a more complete list in the chapter [Marking
Objects][marking-objects].

Knowing that *selecting* is about setting the the mark &mdash; so also
about pushing the current value in the history &mdash; gives you the
opportunity to perform some smart moves. For example:

* Select the current function / paragraph with `mark-defun (C-M-h)`.
* Notice that the cursor ended up being at the beginning of the
  function. The selection ends where the function ends.
* Use `C-u C-SPC` to move to the function end.
* Press `C-x C-x` multiple times: you will keep jumping between the
  beginning and the end of the function.
  
If you're not lacking creativity, you'll eventually come up with a thousand clever uses for this feature.

## Invisible Region
It might not be immediately clear, but when you pasted the text, it
was *already* selected, although the selection was not
highlighted. Emacs has got a very peculiar way of managing the
selected text (the *region*): no matter if highlighted and visible,
the region is always there. As we said, it is the part of text between
the current cursor position and the most recent mark.

This allows you to do tricks such as the following. Imagine you have
the point here:

```json
{
    "parser-directories": [
        "/home/arialdo/prg/tree-sitter/"
    ],
*POINT*
}
```

You paste some code:


```json
{
    "parser-directories": [
        "/home/arialdo/prg/tree-sitter/"
    ],
"theme": {
    "attribute": {
      "color": 124,
      "italic": true
    },
    "constant.builtin": {
      "bold": true,
      "color": 94
    }
}
}
```

and you notice that the result is horribly indented. Since you know
that the code you just pasted *is already selected* &mdash; the mark
and the cursor do surround it &mdash; you can run commands
that expect a region. Type `M-x indent-region RET` or just simply
`C-M-\` and format it.

We saw this trick in [Emacs: Let's Surround!](/emacs-surround) when
talking about the region. You might be interested in finding a bit
more details there.

## Mark what?
Here are some questions you might legimately ask yourself:

* Can I mark a position in a dired buffer? 
* Can I mark a commit in the Magit's Git log buffer?
* What about marking a position in vterm while in vterm-copy-mode?
* Emacs can open PDF and PNG files: can I mark them?

Guess what? It's a full house of "yes! yes! yes!". Of course you can!
Everything in Emacs is a text, everything lives a buffer. Why wouldn't
you be able to?

This is, to me, the beauty of Emacs. It's not the amount of plugins
([VS Code has 57202 extensions][vscode], so what?). It's not its
alleged Operating System nature. To me, the reason why Emacs stands
out as an engineering product is that it is built on top of *few* core
building blocks, just a handful of *simple* notions that marvelously
build upon each other consistently, seemlessly, elegantly, creating a
cohesive structure.


### Note to myself: good things in life are temporary

Here's a last question.

* What if I mark a position and then I kill the buffer? Will Emacs resurrect it when jumping back?

And here, for once, the answer is "no". Marks are really meant to be
volatile.  
But don't despair: there are other means to do this.


### Other Cases
There are other commands that automatically push the current cursor
position in the mark ring before operating. A notable case is
`switch-to-buffer (C-x b)`, the command you use to move to a different
buffer. You will be happy to know that every times you jump to another
file and you mark a new position, Emacs scrupulously saves that
breadcrumb also in the global mark ring. This will allow you to jump
back with `pop-global-mark (C-x C-SPC)`. And of course, consult.el has
a command for browsing this: `consult-global-mark`.

This brings me to the last topic: other than the local mark rings (one
per buffer), there is a global mark ring. What is it for?

## Buffer Local and Global Marks
Each buffer keeps its own mark ring. This means that `mark-ring` is
defined as a *buffer local* variable. You can verify that invoking
`M-x describe-variable RET mark-ring RET` and then navigating to the
source code:

```emacs-lisp
(defvar-local mark-ring nil
  "The list of former marks of the current buffer, most recent first.")
```

By default, `mark-ring` keeps the last `16` positions. But you can
customize this setting `mark-ring-max`.

As we just mentioned, there is also a global mark ring version, not
surprisingly called `global-mark-ring`. Its maximum size is defined,
guess what?, with `global-mark-ring-max`.

A couple of legit questions are:

* Is it possible to feed this global mark ring? 
* When a mark is pushed locally, is this also reflected in the global
mark ring?

The answer is [jein][jein]! As usual, the manual is exhaustive:

>  Each time you set a mark, this is recorded in the global mark ring
> in addition to the current buffer’s own mark ring, 
> if you have switched buffers since the previous mark setting.
> 
> Hence, the global mark ring records a sequence of buffers that you
> have been in, and, for each buffer, a place where you set the mark

To make it simple: as a modern Hansel & Gretel, Emacs keeps
dropping off a breadcrumb every time you ask it so; and every time you
happen to be on a different path, if you mark the new trail, it will
lay done a white stone, a special mark you can follow to track down
your journey from a bird's-eye perspective:

| global marks | ⏺       |   |   | ⏺       |   |   |   | ⏺       |   |   |
| local marks  | ○       | ○ | ○ | ○       | ○ | ○ | ○ | ○       | ○ | ○ |
| buffer       | **`A`** |   |   | **`B`** |   |   |   | **`C`** |   |   |

## A bit of Lisp
If you are curious about how things *work* more than about how to
*use* them (and it's likely, if you spend time tinkering with Emacs)
you might wonder: what does happen to the `mark-ring` variable when a
value is pulled out of it? Is the `mark-ring` like a stack, that is
progressively consumed as values are popped-out of it? 

You could display the point, the mark and the mark ring value with something like:

```emacs-lisp
(defun display-mark ()
  (interactive)
  (message "%s -> %s -> %s" (point) (mark) (mark-ring-positions)))

(defun mark-ring-positions ()
  (mapcar
   (lambda (item)
     (marker-position item))
   mark-ring))
   
(keymap-set global-map "C-c c" 'display-mark)
```

Just hit `C-c c`. You will find that the mark ring is really a
circular structure. When you pop a value out of it, you are not really
consuming it: the ring will rotate so, as long as you don't exceed
`mark-ring-max` values, you will never loose information.


## What's next?
Fantasizing how to improve the mark and its rings one could dream of
some extra-functionalities:

* Items in the history could be given a name so that they can be easily
  called back in any order.
* Visiting back something that was killed should resurrect it.
* Why to store positions only? Why not to store snippets of text too?
* Now I come to think of it: *everything* is a text in Emacs? Why not
  to store keyboard macros in a history?

Wouldn't it be cool to have all of these features?  
Enter Registers!


# References
* [consult.el][consult]
* [The Mark and the Region][emacs-manual]
* [Lesser Known Bash shortcuts](/lesser-known-bash-shortcuts)
* [Marking Objects - Emacs Manual][marking-objects]
* [Emacs: Let's Surround!](/emacs-surround)
* [Git Cheatsheet][git-cheatsheet]
* [Undo][undo]
* [Daniel Mendler - GitHub][minad]
* [German with Nicole - Jein][jein]
* [Visual Studio Marketplace - Extensions for Visual Studio Code][vscode]

[z]: https://github.com/rupa/z
[autojump]: https://github.com/wting/autojump
[zoxide]: https://github.com/ajeetdsouza/zoxide
[consult]: https://github.com/minad/consult
[emacs-manual]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Mark.html
[marking-objects]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Marking-Objects.html
[git-cheatsheet]: https://ndpsoftware.com/git-cheatsheet.html?utm_source=pocket_saves#loc=index
[undo]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Undo.html
[minad]: https://github.com/minad
[jein]: https://www.germanwithnicole.com/blog/36225-jein-ja-nein
[vscode]: https://marketplace.visualstudio.com/search?target=VSCode&category=All%20categories&sortBy=Installs

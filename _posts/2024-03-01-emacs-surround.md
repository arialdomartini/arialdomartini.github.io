---
layout: post
title: "Emacs: Let's surround!"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- Emacs
- Lisp
---
1. Select a region.
2. Let Emacs surround it with custom delimiters.
3. Profit.
<!--more-->


## Note
This post is based on the content of the lessons I took from
[Protesilaos Stavrou][prot] and on the further exploration they
inspired to me.

My goal is to extend Emacs so that the following will be possible:

- To select a text region in a buffer.
- To invoke a function &mdash; possibly through a keybinding.
- To see Emacs displaying a well-formatted list of possible
  surrounding delimiters (such as `(` &mdash; `)` or  ```` ```haskell ```` &mdash;
  ```` ``` ````).
- To select one.
- To have Emacs surround the region with the selected closing
  delimiters.

A further goal would be to implement a [DWIM][dwim] behavior and to let
Emacs guess the extent of the text to surround. This will be the topic
for a future post.

I'm sure that any programmer fluent in Emacs Lisp would be able to
develop this functionality in few minutes. I'm no expert, so I had to
learn. This post documents what I discovered in the several detours I
took during the journey.  
As often happens, the goal has been more an excuse to explore topics
than an objective in itself.

# The idea
The idea is to write an interactive function that:

- Detects the beginning and end of the region.
- Asks the user which pair of surrounding elements to use.
- Adds one element of that pair right before at the beginning of the
  region, and the other right after the end.
- Moves the point back where it was.

# Prepending a surround prefix
Let's build this incrementally, starting from a trivial function that
inserts a hard-coded opening delimiter right before the region starts:

```emacs-lisp
(defun prepend-string-to-region ()
  "Insert a hard-coded string right before the beginning of the current region"
  (interactive)
  (goto-char (region-beginning))
  (insert "<<<"))
```

This prepends a string to the region, keeping the region content
unmodified. An alternative approach is to replace the whole region with
a new text:

```emacs-lisp
(defun prepend-string-to-region ()
  "Insert a hard-coded string right before the beginning of the
current region by replacing the whole region"
  (interactive)
  (let* ((beginning (region-beginning))
         (end (region-end))
         (region-content (buffer-substring-no-properties beginning end))
         (prefixed-content (concat "<<<" region-content)))
    (delete-region beginning end)
    (goto-char (region-beginning))
    (insert prefixed-content)))
```

Although the second function seems a bit aggressive, let's take both
solutions in mind: eventually we will need to figure out which one
provides the most solid approach.

Test them selecting a region and invoking `M-x
prepend-string-to-region`.

## Improving the code
There are 2 problems with both functions:

1. They don't retain the cursor position.
2. They add the opening delimiter even if there is no region selected.

Fixing the first problem is straightforward, using `(save-excursion)`.
Its documentation (`M-x describe-function RET save-excursion RET`) states:

> Save point, and current buffer; execute BODY; restore those things.

Exactly what we need. So, we can change both the functions as follows:

```emacs-lisp
(defun prepend-string-to-region ()
  "Insert a hard-coded string right before the beginning of the current region"
  (interactive)
  (save-excursion
    (goto-char (region-beginning))
    (insert "<<<")))
```

and

```emacs-lisp
(defun prepend-string-to-region ()
  "Insert a hard-coded string right before the beginning of the
current region by replacing the whole region"
  (interactive)
  (let* ((beginning (region-beginning))
         (end (region-end))
         (region-content (buffer-substring-no-properties beginning end))
         (prefixed-content (prepend-prefix "prefix" region-content)))
    (save-excursion
      (delete-region beginning end)
      (goto-char (region-beginning))
      (insert prefixed-content))))
```

As for the second problem, instead, it makes sense to have little explaination.

## Region, mark and point
### Point
When you move your cursor in a buffer, you are modifying the value of
the so called *point*. The point is just simply the location where
editing operations take place, so the position of the cursor.

You can always retrieve the position of the point with the function:

```emacs-lisp
(point)
```

and change its value with:

```emacs-lisp
(goto-char POSITION)
```

(Yes, `(goto-char (point))` is an obnoxious way to consume energy for
getting absolutely no result).

### Mark
In Emacs there exist several *ring* variables. They are circular
buffers for storing a sequence of elements, and they are used for
various purposes. For example:

* `kill-ring` supports the idea of a multi-clipboard.
* `kmacro-ring` stores the history of the keyboard macros.
* `eshell-history-ring` remembers the executed shell commands.

One could think that Emacs also dedicates a ring to store a history of
the point positions. After all, after a movement, it is possible to
jump to the previous position with `C-u C-<SPC>`. So, one could expect
that a `point-ring` exists.

That's not the case. There is, instead the `mark-ring`. This bears the
question: what is the *mark*?

The mark is like a second, hidden, point which *marks* a position
within the buffer, keeping track of the past positions in the
`mark-ring`. The mark can be explicitly set by the user with 
`M-x set-mark-command` (`C-SPC`), or automatically by Emacs itself after
some operations &mdash; which enables some interesting use cases.

`C-SPC` takes the current point position and copies it to the mark.
The previous mark value is pushed to the `mark-ring`, for future uses.

This is useful for several very idiomatic Emacs functionalities. To
beging with, if `C-SPC` pushes the mark in the `mark-ring` and then
copies `point` onto `mark`, one may wonder if there is a way to do the
opposite. And in fact, there is! Just passing an argument to
`set-mark-command` &mdash; that is, interactively pressing `C-u
C-SPC` &mdash; inverts the operation. As the documentation states:

> With prefix argument (e.g., C-u C-SPC), jump to the mark, and set
> the mark from position popped off the local mark ring (this does not
> affect the global mark ring). Use C-x C-@ to jump to a mark popped
> off the global mark ring (see ‘pop-global-mark’).

In other words: 

- You move around a buffer.
- You mark some positions with `C-SPC C-SPC` (I will shortly clarify
  why you need to hit this twice).
- When you want to jump back to those positions, hit `C-u C-SPC`.
- Otherwise, you can also use the beautiful `consult-mark` which lets
you browse the `mark-ring` interactively and even perform real-time
searches.

Why are we talking about the `mark`? Because it has a lot to do with
the region, which is central to our use case. Mark and region form a
broad topic in Emacs. You can read about them in [The Mark and the
Region][mark-and-region] on [https://emacsdocs.org](https://emacsdocs.org)  
For our case, we just need some details. So, let's talk about the region.

### Region
The region is just the fraction of the buffer between the current
`point` and the current `mark`.

This is a subtle notion. With other editors, we are used to think to
the "region" or the "selection" as that part of text which is
*highlighted* after the user selected it either with the mouse (urgh!)
or by pressing Shift and moving the cursor.

In Emacs things are way more flexible. To begin with, a region does
exist even if no text is highlighted. Now that you know that the
region is the text between the point and the mark, this could sound
not too weird. To verify this, do the following experiment:

- Set the mark somewhere, with `C-SPC C-SPC`. The position of the mark
  will be invisible. Remember where it is.
- Move the `point` somewhere else.
- Imagine the (transparent) region determined by those 2 positions.
- Hit `C-w` (`kill-region`) to delete that region.

Surprise surprise (or not): you killed a selection, even if no one was
visible.  
This bears the question: how does it come that some times the region
is visible, some times it is not?

### Activating the mark / the region
It is all about the notion of *active* or *not active* mark (or
*active* or *not active* region: I think the 2 are equivalent).

Try yourself: define a region hitting `C-SPC C-SPC` and then moving
the point somewhere else. Then write:

```emacs-lisp
(activate-mark)
```

and execute it by placing the point right after the closing `)` and
running `M-x eval-last-sexp` (`C-x C-e`). You will see the invisible
region appear.

When the region is active, the text is highlighted by
`transient-mark-mode`. If this mode is disabled, then an active region
will keep being invisible. This might be very puzzling, especially if
one does not know how the mechanism under the hood works with points
and marks. There are good reasons why `transient-mark-mode` is enabled
by default.

The reason why we can usually ignore these nuances is because, very
conveniently, when `set-mark` (`C-SPC`) is invoked, it *also*
activates the mark. Here it is why `C-SPC` is the conventional way to
start selecting text. And this is also why, if your goal is only to
set a value for mark without activating it, you press `C-SPC` twice:

- The first time it sets the mark, activating it.
- The second time, it disables it. 

Try yourself, keeping an eye on the messages in the echo area.

Unusual, when compared with other editors. But also linear and consistent, isn't it?

### A neat trick
Before I mentioned that the mark can be either set intentionally by
the user, or set automatically by Emacs under certain circumstances.

There are ways to use this fact to one own's advantage. The secret is
to know which commands do set `mark`, and to remember that a region
exists even when it is not highlighted.

One of the commands that tamper with `mark` is `yank`, the ordinary
`C-y` used to paste from the clipboard. If you open its documentation
you can read the following:

> Put point at the end, and set mark at the beginning without
> activating it. With just C-u as argument, put point at beginning,
> and mark at end. With argument N, reinsert the Nth most recent kill.

Interesting! Then, right after pasting some text in a buffer, an
invisible, non-active region surrounds it. To help visualize it,
imagine you have the point somewhere in a buffer:

```
Here is a poem I found online:

*POINT*

I hope you will enjoy it.
```

Do yank some text (here I am using [The song of seas][poem] by Protesilaos):

```
Here is a poem I found online:

*MARK*

    On windy days
    pine trees sing
    the song of seas

    No matter the place
    the heart will travel
    to distant shores

    Enter the nascent grove
    where secrets are told
    and follow the rhythm

    With each wave
    new life forms emerge
    as others wash away

    Find trees and seas
    witness transfiguration anew
    and dance to their beat

*POINT*

I hope you will enjoy it.
```

Notice my comment about the position of `*MARK*` and `*POINT*`:
pasting the text leaves the mark where once you had the point; the
point is instead at the end of the pasted text. You can convince
yourself that the pasted text is in fact surrounded by a region either:

- by switching `point` and `mark`, with `C-x C-x`
  (`(exchange-point-and-mark`). This will also conveniently activate
  the region, making it visible.
- by performing any operation on the region, such as `M-x center-region`.
- jumping to the beginning of the pasted text, with `C-u C-SPC`.
- run `M-x indent-rigidly` (`C-x C-i`) and then use your arrows to
  move the pasted section left and right.

## Fixing the second problem
Enough with wandering around. Let's go back to our code. By now, it
should be clear that our second unsolved problem, rather than:

2. it adds the opening delimiter even if there is no region selected

could be better formalized as:

2. [...] if mark/region is not activated.

This suggests us to make use of `(region-active-p)`, a predicate
(notice the final `-p`) to check if the region is active, and to run
the function only `when` it is active:

```emacs-lisp
(defun prepend-string-to-region ()
  "Insert a hard-coded string right before the beginning of the
active region"
  (interactive)
  (when (region-active-p)
    (save-excursion
      (goto-char (region-beginning))
      (insert "<<<"))))
```

and 

```emacs-lisp
(defun prepend-string-to-region ()
  "Insert a hard-coded string right before the beginning of the
active region by replacing the whole region"
  (interactive)
  (when (region-active-p)
    (let* ((beginning (region-beginning))
           (end (region-end))
           (region-content (buffer-substring-no-properties beginning end))
           (prefixed-content (concat "<<<" region-content)))
      (save-excursion
        (delete-region beginning end)
        (goto-char (region-beginning))
        (insert prefixed-content)))))
```

# Appending a surrounding suffix
Cool. Let's complete the function adding the closing delimiter. Writing
the implementation is really trivial:

```emacs-lisp
(defun append-string-to-region ()
  "Insert a hard-coded string right after the end of the
active region"
  (interactive)
  (when (region-active-p)
    (save-excursion
      (goto-char (region-end))
      (insert ">>>"))))
```

So far so good. Problems arise when we try to merge the two functions
to create a `surround-region-with-strings`:

```emacs-lisp
(defun surround-region-with-hard-coded-strings ()
  "Surround the active region with hard-coded strings"
  (interactive)
  (when (region-active-p)
    (save-excursion
      (goto-char (region-beginning))
      (insert "<<<")
      (goto-char (region-end))
      (insert ">>>"))))
```

Can you spot the bug? If you don't see it from the code, try running
the function. Surround the following text from left to right &mdash;
that is, first setting the mark to the beginning of the line, then moving
the point to the end:

```
The quick brown fox jumps over the lazy dog
```

Invoking `M-x surround-region-with-hard-coded-strings` results in:

```
<<<>>>The quick brown fox jumps over the lazy dog
```

instead of the desired:

```
<<<The quick brown fox jumps over the lazy dog>>>
```

Curioulsy, selecting the region the other way around &mdash; that is,
setting the mark after "dog" and then moving the point before "The"
&mdash; brings the desired result.  
Why does this happen?

## It is all about the mark
If you are confused, so was I initially. I had a doubt: is it possible
that `insert` alters the current region, therefore making the
subsequent use of `region-beginning` and `region-end` unreliable? Even
before reading `insert`'s documentation, I wanted to make a little
experiment, executing the following:

```emacs-lisp
(progn
  (save-excursion
    (goto-char 1)
    (set-mark (point))
    (goto-char 16)
    (message (format "Region is from %s to %s" (mark) (point)))))
```

This emits the message:

```
Region is from 1 to 16
```

So far so good. Then I checked the same adding an `insert`:

```emacs-lisp
(progn
  (save-excursion
    (goto-char 1)
    (set-mark (point))
    (goto-char 16)
    (insert "Hello, world")
    (message (format "Region is from %s to %s" (mark) (point)))))
```

This emits:

```
Region is from 1 to 28
```

Good: `28` instead of `16`. Of course! This is so obvious, once we
think how we have defined the region:

> the fraction of the buffer between the current `point` and the
> current `mark`

As a matter of facts, `insert` types characters in the buffer, hence
it changes the position of the `point` &mdash; and therefore, the
extension of the region.

Indeed, the documentation was clear:

> Point and after-insertion markers move forward to end up after the
> inserted text. Any other markers at the point of insertion remain
> before the text.

So, the mysterious behavior is easily explained. Selecting the following:

```
The quick brown fox jumps over the lazy dog
```

from left to right, it means that we end up with `mark` and `point`
like the following.

```
*MARK*The quick brown fox jumps over the lazy dog*POINT*
```

When we execute our `surround-region-with-hard-coded-strings`, we first
insert the prefix:

```emacs-lisp
  (goto-char (region-beginning))
  (insert "<<<")
```

and this, fatally, moves the point right after `mark`, loosing track of the original region:


```
*MARK*<<<*POINT*The quick brown fox jumps over the lazy dog
```

What about saving the original values with `let`? This seems a good
idea, but it's not hard to see why neither this is a solution:

```emacs-lisp
(defun surround-region-with-hard-coded-strings ()
  "Surround the active region with hard-coded strings"
  (interactive)
  (when (region-active-p)
    (save-excursion
      (let ((beginning (region-beginning))
            (end (region-end)))
        ;; insert the opening delimiter
        (goto-char beginning)
        (insert "<<<")
        ;; then the closing one
        (goto-char end)
        (insert ">>>")))))
```

Do you see the new bug? Now the behaviour of surrounding is
independent from the direction the region is defined (and this is
good), but unfortunately the closing delimiter is consistently inserted
in the wrong place. In fact,

```
The quick brown fox jumps over the lazy dog
```

is surrounded as:

```
<<<The quick brown fox jumps over the lazy >>>dog
```

The problem is the last:


```emacs-lisp
        (goto-char end)
        (insert ">>>")))))
```

Even if we saved the original value of `(region-end)` in the variable
`end`, this value does not reflect anymore the right position in the
buffer: in fact, new characters (`<<<`) have been inserted, and the
buffer content was right shifted by 3 characters.  
~~I don't want to fix this doing arithmetic. Although it would be a
simple calculation, most likely the resulting code would be opaque and
brittle~~.  
As [nv-elisp](https://www.reddit.com/user/nv-elisp/) suggests [on
Reddit][nv-eslip], that would not be so difficult:

```emacs-lisp
(defun surround-region-with-hard-coded-strings ()
  "Surround the active region with hard-coded strings"
  (interactive)
  (when (region-active-p)
    (save-excursion
      (let ((beginning (region-beginning))
            (end (region-end))
	    (opening-delimiter "<<<")
	    (closing-delimiter ">>>"))

        (goto-char beginning)
        (insert opening-delimiter)

        (goto-char (+ end (length closing-delimiter)))
	(insert closing-delimiter)))))
```

A possible alternative solution is to *first* insert the closing
surrounding delimiter, *then* the opening one:

```emacs-lisp
(defun surround-region-with-hard-coded-strings ()
  "Surround the active region with hard-coded strings"
  (interactive)
  (when (region-active-p)
    (save-excursion
      (let ((beginning (region-beginning))
            (end (region-end)))
        ;; Insert the closing delimiter first
        (goto-char end)
        (insert ">>>")
        ;; then the opening one
        (goto-char beginning)
        (insert "<<<")))))
```

This works, but honestly it is a fragile approach: it risks to
fail again if only the order of few commands is changed.  
A last option is to reconsider deleting the whole region and replacing
it with a surrounded content:

```emacs-lisp
(defun surround-region-with-hard-coded-strings ()
  "Surround the active region with hard-coded strings"
  (interactive)
  (let* ((beginning (region-beginning))
         (end (region-end))
         (region-content (buffer-substring-no-properties beginning end))
         (prefixed-content (concat "<<<" region-content ">>>")))
    (save-excursion
      (delete-region beginning end)
      (goto-char (region-beginning))
      (insert prefixed-content))))
```


# Keybinding
Before extending the function to make it more flexible and useful,
let's learn how to invoke it with a keybinding. The trivial way is to
use `global-set-key`:

```emacs-lisp
(global-set-key (kbd "C-c j") #'surround-region-with-hard-coded-strings)
```

But this is rough. Emacs is very context-aware: it behaves differently
when operating on text files, or on a Git repository via Magit, or
while manipulating files in Dired. The very idea of a global
keybinding should be taken with a grain of salt.

A better idea is to associate the keybinding with one or more specific
contexts. A way to do so is to rely on the notion of *modes*, a topic
I briefly discussed when exploring hooks, in [How to activate the
functionality X for all files of type Y?](emacs-hooks).

Each mode comes with its keymap, usually named after the mode name
itsalf, with a `-map` suffix: the keymap contains the set of
keybindings that are activated when the mode is activated, and which
are restored to the previous setup when the mode is disactivated.

There are 2 ways to add a keybinding to a mode's map:

- using `define-key`, which is more low-level:

```emacs-lisp
(define-key lisp-interaction-mode-map (kbd "C-c j") #'surround-region-with-hard-coded-strings)
```

- using `keymap-set`, which operates at a higher level:

```emacs-lisp
(keymap-set lisp-interaction-mode-map "C-c j" #'surround-region-with-hard-coded-strings)
```

If you visit the source code of `keymap-set` with 
`M-x describe-function RET keymap-set RET` you will see that under the
hook it eventually invokes the lower lever `define-key`.

# Going beyond hard-coded delimiters
Surrounding a region in an Emacs Lisp buffer with `<<<` and `>>>` is
of a rare uselessness. Let's investigate how to implement the next,
more interesting, steps:

- How to configure Emacs to display a list of delimiters, so that the
  user can select one?
- How to make this list context-dependent (for example: in XML
  mode, we'd like to be offered delimiters such as `<!-` and `->`,
  while in C++ `/*` and `*/` make more sense)
- How to save the user from the need to even select the region, and
  let Emacs guess it?
  

These are topics for the next installments.

# References

- [Protesilaos Stavrou][prot]
  - [The song of seas][poem]
- [The Mark and the Region][mark-and-region]
- [Do What I Mean][dwim]
- [How to activate the functionality X for all files of type Y?](emacs-hooks)

[prot]: https://protesilaos.com/coach/
[mark-and-region]: https://emacsdocs.org/docs/emacs/Mark
[dwim]: https://www.emacswiki.org/emacs/DoWhatIMean
[poem]: https://protesilaos.com/poems/2024-02-06-song-of-seas/
[nv-eslip]: https://www.reddit.com/r/emacs/comments/1b7yhep/comment/ktmndif/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button

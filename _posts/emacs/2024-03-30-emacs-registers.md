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
name, so that they can be conveniently accessed by that name in an arbitrary order.  
But in fact they are much, much more:

- They can be persisted so they survive reboots.
- They can resurrect killed buffers.
- Other than buffer positions, they can contain snippets of text,
keyboard macros, windows layouts and even custom values.


Playing Hansel and Gretel is just the excuse to happily slip into
yet another rabbit hole.  
Let's go!  <!--more-->
# Table of Contents
* [Mark Ring](/emacs-mark-ring)
* Registers
* [Bookmarks](/emacs-bookmarks)

As we saw in [the previous installment](/emacs-mark-ring), rings are a
beautiful and powerful idea. Being cyclic LIFO data structures they
are very suitable for linearly traversing back histories. For example:

* The history of changes in the case of `undo-ring`.
* The history of cursor positions for the `mark-ring`.


There are instances, though, where you might prefer to mark a position
in a more absolute manner, assigning it a name, akin to pinpointing
a location on a map and labelling it `Home` or `Best pub in
town`. 

In this case, there are better data structures than a ring. As
a programmer, you might prefer a key-value lookup table. 

## By hand, from the scratch
Imagine you want to give each stored position a name, using a single
character such as `a` or `x`. This could be easily done with a hash table:

```emacs-lisp
(setq buffer-positions (make-hash-table))

(puthash 
 'a 
 '((current-buffer) . (point)) 
 buffer-positions)
```

The first command defines `buffer-positions` as a hash-table
variable.  
Then we use `(puthash KEY VALUE TABLE)` to store the cons cell
`'(CURRENT-BUFFER . CURRENT-POSITION)` at the key `a`.

Applying what we learnt in [Emacs: Let's surround! - Prompt the user
for input](/emacs-surround-2), we could conceive a function:

- To ask the user for the key.
- To read the value back from `buffer-positions` with `gethash`.
- To finally jump back to the right buffer (with `switch-to-buffer`)
at the right position (with `goto-char`).

```emacs-lisp
(defun jump-back-to (key)
  (interactive "cPosition key: ")
  (let* ((value (gethash key buffer-positions))
         (buffer (car value))
         (position (cdr value)))
    (switch-to-buffer buffer)
    (goto-char position)))
```

Congrats! You just invented the notion of registers!

## Humble Association Lists
As lookup tables, [Hash Tables][hash-tables] are amazingly fast,
especially with large data sets. Indeed, they offer O(1) constant time
reads. But in the humble case of recording buffer positions maybe we
are killing a fly with a cannon and perhaps a simpler
[alist][association-lists] storing `'(KEY . (BUFFER . POSITION)` would
suffice:

```emacs-lisp
(setq buffer-positions '())

(push
 (cons 'a  (cons (current-buffer) (point)))
 buffer-positions)

;; equivalently, but maybe more mysteriously
(push
 `('a . (,(current-buffer) . ,(point)))
 buffer-positions)
```

The function to jump back would be the same as before, only using
`alist-get` instead of `gethash`:

```emacs-lisp
(defun jump-back-to (key)
  (interactive "cPosition key: ")
  (let* ((value (alist-get 'a buffer-positions))
         (buffer (car value))
         (position (cdr value)))
    (switch-to-buffer buffer)
    (goto-char position)))
```

## Is that all?
Of course no. The dumb function we have just written is naïve in so
many ways:

- There is no error management. What if the user selects a key for
which no position has been stored?
- If the buffer is killed, it would be nice if the function restores
it from the file it was visiting.
- Why to limit ourselves to store positions only?
- And finally, and more importantly: how could we imagine that such a
functionality wasn't already natively implemented in Emacs?

## Registers
Registers are implemented in Emacs the same way we just described: a
single alist variable, called `register-alist`, containing keys and
values. Plus, naturally, a large set of functions to operate on them.

Once you have stored a couple of buffer positions, one to key `a`
(ASCII `97`) and one to `x` (ASCII `120`), the value of
`register-alist` would be something like:

```emacs-lisp
((120 . #<marker at 556 in *scratch*>)
 (97 . #<marker at 3805 in 2024-03-30-emacs-registers.md>))
```

I guess you recognise this is the same idea we played with before.  
The fact that registers are stored in an ordinary variable offers the
possibility to build on top of them re-using the whole arsenal of Lisp
and Emacs tools.

This is what never ceases to amaze me of Emacs: its building blocks
are just the simplest ideas you could think of, and the fact they are
orthogonal and composable puts no limits to what you can build with
them.  
Many other editors also offer a notion of registers; it is very
likely, though, that they are implemented as a special feature, alien
to other building blocks. Vim, for example, has a notion of variables,
yet registers are something special, accessed with peculiar `getreg`
and `setreg` functions.  
On the contrary, Emacs registers are nothing special: just items in an
ordinary association list.

What makes them so useful is the set of functions built around them.

## Functions
Akin to `set-mark-command (C-SPC)` to store a position in a mark, and
its alternative `C-u C-SPC` to pull it back &mdash; as we saw in [Mark
Ring](/emacs-mark-ring) &mdash; you might expect 2 equivalent
functions for registers. And you are right:

| Command                         | Functionality                        |
|---------------------------------|--------------------------------------|
| `point-to-register (C-x r SPC)` | Store current location in a register |
| `jump-to-register (C-x r j)`    | Go to location stored in a register  |

I bet that you can implement yourself both, in their basic form. Do
this, please, only to appreaciate how beautifully designed Emacs is.

Registers are so useful that by default they have a whole area of key
bindings under `C-x r`, where `r` stands of course for Register
(actually, they share the area with bookmarks, which are kinda like
registers, and with rectangles). Keybindings in Emacs are completely
arbitrary and you can always redefine them, but I think the standard
ones for registers are fairly convenient:

| Key binding | Mnemonic                          |
|-------------|-----------------------------------|
| `C-x r SPC` | Akin to `C-SPC` for the mark ring |
| `C-x r j`   | `r`egister `j`jump                |


I'm biased, though: I think that learning Emacs by memorizing
thousands of shortcuts is a pointless torture. Instead, as a
programmer, I would rather focus on commands and use packages such as
[which-key][which-key] to help my terrible memory. I like to learn
shortcuts only when I get bored to type the same command over and over.

## Oh My Consult!
Ah, yes: of course, how to forget the amazing [consult][consult]? Just
like there is a `consult-mark` for the mark ring, run
`consult-register`:

- to list all the registers.
- to filter them in real time.
- to browse them, displaying a real-time preview.
- to hit enter and finally jump to the desired location.

What would be life without the marvelous consult?

## Getting files back to life
If `register-alist` were to store not just a reference to buffers, but
to their underlying file names too, then the function
`jump-to-register` would be able to navigate back to closed
files. That would come in very handy, wouldn't it? And it would not be
that hard to develop.

But things are often a bit trickier than expected. Think to Dired:
with dired one can move or rename a file and when this happens, all
the registers mentioning that filename would need to be updated. There
is also a `rename-file` functions, and who knows which package uses
it, and when.  
So, there should be a way to keep the file names stored in registers
up to date.

In reality, Emacs implements a different approach: registers keep only
references to buffers (with the type `marker`) while they are alive,
so they are indipendent from file names. Right before a buffer is
killed, its `marker` registers are updated and converted to a
different type, called `file-query` and containing a file name instead
of a buffer reference.

You can see this in action:

- Visit a file.
- Set any location in that file in a register: `C-x r SPC RET a`.
- Display the value of `register-alist` with `M-x describe-variable
RET register-alist RET`.

You will see something like:

```emacs-lisp
((97 . #<marker at 7197 in 2024-03-30-emacs-registers.md>))
```

Notice how the register `a` is labelled as `marker` and how it does
not mention the file name at all.  
Now:

- Kill the buffer.
- Display the value of `register-alist` again:

```emacs-lisp
((97 file-query
     "/home/arialdo/prg/markdown/arialdomartini.github.io/_posts/emacs/2024-03-30-emacs-registers.md" 7197))
```

If you ask Emacs to jump back to register `a`, it will find a register
of type `file-query` and it will then ask if you desire to visit that
file again.

How did it come to pass that killing a buffer changed a register? It
is all about hooks.

## Hooks, again
We already encountered hooks in the post [How to activate the
functionality X for all files of type Y?](/emacs-hooks)  
In short: a hook is a variable holding a list of parameterless
functions that are invoked when a specific event occurs. As you can
imagine, there is a hook called `kill-buffer-hook`. Before a buffer is
killed, Emacs runs this hook's functions: one of them converts all the
`marker` registers to `file-query` items.

### Wait a sec, something is not quite correct
If you have not created a register, yet, and you run `M-x
describe-variable RET kill-buffer-hook RET` you might be puzzled not
finding any reference to registers. Indeed, Emacs here is a bit lazy:
the creation of hooks to convert registers from `marker` to
`find-query` is deferred.

If you are one of those horrible nosy people who like to open the hood
to look what's inside, you might like to visit the source code of
`point-to-register` and find out this:

```emacs-lisp
(defun point-to-register (register &optional arg)
  ...
  ;; Turn the marker into a file-ref if the buffer is killed.
  (add-hook 'kill-buffer-hook 'register-swap-out nil t)
```

where `register-swap-out` declares:

```emacs-lisp
(defun register-swap-out ()
  "Turn markers into file-query references when a buffer is killed."
```

So, in short: the very moment you create a `marker` element in a
register, that's when Emacs subscribes to the buffer killing event, so
it gets ready to replace the reference to a buffer with its underlying
file name. A bit indirect, indeed. In one of the next posts we will
play with the different, maybe more convenient approach of directly
saving a `file-query` item.

By the way, this is fine, but a bit incomplete. If you expect Emacs to
update the file name in a register when you rename a file via Dired,
get ready to be disappointed: this will not happen; renaming files
breaks registers. Maybe in one of the future issues we will play with
the elisp code to fix this. Stay tuned.

## Beyond positions
I can hear your inner-programmer's voice suggesting:

> Hey! If `register-alist` is an ordinary variable, if it is already
> used to store `markers` and `file-queries` types, then why not to
> use it to store other kinds of elements too, such as keyboard
> macros, snippets of text and the like?"

And this is very legit ambition! There is indeed no reason not to do
that. In fact, there are other groups of functions for storing and
retrieving other kinds or elements with registers:

| Kind of element                   | Command                            | Key binding          | Functionality                                                                              |
|-----------------------------------|------------------------------------|----------------------|--------------------------------------------------------------------------------------------|
| Buffer positions                  | `point-to-register`                | `C-x r SPC`          | Store a position                                                                           |
|                                   | `jump-to-register`                 | `C-x r j`            | Jump to a position                                                                         |
|                                   |                                    |                      |                                                                                            |
| Texts                             | `copy-to-register`                 | `C-x r s`            | Copy text in a record                                                                      |
|                                   | `insert-register`                  | `C-x r i`            | Paste                                                                                      |
|                                   | `append-to-register`               |                      | Append text to a register                                                                  |
|                                   | `prepend-to-register`              |                      | Prepend text to a register                                                                 |
|                                   |                                    |                      |                                                                                            |
| Rectangles                        | `copy-rectangle-to-register`       | `C-x r r`            | Copy a reactangle area to a register                                                       |
|                                   | `insert-register`                  | `C-x r i`            | The ordinary paste                                                                         |
|                                   |                                    |                      |                                                                                            |
| Keyboard macros                   | `kmacro-to-register`               | `C-x C-k x`          | Save a macro in a register                                                                 |
|                                   | `jump-to-register`                 | `C-x r j`            | Jump is smart enough to know it's dealing with a macro                                     |
|                                   |                                    |                      |                                                                                            |
| Files (with unspecified position) | `(set-register r '(file . name))`  |                      | Save a file name in a register                                                             |
|                                   | `jump-to-register`                 | `C-x r j`            | Jump to that file                                                                          |
|                                   |                                    |                      |                                                                                            |
| Windows layouts                   | `window-configuration-to-register` | `C-x r w`            | Save a frame layout to a register                                                          |
|                                   | `jump-to-register`                 | `C-x r j`            | Oh yes, Jump to a position is smart enough<br\>to know it's dealing with<br/>a frame state |
|                                   |                                    |                      |                                                                                            |
| Numbers                           | `number-to-register`               | `C-u number C-x r n` | Store a number                                                                             |
|                                   | `increment-register`               | `C-u number C-x +`   | Increment a number                                                                         |
|                                   | `insert-register`                  | `C-x r i`            | `insert-register` is smart enough to understand<br/>it's dealing with a number             |
12

Wow, that's a mouthful of keybindings to remember, isn't it?  
But I insist: don't be intimidated by shortcuts. Think to commands
instead. They are way more intuitive. `M-x` &mdash; and consult
&mdash; are your best friends. If, and only if, if happen to use `C-u
number C-x r n` so many times that you will find it convenient to
learn the key sequence, then memorize it. Otherwise, just enjoy Emacs
and forget its shortcuts.

## Consult, again
You are a lazy and a short-memory person like me, you could appreciate
`consult-register-store`. It is a smart enough to understand what you
want to store in a register from the context. If you have an active
region, *of course* it will store a region. 

In case of multiple possibilities, it will ask you how to proceed:

![Menu of consult-register-store](static/img/emacs/register/consult-register-store.png)

In the case of storing a region, it asks you whether you want to
append or prepend it to a register:

![Menu of consult-register-store](static/img/emacs/register/consult-register-store.png)

Then, browse your registers with `consult-register` or directly load
them with `consult-register-load`.

Neat, isn't it? I love Consult: [Daniel Mendler][minad] would deserve
a monument.

## A register is forever
Here is another tempting though: if `register-alist` is an ordinary
variable, why not to take its value and save it to a file so Emacs can
restore it the next time it runs? Why not to make registers
persistent? Why not to do the same with the mark-ring? After all, we
don't want our breadcrumbs to be eaten by birds, and we rather prefer
using more durable white stones, don't we?

This is a legit desire, and indeed there are techniques to achieve
this result. It's about the [Save Hist][savehist] and
the [Session Management][session-management] functionalities. 

The short, canonical answer to saving registers is to add
`register-alist` in the Save Hist's variable
`savehist-additional-variables`, which is the collection of variables
you want to be persisted from one session to another:

```emacs-lisp
(add-to-list 'register-alist savehist-additional-variables)
```

Then, enable `savehist-mode` executing `(savehist-mode)`. Your
registers will be saved in the `history` file and restored at the
beginning of the next session.  
Unfortunately, there are some pitfalls with this approach. But this
is another rabbit hole: it deserves a separate post and I promise I
will write about this soon.

First, we need to talk about bookmarks.

(Thanks to [Protesilaos][prot] for the kind review).

# References
* [Protesilaos Stavrou][prot]
* [Rings - Emacs Manual][rings]
* [Hash Tables - Emacs Manual][hash-tables]
* [Association Lists (alists) - Emacs manual][association-lists]
* [which-key][which-key]
* [consult][consult]
* [Save Hist - Emacs Manual][savehist]
* [Session Management - Emacs Manual][session-management]

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/29)

[prot]: https://protesilaos.com/coach/
[rings]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Rings.html
[hash-tables]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Hash-Tables.html
[association-lists]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Association-Lists.html
[which-key]: https://github.com/justbur/emacs-which-key
[consult]: https://github.com/minad/consult
[savehist]: https://www.emacswiki.org/emacs/SaveHist
[session-management]: https://www.emacswiki.org/emacs/SessionManagement

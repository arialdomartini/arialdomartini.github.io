---
layout: post
title: "Emacs: Registers"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- emacs
- lisp
---
# Table of Contents
* [Mark Ring](/emacs-mark-ring)
* [Registers](/emacs-registers)
* Bookmarks

Bookmarks are like Registers, with a few special traits:

- They are long-lived (they survive reboots).
- They can re-open closed buffers.
- They have arbitrarily long names.
- They are context aware.

Consider them as specialized, super convenient Registers.  
Just like Registers are stored in the variable `register-alist`,
Bookmarks are kept in the equivalent `bookmark-alist`. Nothing new,
here: again, it's all built on the same, basic building blocks.

## One command to rule them all

If you read the [Emacs Manual on Bookmarks][bookmarks], you will see a
bunch of functions to manipulate them. They are always useful to know,
but I would suggest you to start with a way simpler approach: just go
with [Consult][contult].  
With Consult, there is a single command to keep in mind: `C-x r b`
`(consult-bookmark)`.

* When you want to bookmark a position, run `(consult-bookmark)` and
  give the bookmark a name.
* When you want to jump back to it, run `(consult-bookmark)` again,
  and select the bookmark from the list.
    
No need to remember multiple commands.

![A list of bookmarks displayed by consult-bookmark](static/img/emacs/bookmarks/consult-bookmark.png)

Naturally, in perfect consult style, the bookmark list can be filtered
as you type, and it provides you with a real-time preview of the file
you are going to jump back to. I often find it handy to run
`(consult-bookmark)` only to preview a file: then, I hit `C-g` to jump
back to what I was working.

## Saved, by default
When you quit Emacs (do you, really?), Bookmarks are automatically
serialized in the file `bookmarks`, in the directory specified by
`bookmark-default-file` &mdash; by default, where your `init.el` file is.

Try to open that file: it just contains the `bookmark-alist` variable,
serialized and prettified. No magic.

Should you ever desire to save the Bookmarks before quitting Emacs,
use `M-x bookmark-save RET`. Of course, `bookmark-save` comes together
with its alter-ego `bookmark-load`.

## Bookmarks are pluggable
Bookmarks are just a variable. This, and the existence of
`bookmark-save` and `bookmark-load` could inspire you to have multiple
sets of bookmarks in different files, and to load them on demand,
maybe when you switch from one project to another, just like
[direnv][direnv] does with your `.profile`.

Indeed, this is what the package [Bookmark+][bookmark-plus] does.  
But if you are curious, you could build this by yourself, on top of
the built-in building blocks, with a bunch of Lisp lines. Here's an idea:

```emacs-lisp
(defun my/switch-project-bookmarks ()
  (setq bookmark-default-file 
        (concat (projectile-project-root) ".bookmarks"))
  (bookmark-load bookmark-default-file t))

(add-hook 'projectile-after-switch-project-hook #'my/switch-project-bookmarks)
```

This lets you have a `.bookmarks` file in each project, and load it
whenever you enter it. It's a draft, by no means ready for production,
but it could give you some inspiration.


## Like Dired, but for Bookmarks
There are indeed a few, interesting built-in functions built ardound
the Bookmarks variable. One is `edit-bookmark`.

Run it, and you will enter `bmenu`, a Dired-like environment: just
like with Dired you can create, filter, select, delete, modify and
manipulate files, with `edit-bookmark` you can do the same, with
Bookmarks. Here are some keybindings you can use:

| Keybinding | What is does                                                 |
|------------|--------------------------------------------------------------|
| `d`        | Mark a Bookmark for deletion. Execute the deletion with `x`. |
| `r`        | Rename a Bookmark.    d                                      |
| `e`        | Write an annotation, associated to a Bookmark                |
| `a`        | Show the annotation annotations                                         |
| `q`        | Quit `bmenu`.                                                        |

Now, only the Almighty Flying Spaghetti Monster knows how much I
dislike having to keep all the keybindings in my mind. It's just
beyond my possibilities.  
Having the memory of a goldfish, what I often do is to run `C-h b`
`(describe-bindings)`: it displays the set of all the active
keybindings for the current buffer, each with its documentation, in a
beautiful interactive and foldable list, from which I can even jump to the documentation.  
After all, Emacs is sold as a self-documenting editor for a reason.

## Resurrecting buffers
What if you jump to a bookmark and you have already killed the buffer
it targets? In the [previous post][emacs-registers] we show how
Registers deal with this with some less than crystalline machinery. On
this regards, Bookmarks are way more linear: they always store a file
path, not a buffer reference. Notice the field `filename` in the
`writing` Bookmark below:

```emacs-lisp
(("writing"
 (filename . "/home/arialdo/prg/personal/arialdomartini.github.io/_posts/emacs/2024-03-30-emacs-bookmarks.md")
 (front-context-string . "\n\n# Less common ")
 (rear-context-string . "ookmark and the ")
 (position . 4605)
 (last-modified 26400 56170 174644 0))
 ...
```

So, it's always trivial for a Bookmark to re-open a buffer by just
visiting the file.

## Moved files
What if the file was moved?  
Ideally, Emacs could detect when the file is being moved or renamed,
on the assumption this is done using Emacs itself: if so, Emacs could
reflect the change updating the item in `bookmark-alist`.

That's not what happens, though. Instead, if you move or rename a
file, its Bookmarks will be orphaned. When trying to jump to the file,
not finding it, Emacs would invoke `(bookmark-relocate)` This function
will ask you the new file location, and will then update the
`filename` field for the affected Bookmark.

## Modified files
What instead if the file is not moved, but the specific line where the
Bookmark was set changed, maybe because other lines have been added or deleted?  
This is where the *context awarness* of Bookmarks comes into play.
Read the content of your bookmarks (`M-x describe-variable RET
bookmark-alist RET`). You will see something like:

```emacs-lisp
(("writing"
 (filename . "/home/arialdo/prg/personal/arialdomartini.github.io/_posts/emacs/2024-03-30-emacs-bookmarks.md")
 (front-context-string . "\n\n# Less common ")
 (rear-context-string . "ookmark and the ")
 (position . 4605)
 (last-modified 26400 56170 174644 0))
 ...
```

Not only does each Bookmark contain the exact location in the file, in
the field `position`: it also holds a bit of the surrounding context,
in the `front-context-string` and `rear-context-string` fields. When
jumping to the bookmark, the 3 together are used, in the attempt of
compensating the possibly occurred changes.

## You said Bookmark+?
[Bookmark+][bookmark-plus] is an impressive package that extends what
you can do with Bookmarks. Among many other things:

* It lets you bookmarks URLs, regions of text, buffers not associated
  to a file.
* It lets you store keyboard macros, Lisp functions and variables
  inside bookmarks.
* It lets you organize your bookmarks with tags.
* It lets you manage text snippets.

But this the topic for a separate post.

I wish you a great day and many happy hours of coding!


(Thanks to [Protesilaos][prot] for the kind review).

# References
* [Protesilaos Stavrou][prot]
* [consult][consult]
* [Emacs Manual - Bookmarks][bookmarks]
* [direnv][direnv]
* [Bookmark+][bookmark-plus]

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/29)

[prot]: https://protesilaos.com/coach/
[consult]: https://github.com/minad/consult
[bookmarks]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Bookmarks.html
[direnv]: https://github.com/direnv/direnv
[bookmark-plus]: https://www.emacswiki.org/emacs/BookmarkPlus

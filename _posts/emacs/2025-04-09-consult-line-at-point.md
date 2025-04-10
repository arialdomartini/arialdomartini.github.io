---
layout: post
title: "Emacs: consult-line-symbol-at-point"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- emacs
- lisp
---
I'm addicted to [consult.el][consult]. It is so convenient that when I
found out that `isearch-forward-symbol-at-point (M-s .)` was a thing,
I felt the desire to integrate it with consult.el. Luckily, this was
way easier than I thought.

Let's walk through the steps I took. As usual, along the path, there
are random pearls here and there to be picked.

TL; DR:

```elisp
(defun consult-line-symbol-at-point ()
  "Search for a line matching the symbol found near point."
  (interactive)
  (consult-line
   (or (thing-at-point 'symbol))))
   
(global-set-key (kbd "M-s .") #'consult-line-symbol-at-point)
(global-set-key (kbd "M-s M-s .") #'isearch-forward-symbol-at-point)
```

<!--more-->

## You have no idea how powerful isearch is!
I was fascinated by the post [You have no idea how powerful isearch
is!][bozhidar] by Bozhidar Batsov. In fact, I really had no idea.

One of the tricks he suggests is:

> Type `M-s .` to search for the symbol at point.
> (useful in the context of programming languages)

This is so more convenient than what I was used to do, that was: once
positioned on a symbol,

- `C-=` (`er/expand-region`) to select it
- `M-w` to copy it.
- `C-s` to trigger `consult-line`
- `C-y` to use it as the search pattern.

`M-s .` does the same in one single hit. With the only difference that
it uses `isearch-forward` instead of `consult-line`.

As I mentioned, once tried `consult.el`, I could not do without.
Unfortunately, `consult.el` does not provide
`consult-line-symbol-at-point`. How hard is it to write it?

## How does `isearch-forward-symbol-at-point` work?
It makes sense to figure out first how
`isearch-forward-symbol-at-point` works. Which you can do with
either:

* `M-x describe-key RET M-s .`
* `M-x describe-function isearch-forward-symbol-at-point`

(in turn, `describe-key` and `describe-function` are conveniently
bound respectively to `C-h k` and `C-h f`).

```
M-s . runs the command isearch-forward-symbol-at-point (found in
global-map), which is an interactive native-compiled Lisp function in
`isearch.el`.

It is bound to M-s ..

(isearch-forward-symbol-at-point &optional ARG)
```

From the help page, you can jump to the source code either:

* hitting `s`.
* with `M-x help-view-source`.
* hitting enter while hovering the link `isearch.el`.

Fine. Here's the code:

```elisp
(defun isearch-forward-symbol-at-point (&optional arg)
  (interactive "P")
  (isearch-forward-symbol nil 1)
  (let ((bounds (find-tag-default-bounds))
        (count (and arg (prefix-numeric-value arg))))
    (cond
     (bounds
      (when (< (car bounds) (point))
	(goto-char (car bounds)))
      (isearch-yank-string
       (buffer-substring-no-properties (car bounds) (cdr bounds)))
      (when count
        (isearch-repeat-forward count)))
     (t
      (setq isearch-error "No symbol at point")
      (isearch-push-state)
      (isearch-update)))))
```

Spitting blood (I'm not that good at Lisp), I could extract the part
that identifies the symbol at point with in this function:

```elisp
(defun get-symbol-at-point ()
  (let ((bounds (find-tag-default-bounds)))
    (cond
     (bounds
      (when (< (car bounds) (point))
	(goto-char (car bounds)))
       (buffer-substring-no-properties (car bounds) (cdr bounds)))
     (t ()))))
```

You can interactively test it with:

```elisp
(defun show-symbol-at-point ()
  (interactive)
  (message "%s" (get-symbol-at-point)))
```

Move the point on any symbol and hit `M-x show-symbol-at-point RET`.
Cool: sounds like it is a good starting point.

## Emacs' geological layers?
My good friend [Prot][prot] brought to my attention that in fact a
function `symbol-at-point` is built-in in Emacs. It is part of
[thingatpt.el][thing-at-point.el] and, blaming the source code, it
seems to have been introduced 19 years ago. Indeed, it is used here
and there (check the code of `org-open-at-point-global`,
`describe-symbol`, `imenu--completion-buffer` for example). It's not
completely clear to me why it is not being used in
`isearch-forward-symbol-at-point`. Maybe there are historical reasons,
but I'm a bit confused, since it seems that
`isearch-forward-symbol-at-point` has been introduced after
`thingatpt.el`.

Whatever. Let's try it out, replacing my horrible custom
`get-symbol-at-point` with the standard `symbol-at-point`:

```elisp
(defun show-symbol-at-point ()
  (interactive)
  (message "%s" (symbol-at-point)))
```

Works like a charm!

## How does `consult-line` work?
Now, if we want to build a `consult-line-symbol-at-point` function,
we'd better have a look to `consult-line`'s source code first. Here we
go.

Run `M-x describe-function RET consult-line RET`, then get to the code
with `M-x help-view-source RET` (or `s`).

```elisp
(defun consult-line (&optional initial start)
  "Search for a matching line.

Depending on the setting `consult-point-placement' the command
jumps to the beginning or the end of the first match on the line
or the line beginning.  The default candidate is the non-empty
line next to point.  This command obeys narrowing.  Optional
INITIAL input can be provided.  The search starting point is
changed if the START prefix argument is set.  The symbol at point
and the last `isearch-string' is added to the future history."

  (interactive (list nil (not (not current-prefix-arg))))
  (let* ((curr-line (line-number-at-pos (point) consult-line-numbers-widen))
         (top (not (eq start consult-line-start-from-top)))
         (candidates (consult--slow-operation "Collecting lines..."
                       (consult--line-candidates top curr-line))))
    (consult--read
     candidates
     :prompt (if top "Go to line from top: " "Go to line: ")
     :annotate (consult--line-prefix curr-line)
     :category 'consult-location
     :sort nil
     :require-match t
     ;; Always add last `isearch-string' to future history
     :add-history (list (thing-at-point 'symbol) isearch-string)
     :history '(:input consult--line-history)
     :lookup #'consult--line-match
     :default (car candidates)
     ;; Add `isearch-string' as initial input if starting from Isearch
     :initial (or initial
                  (and isearch-mode
                       (prog1 isearch-string (isearch-done))))
     :state (consult--location-state candidates))))
```

Basically, `consult-line` is just a wrapper around the internal
function `consult--read`. We found something similar when we wanted to
replace `completing-read` with a consult function for getting real-time
preview, in [Emacs: let's zoom](/emacs-zoom#completing-read).  
In the worst hypothesis, we could do the same and use `consult--read`
passing it the symbol at point. But this is not even necessary. Notice
the first `consult-line` parameter, `initial`. If it's not `nil`, then
it will be used as the initial pattern. Run this with `C-x C-e`:

```elisp
(consult-line "whatever")
```

Cool. It seems that all we have to do is to feed `consult-line` with 
`symbol-at-point` as the initial value.

```elisp
(defun consult-line-symbol-at-point ()
  "Search for a line matching the symbol found near point."
  (interactive)
  (consult-line (symbol-at-point)))
   
(global-set-key (kbd "M-s .") #'consult-line-symbol-at-point)
```

Try it out. Nope. We get a `apply: Wrong type argument: stringp, Try`.
This is because `(symbol-at-point)` returns a `symbol`, whereas
`consult-line` wanted a `string`. Fine, there must be a
function to convert symbols to strings, right? I would try with:

```elisp
M-x describe-function RET symbol
```

to list the symbol-related functions. `symbol-name` sounds like a good
candidate. Its documentation says:

> Return SYMBOL's name, a string.

Let't try it out:

```elisp
(defun consult-line-symbol-at-point ()
  "Search for a line matching the symbol found near point."
  (interactive)
  (consult-line (symbol-name (symbol-at-point))))
   
(global-set-key (kbd "M-s .") #'consult-line-symbol-at-point)
```

## nil!
Does it work when there is no symbol at point? `symbol-at-point`
declares:

> Return the symbol at point, or nil if none is found.

`(consult-line)` happily works, if no `initial` pattern (or `nil`) is
used. But the problem is, `(symbol-name nil)` is `"nil"`, so consult
would search for that string. Shall we raise an error, instead?

## DWIM
Let's implement a Do What I Mean behavior instead. If there is no
symbol at point, `M-s .` shall act as a standard `consult-line`.

We may use an `if` clause:

```elisp
(defun consult-line-symbol-at-point ()
  "Search for a line matching the symbol found near point."
  (interactive)
  
  (let ((symbol (symbol-at-point)))
    (if symbol
        (consult-line (symbol-name symbol))
      (consult-line))))
```

Then, to remove the duplicated call to `consult-line` we could swap
`if` and `consult-line`, playing with the fact that:

```elisp
(if condition
    (function when-true)
  (function when-false))
```

can be always written as:

```elisp
(function
 (if condition when-true
   when-false)
```

Here we go:

```elisp
(defun consult-line-symbol-at-point ()
  (interactive)
  
  (let ((symbol (symbol-at-point)))
    (consult-line
     (if symbol
         (symbol-name symbol)
       ()))))
```

Now, 

```elisp
(if condition when-true ()))
```

can be shortened as:

```elisp
(when condition when-true)
```

So:

```elisp
(defun consult-line-symbol-at-point ()
  (interactive)
  
  (let ((symbol (symbol-at-point)))
    (consult-line (when symbol (symbol-name symbol)))))
```

This is already good enough. But if you are curious like a monkey, you
would not refrain from browsing the code of `symbol-at-point`.

## How does `symbol-at-point` work?
Inspecting the code, it turns out that `symbol-at-point` is just a
little wrapper around the more generic `thing-at-point`:

```elisp
(defun symbol-at-point ()
  "Return the symbol at point, or nil if none is found."
  (let ((thing (thing-at-point 'symbol)))
    (if thing (intern thing))))
```

You see that call to `intern`? If you check `intern`'s documentation
(`C-h f intern RET`), you will read:

> Return the canonical symbol whose name is STRING.

Oh! That's why we needed to invoke `symbol-name` to get the string
back! But this also means that if we just invoke `thing-at-point`
instead of `symbol-at-point`, we can save some key hits:

```elisp
(defun consult-line-symbol-at-point ()
  (interactive)
  
  (let ((symbol (thing-at-point 'symbol)))
    (consult-line (when symbol symbol))))
```

Wait a second: what does `thing-at-point` return, when there it no
thing at point? Position the point in an empty part of the buffer,
then invoke it with:

```elisp
M-x eval-expression RET (thing-at-point 'symbol)
```

It returns `nil`! Beautiful, so it means we don't need all that
ceremony about creating the variable `symbol` and checking it with
`when`. We can simplify our code as:

```elisp
(defun consult-line-symbol-at-point ()
  (interactive)
    (consult-line (thing-at-point 'symbol)))
```

There is no greater pleasure than deleting code, is there?

## Have I already told you are a curious monkey?
I know what you are thinking: what is that `'symbol` argument we
passed to `thing-at-point`? Sounds like a *kind of thing* we want to
detect. 

What about what the `thing-at-point` documentation says about the
other possible values?

> THING should be a symbol specifying a type of syntactic entity.
> Possibilities include 'symbol', 'list', 'sexp', 'defun', 'filename',
> 'existing-filename', 'url', 'email', 'uuid', 'word', 'sentence',
> 'whitespace', 'line', 'number', 'face' and 'page'.

And what about this intriguing invitation?

> See the file 'thingatpt.el' for documentation
> on how to define a symbol as a valid THING.

I can not resist, can you? Let's try those other syntactic entity
types, then let's dare to define our first custom one.

## You are repetitive, Arialdo!

Let me found out how many times I keep repeting "Try yourself".

Define: 

```elisp
(defun consult-line-sentence-at-point ()
  (interactive)
  (consult-line (thing-at-point 'sentence)))
```

Try yourself.

Notice how we are asking `thing-at-point` to detect a whole sentence,
not just the single symbol at point.

![consult-line-sentence-at-point in action](static/img/emacs/consult-line-at-point/try-yourself.png)

Cool. I'm not that repetitive, after all.

I have no idea how this would be useful. Maybe detecting sexps could
come more in handy:

```elisp
(defun consult-line-sexp-at-point ()
  (interactive)
  (consult-line (thing-at-point 'sexp)))
```

![consult-line-sexp-at-point in action](static/img/emacs/consult-line-at-point/sexp-at-point.png)


You got the idea. This bears the question: how to define a new custom
type of thing? We have no choice but to follow the invitation:

> See the file 'thingatpt.el' for documentation
> on how to define a symbol as a valid THING.

Let's go. `M-x find-library RET thingatpt RET`

## Providers
Browsing around, you should stumble upon this variable:

```elisp
(defvar thing-at-point-provider-alist nil
  "Alist of providers for returning a \"thing\" at point.
This variable can be set globally, or appended to buffer-locally
by modes, to provide functions that will return a \"thing\" at
point.  The first provider for the \"thing\" that returns a
non-nil value wins.

For instance, a major mode could say:

(setq-local thing-at-point-provider-alist
            (append thing-at-point-provider-alist
                    '((url . my-mode--url-at-point))))

to provide a way to get an `url' at point in that mode.  The
provider functions are called with no parameters at the point in
question.
```

Sounds like our culprit.

## A provider for Double Quoted String
Imagine we want to write `consult-line-quoted-string-at-point`, which
uses as the search pattern the whole string surrounded by double
quotes, at point. You could use it to find duplicated strings in code:

![consult-line-quoted-string-at-point in action](static/img/emacs/consult-line-at-point/quoted-string-at-point.png)

What we should do is to define a new provider, `get-text-in-quotes`,
able to detect quoted string around the point, and to push it either
to the list of the global or the local thing-at-point providers:


Here's a function that would detect the position of `"` before and
after point:

```elisp
(defun get-text-in-quotes-at-point ()
  (interactive)
  (save-excursion
    (let (start end)
      (search-backward "\"" nil t)
      (setq start (point))
      (forward-char)
      (search-forward "\"" nil t)
      (setq end (point))
      (buffer-substring-no-properties start end))))
```

(setq thing-at-point-provider-alist
            (append thing-at-point-provider-alist
                    '((quoted-string . get-text-in-quotes))))
                    
```elisp
(defun consult-line-quoted-string-at-point ()
  "Search for a line matching the quoted string found near point."
  (interactive)
  (consult-line
   (or (thing-at-point 'quoted-string))))
   
(global-set-key (kbd "M-s .") #'consult-line-quoted-string-at-point)
(global-set-key (kbd "M-s M-s .") #'isearch-forward-symbol-at-point)


# References


* [consult][consult]
* [You have no idea how powerful isearch is!][bozhidar]
* [Protesilaos Stavrou][prot]
* [thingatpt.el][thing-at-point.el]
* [DWIM][dwim]

[consult]: https://github.com/minad/consult
[prot]: https://protesilaos.com/
[bozhidar]: https://emacsredux.com/blog/2025/03/18/you-have-no-idea-how-powerful-isearch-is/
[thing-at-point.el]: https://www.emacswiki.org/emacs/ThingAtPoint
[dwim]: https://www.emacswiki.org/emacs/DoWhatIMean

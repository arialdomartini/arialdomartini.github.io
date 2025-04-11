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
I immediately felt the desire to integrate it with
consult.el. Luckily, this was way easier than I thought.

Let's walk through the steps I took. As often happens with Emacs,
along the path of exploring its source code, we will find some random
pearls here and there to pick.

## TL; DR:

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
This issue is based on a lesson I got from [Protesilaos][prot].

## You have no idea how powerful isearch is!
I was fascinated by the post [You have no idea how powerful isearch
is!][bozhidar] by [Bozhidar Batsov][bozhidar-home]. In fact, I really had
no idea.

One of the tricks he suggests is:

> Type `M-s .` to search for the symbol at point.<br/>
> (useful in the context of programming languages)

It's very convenient: when your point is over a symbol, just hit `M-s .`
to find other occurrences. Emacs will do its best to figure out what
*symbol* means.

`M-s .` (that is, `isearch-forward-symbol-at-point`) is so more
convenient than what I was used to do: once positioned on a
symbol,

- `C-=` (`er/expand-region`) to select it
- `M-w` to copy it.
- `C-s` to trigger `consult-line`
- `C-y` to use it as the search pattern.

`M-s .` does the same in one single shot. With the only difference
that, alas!, it uses `isearch-forward` instead of `consult-line`.  
As I mentioned, once tried `consult.el`, I could not do without it
anymore. ~~Unfortunately, `consult.el` does not provide
`consult-line-symbol-at-point`~~.

How hard is it to write it?

Edit: it turns out (thank you @Crandel!) that the consult.el [README](https://github.com/minad/consult/blob/30a42ac8f3d42f653eda234ee538c1960704f4f2/README.org#fine-tuning-of-individual-commands)
suggests obtaining this result simply with:

```elisp
(consult-customize
 consult-line
 :add-history (seq-some #'thing-at-point '(region symbol)))

(defalias 'consult-line-thing-at-point 'consult-line)

(consult-customize
 consult-line-thing-at-point
 :initial (thing-at-point 'symbol))
```

Although next time I should RTFM!, I don't regret not knowing this:
learning it by hacking has been a rewarding experience.

## How does `isearch-forward-symbol-at-point` work?
It makes sense to figure out first how
`isearch-forward-symbol-at-point` works. Which you can do with
either:

* `M-x describe-key RET M-s .`
* `M-x describe-function isearch-forward-symbol-at-point`

(in turn, `describe-key` and `describe-function` are conveniently
bound respectively to `C-h k` and `C-h f`). You will get this
documentation:

```
M-s . runs the command isearch-forward-symbol-at-point (found in
global-map), which is an interactive native-compiled Lisp function in
`isearch.el`.

It is bound to M-s ..

(isearch-forward-symbol-at-point &optional ARG)
```

From this help page, you can jump to the source code either:

* hitting `s`.
* running `M-x help-view-source`.
* hitting Enter while hovering the link `isearch.el`.

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
that identifies the symbol at point with this function:

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

I'm honest: I cannot understand it completely. However, it's easy to
verify that it actually works:

```elisp
(defun show-symbol-at-point ()
  (interactive)
  (message "%s" (get-symbol-at-point)))
```

Move the point on any symbol and execute `M-x show-symbol-at-point
RET`. Cool: sounds like it is a good starting point.

## Emacs' geological layers?
My good friend [Prot][prot] brought to my attention that in fact a
function `symbol-at-point` is built-in in Emacs. It is part of
[thingatpt.el][thing-at-point.el] and a `git blame` reveals it has been
introduced 19 years ago. Indeed, it is used here and there (check the
code of `org-open-at-point-global`, `describe-symbol`,
`imenu--completion-buffer` for example). It's not completely clear to
me why it has not been used in `isearch-forward-symbol-at-point` too.
Maybe there are historical reasons. This confuses me, since
`isearch-forward-symbol-at-point` has been introduced after
`thingatpt.el`.

Whatever.  
Let's try it out, replacing my horrible custom `get-symbol-at-point`
with the standard `symbol-at-point`:

```elisp
(defun show-symbol-at-point ()
  (interactive)
  (message "%s" (symbol-at-point)))
```

Works like a charm! Good: we know how to get the symbol at point.
We will have to pass it somehow to `consult-line`.

## How does `consult-line` work?
Now, if we want to build a `consult-line-symbol-at-point` function,
we'd better have a look to `consult-line`'s source code first.

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

That's it, not so huge, afterall. Basically, `consult-line` is just a
thin wrapper around the internal function `consult--read`. We found
something similar when we wanted to replace `completing-read` with a
consult function, for getting
real-time preview in [Emacs: let's zoom](/emacs-zoom#completing-read).  
In the worst hypothesis, we could do the same and use this internal
`consult--read`, passing it the symbol at point. But this is not even
necessary. Notice the first `consult-line` parameter, `initial`? If
it's not `nil`, then it will be used as the initial pattern.  
Let's see how it works. Evaluate this with `C-x C-e`:

```elisp
(consult-line "whatever")
```

Cool. It seems that really all we have to do is to feed `consult-line`
with `symbol-at-point` as the initial value.

```elisp
(defun consult-line-symbol-at-point ()
  "Search for a line matching the symbol found near point."
  (interactive)
  (consult-line (symbol-at-point)))
   
(global-set-key (kbd "M-s .") #'consult-line-symbol-at-point)
```

Try it out. Nope. We get an 

```
apply: Wrong type argument: stringp, Try
```

This is because `(symbol-at-point)` returns a `symbol`, whereas
`consult-line` wants a `string`. Fine: there must be a function to
convert symbols to strings, right? I would try with:

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

Woah. We are almost there.

## nil!
Does this work when there is no symbol at point? `symbol-at-point`
declares:

> Return the symbol at point, or nil if none is found.

`(consult-line)` happily works when the `initial` pattern is `nil`
&mdash; or if it omitted. But the problem in our code is with
`symbol-name`: `(symbol-name nil)` is `"nil"`, as a string. So
consult.el would search for that string. Mumble mumble, shall we raise
an error, in that case?

## DWIM
There's a better alternative: to implement a [Do What I Mean][dwim]
behavior. That is, if there is no symbol at point, `M-s .` shall act
as a standard `consult-line`, if it's over a symbol, it would search
for that symbol.

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

Now, an `if` nested in a `let` can be shortened with a `if-let`:


```elisp
(defun consult-line-symbol-at-point ()
  "Search for a line matching the symbol found near point."
  (interactive)
  
  (if-let ((symbol (symbol-at-point)))
        (consult-line (symbol-name symbol))
      (consult-line)))
```

But we can do better than this. We can remove the duplicated call to
`consult-line` swapping `if` and `consult-line`, playing with
the fact that:

```elisp
(if condition
    (function when-true)
  (function when-false))
```

can be always written as:

```elisp
(function
 (if condition 
     when-true
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

Now, if you are curious as I am, it’s impossible not to take a closer
look to the code of `symbol-at-point`. Which reveals a possible
further improvement.

## How does `symbol-at-point` work?
It turns out that also `symbol-at-point` is just a little wrapper, in
this case around the more generic `thing-at-point`:

```elisp
(defun symbol-at-point ()
  "Return the symbol at point, or nil if none is found."
  (let ((thing (thing-at-point 'symbol)))
    (if thing (intern thing))))
```

You see that call to `intern`? If you check `intern`'s documentation
(`C-h f intern RET`), you will read:

> Return the canonical symbol whose name is STRING.

Now we see why we needed to invoke `symbol-name`! `symbol-at-point`
converts the string to a symbol, so we needed `symbol-name` to get the
string back! It makes sense to directly invoke `thing-at-point`, then:

```elisp
(defun consult-line-symbol-at-point ()
  (interactive)
  
  (let ((symbol (thing-at-point 'symbol)))
    (consult-line (when symbol symbol))))
```

Wait a second: what does `thing-at-point` return, when there is no
thing at point? Move the point in an empty part of the buffer,
then invoke it with:

```elisp
M-x eval-expression RET (thing-at-point 'symbol)
```

It returns `nil`! We are lucky: it means that we don't need all the
ceremony about creating the variable `symbol` and checking its value
with `when`. We can simplify our code as:

```elisp
(defun consult-line-symbol-at-point ()
  (interactive)
    (consult-line (thing-at-point 'symbol)))
```

There is no greater pleasure than deleting code, is there?

## Have I already told you are curious as a monkey?
I know what you are asking yourself: what is that `'symbol` argument
we passed to `thing-at-point`? Sounds like the *kind of thing* we want
to detect.  
And what about that part of the `thing-at-point` documentation that
mentions other possible values?

> THING should be a symbol specifying a type of syntactic entity.
> Possibilities include 'symbol', 'list', 'sexp', 'defun', 'filename',
> 'existing-filename', 'url', 'email', 'uuid', 'word', 'sentence',
> 'whitespace', 'line', 'number', 'face' and 'page'.

And, finally, what about this intriguing invitation?

> See the file 'thingatpt.el' for documentation
> on how to define a symbol as a valid THING.

I can not resist, can you? We have to try those other *syntactic
entity* types, then we have to challenge ourselves defining our first
custom *thing-at-point*.

## You are repetitive, Arialdo!
Let me find out how many times I kept repeating "Try yourself" in his
post.

Define: 

```elisp
(defun consult-line-sentence-at-point ()
  (interactive)
  (consult-line (thing-at-point 'sentence)))
```

Try yourself.

Notice how we are asking `thing-at-point` to detect a whole sentence,
not just the single symbol at point.

![consult-line-sentence-at-point in action](static/img/emacs/consult-line-at-point/try-yourself.png){:width="100%"}

Cool. I'm not that repetitive, after all.  
Honestly, I have no idea how this would be useful. Maybe detecting
sexps could come more in handy:

```elisp
(defun consult-line-sexp-at-point ()
  (interactive)
  (consult-line (thing-at-point 'sexp)))
```

![consult-line-sexp-at-point in action](static/img/emacs/consult-line-at-point/sexp-at-point.png){:width="100%"}


You got the idea.

This bears the question: how to define a new custom
type of thing? We have no choice but to try.  
The documentation is clear:

> See the file 'thingatpt.el' for documentation
> on how to define a symbol as a valid THING.

So, let's read the library code! `M-x find-library RET
thingatpt RET`

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

If you recap what we discovered until this point, you probably agree
that this is a beautiful design trait of Emacs:

* `isearch-forward-symbol-at-point` focuses on *searching* things,
  seen as strings.
* `thing-at-point` focuses on returning that *thing*. It does that by
delegating the job to a customizable collection of *thing*-providers.
* Each *thing*-provider implements some custom, arbitrary logic to
  detect a *thing* given the position in a buffer.

Indeed, we were able to easily create our
`consult-line-symbol-at-point` because of this modular nature.

Time to challenge ourselves to define a *thing* that is not natively
supported by `isearch-forward-symbol-at-point`.


## A provider for literal strings, via Tree Sitter
Imagine we want to write `consult-line-literal-string-at-point`, to
search for occurrences of the whole literal string at point, whatever
buffer's programming language defines as a *literal string*. You could
use it to find duplicated strings in code:

![consult-line-literal-string-at-point in action](static/img/emacs/consult-line-at-point/literal-string-at-point.png){:width="100%"}

What we should do is to define a new provider,
`ts-get-literal-string-at-point`, a function whose goal is to return
the literal string around the point. Notice the `ts-` in the name?
That stands for Tree-sitter. Indeed, we will rely on whatever
Tree-sitter grammar is active in the current buffer.

Once defined the provider, we will need to register it in the list of
the global or the local *thing-at-point* providers. At that moment, we
will get the chance to associate it to a symbol of our choice (we will
use `str_lit`).

Once done that, `thing-at-point` will be able to detect literal
strings at point (using the symbol we chose).

(Pause a minute to reflect again on the modular design of Emacs: Tree
Sitter has been conceived in 2018, independently from Emacs and when
Emacs was already 34. What we are really doing here is connecting some
prehistoric Emacs machinery with Tree-sitter, the brand new kid on the
block. There must be some deep beauty in the Emacs design if this
works without any gimmick).

So, here's the provider:

```elisp
(defun ts-get-literal-string-at-point ()
  "Return the string node at point using Tree-sitter, or nil if none is found."
  (let ((node (treesit-node-at (point))))
    (when (equal (treesit-node-type node) "str_lit")
          (treesit-node-text node))))
```

It works like this:

* It asks Tree-sitter to get the node at point, with `(treesit-node-at
  (point))`.
* It checks the node type is of a literal string, `(equal ... "str_lit")`,
* then it returns its content `(treesit-node-text node)`.
* Otherwise, it just returns `nil`.

And here is how we can (globally) register it:

```elisp
(setq thing-at-point-provider-alist
            (append thing-at-point-provider-alist
                    '((str_lit . ts-get-literal-string-at-point))))
```

In a real case scenario, this would be done by a major mode, so most
likely using `setq-local` instead of `setq`, inside a hook. If you are
curious how major modes and hooks work in Emacs, you might give a read
to [Emacs: how to
activate the functionality X for all files of type
Y?](emacs-hooks).

Finally, let's have a function connecting `thing-at-point` against
`str_lit` with `consult`:

```elisp
(defun consult-line-literal-string-at-point ()
  (interactive)
  (consult-line (thing-at-point 'str_lit)))


(global-set-key (kbd "M-s C-s .") #'consult-line-literal-string-at-point)
```

It really works! Try yourself.

(Ouch! I repeated myself)


# References


* [consult][consult]
* [You have no idea how powerful isearch is!][bozhidar]
* [Protesilaos Stavrou][prot]
* [thingatpt.el][thing-at-point.el]
* [DWIM][dwim]
* [Bozhidar Batzov - Emacs Redux][bozhidar]

[consult]: https://github.com/minad/consult
[prot]: https://protesilaos.com/
[thing-at-point.el]: https://www.emacswiki.org/emacs/ThingAtPoint
[dwim]: https://www.emacswiki.org/emacs/DoWhatIMean
[bozhidar]: https://emacsredux.com/blog/2025/03/18/you-have-no-idea-how-powerful-isearch-is/
[bozhidar-home]: https://emacsredux.com/

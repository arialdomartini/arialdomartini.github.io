---
layout: post
title: "Emacs: a peek under Imenu's hood"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- emacs
- lisp
---
I promised myself to write a Tree-sitter major mode for F#. I've been
told that it should not be such an insurmountable endeavour, but given
that I start from the scratch, along the journey I will need to learn
a bunch of new topics from the ground up. Nice, that's why we use
Emacs, right?

For each of those themes, I will publish a little blog post to share
the lesson learnt.

Today it's the Imenu's turn.

<!--more-->

Imenu is an Emacs feature that produces menu items for accessing
locations in buffers. Here's the one for this very blog post:

<img width="100%" src="/static/img/emacs/imenu/imenu.gif" />

If you are a programmer it is very likely that you have switched the
visual menu off with:

```elisp
(menu-bar-mode -1)
```

and that you prefer using the keyboard. Luckily for us keeb fans,
Imenu happily works in the minibuffer too. Try yourself:

- Open a file for which you have a major mode installed.
- Run `M-x imenu`.

If you have [consult.el][consult] installed, you would surely prefer
`M-x consult-imenu` which offers narrowing and real-time preview.

Why would an F# mode need Imenu in the first place? Because you would
use it to jump to function definitions, variables and other structural
elements of the code. Together with `consult-outline` this is one of
the most convenient ways to reason about the structure of the code
module you are visualizing.

So, our long term goal is to learn how to extract the structural
elements of an F# buffer and to accordingly configure Imenu.

It turns out that:

- Imenu was initially conceived to work mainly using regular
  expressions.
- Tree-sitter offers a completely different way to extract structural
  elements from code, which saves major modes developers from dealing
  with regular expressions (because, as the saying goes "the plural of
  regex is regrets").
  
We start our journey learning the classical way to use Imenu. We will
cover the integration between Tree-sitter and Imenu in a second blog post.

## Imenu 

Let's start with a simple case. Open the scratch buffer and past this
content:

```
(defun greet ()
  (message "Hello, world!"))

(defun be-kind ()
  (message "OK, I will"))
```

Then run `imenu`: you should see those 2 functions listed. Selecting
one, will get you to its definition. This is basically what Imenu does.

Besided *what* it does, we want to learn more *how* it actually works.
So, let's start delving into some details.

## imenu--index-alist
At its core, Imenu just displays the content of the (private) variable
`imenu--index-alist`. Try yourself. Again in the scratch buffer, paste and
evaluate (`C-x C-e`) the following:

```elisp
(setq imenu--index-alist
  `(("beginning" . 1)
    ("third char" . 3)
    ("fifth char" . 5)))
```

Then execute `imenu` again. No matter what the Imenu content was
before, by overwriting `imenu--index-alist` you have just hijacked it.
You can even display it in the visual menu bar:

```elisp
(menu-bar-mode 1)
(imenu-add-to-menubar "My menu")
(menu-bar-open)
```

Select any of those 3 items to learn that the numbers in the
`imenu--index-alist` alist are absolute buffer positions. Nice to know.

Notice that besides the 3 items you hard-codeed in
`imenu--index-alist` there is a 4th item `*Rescan*`. Selecting it will
restore the original Imenu items to the 2 `greet` and `be-kind`
functions.

Another experiment you can do is:

- Evaluate again

```elisp
(setq imenu--index-alist
  `(("beginning" . 1)
    ("third char" . 3)
    ("fifth char" . 5)))
```

- Run `consult-imenu`.

You will not see those 3 hard-coded items. Instead, it seems that
`consult-imenu` invokes `*Rescan*` when it starts.

Wrapping up, our first intuition could be that:

- `imenu` displays the content of `imenu--index-alist`.
- `imenu--index-alist` is populated by some function which is invoked
  when `*Rescan*` is selected.
- `consult-imenu` refreshes the content of `imenu--index-alist` when
  it starts.


## Imenu scanning
So, when you visit a buffer `imenu--index-alist` does not start empty:
it already contains some reasonable values. We can infer that some
function must have automatically run, that inserted the proper items.
If you reset `imenu--index-alist`:

```
(setq imenu--index-alist nil)
```

and you run again `imenu`, you can verify that `greet` and `be-kind`
are back: Imenu has rescaned the buffer and detected them.

Which function in Emacs populates `imenu--index-alist`?  
The answer is: it's up to you. Imenu does not care, you can plug in
any function you like, as long as it returns an alist to use.

If you poke into the Imenu code, you will find this snippet as part
of the `imenu--make-index-alist` function:

```elisp
(setq imenu--index-alist
      ...
      ...
      (funcall imenu-create-index-function))
```

Here's the function that sets the value for `imenu--index-alist`!  
So, Imenu expects that someone or something provides it with a
function to scan the buffer and to return an index of menu item. This
*something* is normally the major mode of the buffer.

To challenge this, let's activate a major mode and let's discover
which function `imenu-create-index-function` targets:

- Visit a Markdown file and run `M-x describe-variable RET
imenu-create-index-function`. It reveals that the function used by
Imenu is `markdown-imenu-create-nested-index`. Apparently, an
implementation very specific to Markdown.

- If you visit a C file, instead, the value of
`imenu-create-index-function` is
`imenu-default-create-index-function`, which sounds more generic.

- The same happens for a lot of other major modes.

So, interestingly: some major modes provide a custom way for scanning
their buffers and indexing the Imenu; others rely on a default
function, provided by Imenu itself.

This provides a hint about what a major mode is: a function providing
a collection of variable values. (If you want to know a bit more about
major modes, you might like [Emacs: how to activate the functionality
X for all files of type Y?](/emacs-hooks)).

It also gives us some suggestions:

- Should not we find a better method, when developing the Tree-sitter
mode for F# we can always develop a completely custom Imenu index
function, and we can plug it into Imenu through
`imenu-create-index-function`.

- This `imenu-default-create-index-function` default implementation
sounds inspiring. It's used by a variety of independent major modes,
therefore it must offer some flexible options to customize it. Let's
investigate.

So, our next 2 goals to challenge our discovery can be:

- We write a completely custom index function, and we plug it into
  Imenu.
- Then, we study what the default Imenu index function offers.

## A custom imenu-create-index-function
Open the scratch buffer (`M-x scratch-buffer RET`) and evaluate:

```elisp
(defun my-index-function ()
  '(("one" . 1)
    ("two" . 2)
    ("three" . 3)))

(setq imenu-create-index-function #'my-index-function)
```

Then reopen `imenu` and rescan the index (or just use `consult-imenu`
which we learnt always rescans the index).  
Yes! We see the 3 items.

Of course, our function needn't return hard-coded values; it can
contain any custom logic. It could generate an index in a cycle:

```elisp
(defun my-index-function ()
  (cl-loop for i from 1 to 50
           collect (cons (format "Item %d" i) i)))
```

or it could actually scan the buffer searching for `(defun FUNCNAME`
patterns:

```elisp
(defun my-index-function ()
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (not (eobp))
             for item = (when (looking-at "(defun\\s-+\\(\\S-+\\)")
                          `(,(match-string 1) . ,(point)))
             if item collect item
             do (forward-line 1))))
```

Cool. This gives up hope to integrate Imenu with Tree-sitter. We
could:

- ask Tree-sitter to analyze the buffer and to return back the
  syntax tree for our F# file.
- use some custom logic to scan the syntax tree &mdash; instead of the
  chars in the buffer &mdash; building one item for each of the F#
  structural elements we want to index.
  
Way better than working with regular expressions. We will get to it.

## The default imenu index function
Time to see what's inside the default index function, provided
out-of-the-box by `imenu`.

- Open the function documentation with `M-x describe-function RET
  imenu-default-create-index-function RET`.
- Access its code with `M-x help-view-source RET` (or just type `s`).
  
This reveals this implementation (for some reason, with a wrong
indentation), which is not too complicated:

```elisp
(defun imenu-default-create-index-function ()
  "Default function to create an index alist of the current buffer.

The most general method is to move point to end of buffer, then repeatedly call
`imenu-prev-index-position-function' and `imenu-extract-index-name-function'.
All the results returned by the latter are gathered into an index alist.
This method is used if those two variables are non-nil.

The alternate method, which is the one most often used, is to call
`imenu--generic-function' with `imenu-generic-expression' as argument."
  ;; These should really be done by setting imenu-create-index-function
  ;; in these major modes.  But save that change for later.
  (cond ((and imenu-prev-index-position-function
	      imenu-extract-index-name-function)
	 (let ((index-alist '()) (pos (point-max))
               (start (float-time))
	       name)
	   (goto-char pos)
	   ;; Search for the function
	   (while (and (funcall imenu-prev-index-position-function)
                       ;; Don't use an excessive amount of time.
                       (< (- (float-time) start) imenu-max-index-time))
             (unless (< (point) pos)
               (error "Infinite loop at %s:%d: imenu-prev-index-position-function does not move point" (buffer-name) pos))
             (setq pos (point))
	     (save-excursion
	       (setq name (funcall imenu-extract-index-name-function)))
	     (and (stringp name)
 		  ;; [ydi] Updated for imenu-use-markers.
		  (push (cons name
                              (if imenu-use-markers (point-marker) (point)))
			index-alist)))
	   index-alist))
	;; Use generic expression if possible.
	((and imenu-generic-expression)
	 (imenu--generic-function imenu-generic-expression))
	(t
         (imenu-unavailable-error "This buffer cannot use `imenu-default-create-index-function'"))))
```

Reduced to its core, it looks like the following:

```elisp
(defun imenu-default-create-index-function ()
  (cond ((and imenu-prev-index-position-function
	          imenu-extract-index-name-function)
	          
              ;; [code omitted]
              ;; index using those 2 functions

	    ((and imenu-generic-expression)
	     (imenu--generic-function imenu-generic-expression))
	    (t
         (imenu-unavailable-error "This buffer cannot use `imenu-default-create-index-function'"))))
```

Basically:

1. It checks if the major mode provides 2 specific functions for
  indexing (one for positioning, one for extracting). If so, it uses
  them. We will see how this work in a minute.
2. Otherwise, it checks if the major mode provides a set of regular
  expressions (in the variable `imenu-generic-expression`). If so, it
  uses them, delegating the work to the function
  `imenu--generic-function`.
3. Otherwise, it raises an error.

Let's start from the case 2, which is by far and large the most
popular &mdash; but not necessarily the most powerful &mdash; usage of
Imenu.

### imenu-generic-expression
So,/ this is the case in which `imenu--generic-function` is passed the
regular expressions defined in `imenu-generic-expression`. I'm curious
to inspect both. Notice that `imenu--generic-function` is private, so
it's not meant to be modified. `imenu-generic-expression`, instead, is
meant to be customized.
  
If you visit the scratch buffer and you inspect the value of
`imenu-generic-expression` you will find a collection of very well
crafted regular expressions:


```elisp
  ((nil "^\\s-*(\\(cl-def\\(?:generic\\|ine-compiler-macro\\|m\\(?:acro\\|ethod\\)\\|subst\\|un\\)\\|def\\(?:advice\\|generic\\|ine-\\(?:advice\\|compil\\(?:ation-mode\\|er-macro\\)\\|derived-mode\\|g\\(?:\\(?:eneric\\|lobal\\(?:\\(?:ized\\)?-minor\\)\\)-mode\\)\\|inline\\|m\\(?:ethod-combination\\|inor-mode\\|odify-macro\\)\\|s\\(?:etf-expander\\|keleton\\)\\)\\|m\\(?:acro\\|ethod\\)\\|s\\(?:etf\\|ubst\\)\\|un\\*?\\)\\|ert-deftest\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
   (nil "^\\s-*(\\(def\\(?:\\(?:ine-obsolete-function-\\)?alias\\)\\)\\s-+'\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
   ("Variables" "^\\s-*(\\(def\\(?:c\\(?:onst\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\)\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
   ("Variables" "^\\s-*(defvar\\(?:-local\\)?\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)[[:space:]
  ]+[^)]" 1)
   ("Types" "^\\s-*(\\(cl-def\\(?:struct\\|type\\)\\|def\\(?:class\\|face\\|group\\|ine-\\(?:condition\\|error\\|widget\\)\\|package\\|struct\\|t\\(?:\\(?:hem\\|yp\\)e\\)\\)\\)\\s-+'?\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2))
```

I don't even dare an attempt to understand. I'm just too grateful to
whoever took the time to write them. Chapeau!  
I am more interested in answering the questions:

- In which file of Emacs is this value defined?
- How is this used by Imenu? What does `imenu--generic-function` do
  with this collecgtion?

To answer the first question, we can follow our intuition: it's up to
the major mode to configure Imenu. So, we can:

- Inspect which major mode is used by the current buffer, with `M-x
  describe-mode`.
- Navigate to its source code, from the file link mentioned in the
  documentation.
- Search for a reference to `(setq-local imenu-generic-expression`.
- If no result is found, we navigate to the parent major mode,
  following the expressions like `(define-derived-mode current-mode
  parent-mode` hitting `M-.` on `parent-mode` (again, read the [post
  about hooks](/emacs-hooks) to learn more).
  
Doing this exercise starting from the scratch buffer takes us to the
file `lisp-mode.el` where a giant variable
`lisp-imenu-generic-expression` is defined:

```elisp
(defvar lisp-imenu-generic-expression
  (list
   (list nil
	 (purecopy (concat "^\\s-*("
			   (regexp-opt
			    '("defun" "defmacro"
                              ;; Elisp.
                              "defun*" "defsubst" "define-inline"
			      "define-advice" "defadvice" "define-skeleton"
			      "define-compilation-mode" "define-minor-mode"
			      "define-global-minor-mode"
			      "define-globalized-minor-mode"
  ...
  ;; [code omitted]
```


Mystery solved.

Now, we just need to understand how this collection of regular
expressions is used. If you remember, the default implementation of
the index function `imenu-default-create-index-function` mentioned:

```
	    ((and imenu-generic-expression)
	     (imenu--generic-function imenu-generic-expression))
```

Let's give a look to the code of `imenu--generic-function`:

- Do a `M-x describe-function RET imenu--generic-function RET`.
- Then `M-x help-view-source` (or just hit `s`).

Phew! This one is way more complex than the one before. What is
relevant for us is:

- It maps over the elements of `imenu-generic-expression`, the
  collection of regexen. We expected that.

- For each regex, it starts with a `(goto-char (point-max))`. This
  means, it scans the buffer backwards, allegedly "for convenience of
  adding items in order".

- Then, it collectes each matching candidate.

OK, not too alien. The reason why the function is so long is because
it also takes into account error handling, duplicates, execution time,
edge cases etc. We can skate over it.

Instead, let's put it into play. Type in the scratch buffer:

```elisp
- buy apples
- buy bananas
- sell ananas

(setq-local imenu-create-index-function #'imenu-default-create-index-function)

(setq imenu-generic-expression
  '(("to buy" "^- buy \\(.*\\)$" 1)
    ("to sell" "^- sell \\(.*\\)$" 1)))
```

Each element in the `imenu-generic-expression` shall follow this
pattern:

```elisp
(MENU-TITLE REGEXP INDEX)
```

where

* `MENU-TITLE` is the title of the submenu to host the item.
* `REGEXP` is the regular expression to capture the candidate. The
  regex may contain a group.
* `INDEX` is the group number to use as the item to be displayed. `0`
means the whole match.

There are other options, which you can consult visiting `M-x
describe-variable RET imenu-generic-expression RET`.

We will not deepen this approach any further: we know that the F#
major mode will rely on the syntax tree provided by Tree-sitter, not
on regular expressions. 

Let's see, instead, the other option offered by the default index
function, `imenu-default-create-index-function`. 

## Find position and extract
Remember that we summarized `imenu-default-create-index-function` as:

```elisp
(defun imenu-default-create-index-function ()
  (cond ((and imenu-prev-index-position-function
	          imenu-extract-index-name-function)
	          
              ;; [code omitted]
              ;; index using those 2 functions

	    ((and imenu-generic-expression)
	     (imenu--generic-function imenu-generic-expression))
	    (t
         (imenu-unavailable-error "This buffer cannot use `imenu-default-create-index-function'"))))
```

and that we said that the first branch of the condition:

- uses a combination of 2 functions,
  `imenu-prev-index-position-function` and
  `imenu-extract-index-name-function`. The former finds the position
  of the next candidate; the latter builds the index item for that
  candidate.

It's time to say more about what the omitted code does. It was:

```elisp
(let ((index-alist '()) (pos (point-max))
      (start (float-time))
	  name)
  (goto-char pos)
  ;; Search for the function
  (while (and (funcall imenu-prev-index-position-function)
              ;; Don't use an excessive amount of time.
              (< (- (float-time) start) imenu-max-index-time))
    (unless (< (point) pos)
      (error "Infinite loop at %s:%d: imenu-prev-index-position-function does not move point" (buffer-name) pos))
    (setq pos (point))
	(save-excursion
	  (setq name (funcall imenu-extract-index-name-function)))
	(and (stringp name)
 		 ;; [ydi] Updated for imenu-use-markers.
		 (push (cons name
                     (if imenu-use-markers (point-marker) (point)))
			   index-alist)))
  index-alist)
```

Indulge me while to simplify it in this naïve way:

```elisp
(let ((index-alist '())
      (pos (point-max))
      (name nil))
  
  (goto-char pos)

  (while (funcall imenu-prev-index-position-function)
    (setq pos (point))
	(save-excursion
	  (setq name (funcall imenu-extract-index-name-function)))
	(and (stringp name)
		 (push (cons name (point))
			   index-alist)))
  index-alist)
```

We can interpret it as:

- It starts from the end. As before, then, it scans the buffer
  backward.
- As long as it finds candidates, it runs a cycle.
- For each step of the cycle it makes use of 2 custom functions:
  - `imenu-prev-index-position-function`, which has the goal of
    finding a candidate and moving the point there.
  - `imenu-extract-index-name-function` which, given the found
    candidate, has the goal to calculate the menu item.

- When it's done, it returns the collection of items.

OK, fair enough. Let's give it a shot with a silly example which
creates a menu item for each line in the buffer:

```elisp
(defun find-all-lines ()
  "Move point to previous lines, returning t each time.
Returns nil when reaching buffer start, signaling completion."
  (if (= (point) (point-min))
      nil
    (forward-line -1)
    t))

(defun get-line-number ()
  "Return the current line number as the index name."
  (format "Line %d" (line-number-at-pos)))

(setq-local imenu-prev-index-position-function #'find-all-lines)
(setq-local imenu-extract-index-name-function #'get-line-number)
```

* `find-all-lines` always indicates that the beginning of each line is
  a good candidate.
* Given the start of line position, `get-line-number` just returns a
  string with the line number as the menu item name to display.


Run it with `imenu`. Nice, it works.
vvv
## What about Tree-sitter?
So, which approach shall we take to integrate imenu with Tree-sitter?
Sure enough, we won't rely on `imenu-generic-expression` and its
regular expressions. Shall we play with the couple of position+extract
functions? Or will we have to develop a whole brand new custom index
function to replace the standard
`imenu-default-create-index-function`?

It turns out `treesit`, the built-in Tree-sitter package, has some
delightful surprises for us. But this is the topic for the next
episode.

Hang tight! Happy emacsing.

# References

* [consult.el][consult]
* [Emacs: how to activate the functionality X for all files of type Y?](/emacs-hooks)

[consult]: https://github.com/minad/consult


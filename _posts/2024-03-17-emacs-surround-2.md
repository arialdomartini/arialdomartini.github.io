---
layout: post
title: "Emacs: Let's surround! - Promtp the user for inputs"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- Emacs
- Lisp
---
In the [previous installment](emacs-surround) we learnt how to
surround a region with hard-coded `<<<` and `>>>`. Let's learn now how
to interactively ask arguments to the user and to go beyond hard-coded
delimiters.  <!--more-->

Here's the last version we got:

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

As you see, the 2 delimiters are already variables. Making our
function parametric is a matter of promoting `opening-delimiter` and
`closing-delimiter` from let binding to function paramters. Let's
split the function in 2:

* an lower-level function, receiving `opening-delimiter` and
`closing-delimiter` as parameters.
* an entry point that, for now, will continue using the hard-coded
  "<<<" and ">>>".

```emacs-lisp
(defun surround-region--surround (opening-delimiter closing-delimiter)
  "Surround the active region with hard-coded strings"
  (when (region-active-p)
    (save-excursion
      (let ((beginning (region-beginning))
            (end (region-end)))

        (goto-char beginning)
        (insert opening-delimiter)

        (goto-char (+ end (length closing-delimiter)))
	(insert closing-delimiter)))))


(defun surround-region-with-hard-coded-strings ()
  "Surround the active region with hard-coded strings"
  (interactive)
  (surround-region--surround "<<<" ">>>"))
```

`surround-region--surround` could easily be a nested lambda. Notice
the double `--`: by convention, it means the function should be
considered private.

We can finally focus on removing the hard-coded values from
`surround-region-with-hard-coded-strings`.

# Asking interactively to the user
There are several ways to ask input to the user, interactively. The
conventional way revolves around using `(interactive)`.

You have surely notice that `interactive` is invoked at the very
beginning of our function. It is called to specify that the function
can be invoked interactively, either via `M-x` or via a keybinding. In
Emacs lingo, we say that it is a *command*. Try yourself to remove the
call to `interactive`: you will not ble able anymore to find the
function in the completion command list displayed with `M-x`. You will
still be able to invoke it from Lisp, of course.

Also notice that `surround-region--surround` is not defined as
interactive: since it is invoked from
`surround-region-with-hard-coded-strings`, which is interactive, Emacs
will not complain. It's like interactivity is propagated in a call
chain: indeed, commands can always invoke functions.

## Passing `interactive` a string
What is of particular interest for our case is how `interactive` can
be used to ask the user for some inputs, interactively. You can learn
how reading the chapter [using `interactive`][interactive] of the
Emacs manual. Here I am providing just the basic.

If a command has no parameters, just invoke `interactive`:

```emacs-lisp
(defun a-parameterless-command ()
  (interactive)
  (message "Invoked!"))
```

If a command takes a parameter, you have to inform `interactive` so
that it will take care of getting the argument from the user and pass
it to the command. `interactive` takes a string with 2 elements:

* A code, specifying the type of expected parameter (e.g., a string, a
  number, a buffer, a file name).
* A string prompt to be displayed to the user.

As for the code, there are many, documented in the chapter
[interactive codes][interactive-codes]. Let's make an example using
`M`, for strings:


```emacs-lisp
(defun one-parameter-command (name)
  (interactive "MYour name: ")
  (message (format "Hi, %s" name)))
```

Notice:

* The `M` before the prompt string `Your name: `.
* The extra space after `:`

Try it. The value will be passed as the `name` argument.

If your command has multiple parameters, you will need to provide a
multiline string argument to `interactive`, one line per each
parameter, using `\n` as a separator. Here's an example:

```emacs-lisp
(defun repeat-sentence (sentence n)
  (interactive "MSentence: \nnNumber: ")
  (dotimes (_ n)
    (insert sentence)))
```

By the way: if you want to insert a newline while interactively
inputing a value, type `C-q C-j`:

* `C-q (quoted-insert)` reads the next input character and inserts it.
* `C-j (electric-newline-and-maybe-indent)` which has the same effect
  of a newline.


There are several other parameter types supported out-of-the-box. For
example, the following (useless and redundant) command lets you switch
to another buffer:

```emacs-lisp
(defun jump-to-existing-buffer (buffer)
  (interactive "bSwitch to: ")
  (switch-to-buffer buffer))
```

Notice two important aspects:

* When you run it, Emacs will provide you with all the facilities to
  select a buffer. For example, if you have Consult and Marginalia
  installed, these will be used to display the list.
* `switch-to-buffer` is itself another interactive command. If you run
  it directly, it will interactively ask you for the buffer. Yet, when
  run from our `jump-to-existing-buffer` no further questions are
  asked.
  
The last point is important. Let's investigate on the source code of
`switch-to-buffer`:

```emacs-lisp
(defun switch-to-buffer (buffer-or-name &optional norecord force-same-window)
  "Display buffer BUFFER-OR-NAME in the selected window. [...]"

(interactive
   (let ((force-same-window
          (unless switch-to-buffer-obey-display-actions
             [...]
     (list (read-buffer-to-switch "Switch to buffer: ") nil force-same-window)))
```

There are 2 very interesting things to notice.  
First, when the function is invoked from Lisp, provided
with the needed parameters, the `interactive` part is not invoked.
This makes sense, otherwise nested interactive questions would be very
unconvenient. So, a command can be invoked as a function.

Second, and more imporant for our case: you probably noticed that
`interactive` in `switch-to-buffer` is not getting a multiline string,
as we described before, but a list.  
Indeed, this is another, and more powerful, way to define interactive
aguments. Let's explore this.

## Passing `interactive` a list
Read the following and try to guess which values will be used as arguments::

```emacs-lisp
(defun a-2-parameter-command (name second-name)
  (interactive (list "John" "Doe"))
  (message (format "Hi, %s %s!" name second-name)))
```

Yes: the 2 interactive arguments are automatically input by Emacs when
the command is invoked interactively. `name` and `second-name` are
defined as interactive paramaters, but then they are never asked to
the user.  
How can this be possibly useful?

The fact is, the list can be the result of another, arbitrary
function, including an interactive command. Enter a new command:
`completing-read`. Try the following:

```emacs-lisp
(completing-read
    "Your generation: "
    '("baby-boomer" "x" "millennial" "z" "alpha") nil )
```

As you see, the first argument is the prompt, the second is the list
of items to interactively choose from. The value of `completing-read`
is the selected item.

What if we wanted to *display* a string but *return* an associated
value for the selected item? One could think to have a list of key /
value pairs such as:

```emacs-lisp
(completing-read
 "Your generation: "
 '(("Baby Boomers" "boomer")
   ("Generation X" "x")
   ("Millennials / Generation Y" "y")
   ("Zoomers / Generation Z" "zoomer")
   ("Generation Alpha" "hey, alpha!")))
```

Unfortunately, this still returns the key, not the value.
Here's a modified version doing the trick:

```emacs-lisp
(let ((choices '(("Baby Boomers" . "boomer")
		 ("Generation X" . "x")
		 ("Millennials / Generation Y" . "y")
		 ("Zoomers / Generation Z" . "zoomer")
		 ("Generation Alpha" . "hey, alpha!"))))
  (alist-get 
    (completing-read "Your generation: " choices )
    choices nil nil 'equal))
```

See more about this approach in [How to return the value instead of key by completing-read][completing-read-stackoverflow].

## Putting completing-read and interactive together
Given what we said, the following should not be hard to understand:

```emacs-lisp
(defun ask-generation ()
  (let ((choices '(("Baby Boomers" . "boomer")
                   ("Generation X" . "x")
                   ("Millennials / Generation Y" . "y")
                   ("Zoomers / Generation Z" . "zoomer")
                   ("Generation Alpha" . "hey, alpha!"))))
    (alist-get 
     (completing-read "Your generation: " choices )
     choices nil nil 'equal)))

(defun insert-generation (generation)
  (interactive (list (ask-generation)))
  (insert generation))
```

# Data clamp
We could do the same with our:

```emacs-lisp
(defun surround-region-with-hard-coded-strings ()
  "Surround the active region with hard-coded strings"
  (interactive)
  (surround-region--surround "<<<" ">>>"))
```

but first I would like to put the opening and the closing delimiters
together, in the same data structure: after all, they are always
operated in a pair. Passing them separately, when they infact are
always operated in a pair, is what in OOP is known as a [Data
Clump][data-clump].

```emacs-lisp
(defun surround-region--surround (delimiters)
  "Surround the active region with hard-coded strings"
  (when (region-active-p)
    (save-excursion
      (let ((beginning (region-beginning))
            (end (region-end))
            (opening-delimiter (car delimiters))
            (closing-delimiter (cdr delimiters)))

        (goto-char beginning)
        (insert opening-delimiter)

        (goto-char (+ end (length closing-delimiter)))
	(insert closing-delimiter)))))


(defun surround-region-with-hard-coded-strings ()
  "Surround the active region with hard-coded strings"
  (interactive)
  (surround-region--surround '("<<<" . ">>>")))
```

# Putting all together
Here we go, we are ready to modify
`surround-region-with-hard-coded-strings` so that it interactively asks for
delimiters:

```emacs-lisp

(defun surround-region--surround (delimiters)
  "Surround the active region with hard-coded strings"
  (when (region-active-p)
    (save-excursion
      (let ((beginning (region-beginning))
            (end (region-end))
            (opening-delimiter (car delimiters))
            (closing-delimiter (cdr delimiters)))

        (goto-char beginning)
        (insert opening-delimiter)

        (goto-char (+ end (length closing-delimiter)))
	(insert closing-delimiter)))))


(defun surround-region--ask-delimiter ()
  (let ((choices '(("<<< and >>>" . ("<<<" . ">>>"))
                   ("double quotes: \"\"" . ("\"" . "\""))
                   ("markdown source block: ```emacs-lisp" . ("```emacs-lisp" . "```"))
                   ("comment: *\ /*" . ("/*" . "*/")))))
    (alist-get 
     (completing-read "Your generation: " choices )
     choices nil nil 'equal)))

(defun surround-region-with-hard-coded-strings (delimiters)
  "Surround the active region with hard-coded strings"
  (interactive (list (surround-region--ask-delimiter)))
  (surround-region--surround delimiters))
```

# What's next?
There is so much more to do:

* The list could propose the last used choice by default.
* Or even better, we could like to have a history and to persist it.
* The suggested delimiters could be context dependent (e.g., it does not make sense to suggest a markdown delimiter when working in Python).
* Do we really need to develop this from the scratch? Of course no!
  There are off-the-shelf packages doing this (and much more). But
  learning is fun, isn't it?

So far so good. We will keep exploring this topic one of the next
days.  
Bye!

# References
* Emacs manual
  * [interactive][interactive]
  * [interactive codes][interactive-codes]
* [How to return the value instead of key by completing-read - StackOverflow][completing-read-stackoverflow]
* [Data Clump - Wikipedia][data-clump]

[interactive]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Using-Interactive.html
[interactive-codes]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Interactive-Codes.html
[completing-read-stackoverflow]: https://stackoverflow.com/questions/35390729/how-to-return-the-value-instead-of-key-by-completing-read
[data-clump]: https://en.wikipedia.org/wiki/Data_clump

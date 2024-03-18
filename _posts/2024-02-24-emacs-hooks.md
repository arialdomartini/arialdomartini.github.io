---
layout: post
title: "Emacs: how to activate the functionality X for all files of type Y?"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- Emacs
- Lisp
---
-   When a file of type Y is opened, Emacs sets a specific major mode.
-   Each major mode is equipped with a hook, a variable holding a list of functions.
-   After that major mode is activated, all the functions in its hook are run.
-   If you add a function to the mode's hook it will be run for that kind of file.
-   You can use \`add-hook\` for that.

Therefore, just use:

```emacs-lisp
(add-hook '<major-mode>-hook #'<function-you-wish-to-trigger>)
```

For example, to have line numbers in your Python files, use:

```emacs-lisp
(add-hook 'python-mode-hook #'display-line-numbers-mode)
```

<!--more-->


# Table of Contents

- [A sample use case](#a-sample-use-case)
- [Solution](#solution)
- [Major modes](#major-modes)
- [Hooks](#hooks)
- [Adding a function in a hook](#adding-a-function-in-a-hook)
- [Removing functions from hooks](#removing-functions-from-hooks)
- [More on modes](#more-on-modes)
- [Conflicts](#conflicts)
- [Anonymous functions](#anonymous-functions)
- [Associating major modes to file types](#associating-major-modes-to-file-types)
- [References](#references)

<!-- markdown-toc end -->

## Note
This post is based on the content of the lessons I took from
 [Protesilaos Stavrou][prot]
 and it includes his comments.

# A sample use case

There are 3 functionalities that I like to have enabled for specific kinds of files:


| Minor mode                                       | Feature                                                         | Where it makes sense to me |
|--------------------------------------------------|-----------------------------------------------------------------|----------------------------|
| [olivetti-mode][olivetti-mode]                   | Enhances the appearance of prose documents                      | Markdown, Org              |
| [toggle-truncate-lines][toggle-truncate-lines]   | When it is on, long lines are not wrapped at the window's edge | All files                  |
| [aggressive-indent-mode][aggressive-indent-mode] | Format Lisp code in real time as you type                       | Emacs Lisp files           |

Your mileage may be different.

In this post we will find out how to istruct Emacs to enable a
specific feature when specific kinds of files are opened.


# Solution

```emacs-lisp
(add-hook 'org-mode-hook #'olivetti-mode)

(add-hook 'markdown-mode-hook #'olivetti-mode)

(defun turn-on-toggle-truncate-line ()
  "Turns truncating on, without printing messages"
  (let ((inhibit-message t))
    (toggle-truncate-lines 1)))
(add-hook 'log4j-mode-hooks #'turn-on-toggle-truncate-line)

(add-hook 'emacs-lisp-mode-hooks #'aggressive-indent-mode)
```

# Major modes

When files are opened, Emacs enables a specific major mode.

A major mode is a function that configures Emacs to provide specific
functionalities. It sets up keybindings, indentation rules, syntax
highlighting and other features tailored to that specific file type.
Typically, different kinds of files require different sets of
features.

You can verify that a major mode is indeed a function by running:

    M-x describe-function lisp-mode

Type `s` to inspect the source code, or jump to the Source Code section if you use the [helpful](https://github.com/Wilfred/helpful) package.

Major modes have a notion of inheritance. This means that when a major
mode is activated, it is run together with all its the parent mode functions.

Some modes such as `fundamental-mode` and `text-mode` are top-level
parents - and therefore the least specialized ones. `fundamental-mode`  is defined with `defun`:

```emacs-lisp
  (defun fundamental-mode ()
  "Major mode not specialized for anything in particular.
Other major modes are defined by comparison with this one."
  (interactive)
  (kill-all-local-variables)
  (run-mode-hooks))
```

Modes that inherit from other modes, such as `emacs-lisp-mode`, are
defined with the `define-derived-mode` macro:

```emacs-lisp
(define-derived-mode emacs-lisp-mode lisp-data-mode
  ...
````

`text-mode`, that is a top-level mode, is also declared with `define-derived-mode`, indicating `nil` as its parent;

```emacs-lisp
(define-derived-mode text-mode nil "Text"
"Major mode for editing text written for humans to read.
```

Indeed, most of the top-level major modes are created with
`define-derived-mode` by passing a nil `PARENT` argument. The fact
that `fundamental-mode` is declared instead as a `defun`, is likely a
legacy rather than the current convention.

The advantage of `define-derived-mode` over `defun` is that it
automatically declares the corresponding hook, the key map, the abbrev
table etc. Perhaps, then the macro `define-derived-mode` is a
misnomer. It should be `defmode` or something.

It's easy to follow the inheritance line up to the first parent with
`xref-find-definitions (M-.)`. For example, if you display the source
code of `emacs-lisp-mode` with `describe-function`:

```emacs-lisp
C-h f emacs-lisp-mode RET
```

and then you display its source code, you can see it derives from `lisp-data-mode`:


```emacs-lisp
(define-derived-mode emacs-lisp-mode lisp-data-mode
...
```

Move the point over `lisp-data-mode` and hit `M-.` to jump to its
definition:


```emacs-lisp
(define-derived-mode lisp-data-mode prog-mode "Lisp-Data"
...
```

So, `lisp-data-mode` inherits from `prog-mode`. Do the same for
`prog-mode`:

```emacs-lisp
(define-derived-mode prog-mode fundamental-mode "Prog"
...
```

and finally for `fundamental-mode`:

```emacs-lisp
(defun fundamental-mode ()
  "Major mode not specialized for anything in particular.
Other major modes are defined by comparison with this one."
  (interactive)
  (kill-all-local-variables)
  (run-mode-hooks))
```

We reached the top-most mode.  
The inheritance line we just found out is:

```
    fundamental-mode 
          |
      prog-mode
          |
    lisp-data-mode
          |
    emacs-lisp-mode
```

The source code of `fundamental-mode` is very interesting: the last
command is `(run-mode-hooks)`, which leads us to the to the concept
of hooks.


# Hooks

Each major mode is equipped with a hook, a variable containing a list
of functions. 

All major mode hooks are parameterless, and they are called "normal".
There are also "abnormal" hooks (conventially named with a `-functions` suffix) such as `enable-theme-functions` and `after-load-functions`, which take parameters.

As you can foresee from the source code of `fundamental-mode`, after
the major mode has been activated, the functions in the hook are
triggered.

The same happens for modes inheriting from a parent major mode. We saw
before that those modes are defined via the macro
`define-derived-mode`. The source of `define-derived-mode` may not be
straightforward to undestand, but it is easy to see that its last
operation is to run the hook functions:

```emacs-lisp
;; Run the hooks (and delayed-after-hook-functions), if any.
(run-mode-hooks ',hook)))))
```


By convention, each major mode defines a hook whose name is the mode
name followed by `-hook`. For example, the hook for `emacs-lisp-mode`
is `emacs-lisp-mode-hook`.

You can display the value of a hook with `M-x describe-variable RET <hook name>`.

For example, in my case `prog-mode-hook` contains:

```emacs-lisp
prog-mode-hook
```

Note the the hook contains `rainbow-mode`: this makes sense, because we
know that modes are just functions. This is in fact the idiomatic way
to associate minor modes to a major mode.


# Adding a function in a hook

So, let's go back to my original goal of adding the beautiful [aggressive-indent-mode](https://github.com/Malabarba/aggressive-indent-mode) to `emacs-lisp-mode`:

The current value of:

```emacs-lisp
M-x describe-variable RET emacs-lisp-mode-hook RET
```

returns:

```emacs-lisp
(ert--activate-font-lock-keywords erefactor-lazy-highlight-turn-on)
```

While it is possible to add `aggressive-indent-mode` with:

```emacs-lisp
(setq emacs-lisp-modehook '(ert--activate-font-lock-keywords aggressive-indent-mode))
```

this is not very convenient, as it forces to copy the already existing values.

One could think of using:

```emacs-lisp
(push #'aggressive-indent-mode emacs-lisp-mode-hook)
```

but this is also suboptimal, because `push` is not idempotent, so
running the code above twice would insert `aggressive-indent-mode`
multiple times.

The idiomatic way to customize a hook is via `add-hook`.

```emacs-lisp
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
```

`add-hook` is an amazingly complex function, but what matters here is
that it makes sure that it adds a function to the hook variable
ensuring that it is not added again if already present.

Of course, you can also add custom functions to a mode. For example:

```emacs-lisp
(defun my-say-hello ()
  (interactive)
  (message "Enjoy your programming session!"))
    
(add-hook 'prog-mode-hook #'my-say-hello)
```

Open any `.el` file and verify that the greeting is displayed in the echo area.


# Removing functions from hooks

As you could imagine, the inverse of `add-hook` is `remove-hook`:

```emacs-lisp
(remove-hook 'prog-mode-hook #'my-say-hello)
```

# More on modes

Some major modes are mostly used as parent text-mode prog-mode
special-mode, they can also be used directly prog-mode by its own does
nothing.

It should not come as a surprise that minor modes too have hooks. When
a file is opened, its major mode is run, together with all its
parent major modes. Minor modes too are run, but typically they don't inherit from eachother.

This leads us to a question: what does happen if a function is defined
in multiple hooks? Let's find this out.


# Conflicts

We know that `emacs-lisp-mode`  inherits from `prog-mode`. So, let's
add `my-say-hello` to both their hooks:

```emacs-lisp
(add-hook 'emacs-lisp-mode-hook #'my-say-hello)
(add-hook 'prog-mode-hook #'my-say-hello)
```

Let's display the view echo ares messages buffer with:

```emacs-lisp
M-x view-echo-area-messages RET
```

and finally let's open a random `.el` file.

Oh, no! There is a disappointing:

```emacs-lisp
Enjoy your programming session! [2 times]
```

This gets us to a general recommendation:

-   either make sure that the inheritance graph of modes avoids
    duplications.
-   or make sure that hooks invoke idempotent functions. Luckily, this is the default for the vast majority of minor modes.

In the case of `aggressive-indent-mode`, its author has been so
diligent to make it idempotent. Indeed, its documentation (`C-h f aggressive-indent-mode RET`) states:

```emacs-lisp
If called from Lisp, toggle the mode if ARG is ‘toggle’.  Enable
the mode if ARG is nil, omitted, or is a positive number.
```
You can easily verify that `toggle-truncate-lines` behaves
differently. If you set it up twice:

```emacs-lisp
(add-hook 'emacs-lisp-mode-hook #'toggle-truncate-lines)
(add-hook 'prog-mode-hook #'toggle-truncate-lines)
```

opening a file will result in having it off.

It's better to have the following:

```emacs-lisp
(remove-hook 'emacs-lisp-mode-hook #'toggle-truncate-lines)
(remove-hook 'prog-mode-hook #'toggle-truncate-lines)


(defun turn-on-toggle-truncate-line ()
  "Turns truncating on, without printing messages"
  (let ((inhibit-message t))
    (toggle-truncate-lines 1)))

(add-hook 'emacs-lisp-mode-hook #turn-on-toggle-truncate-line)
(add-hook 'prog-mode-hook #turn-on-toggle-truncate-line)
```

Of course, it is never a good idea to intentionally add the same
function to multiple hooks in the same hierarchy. In this case, I
prefer to have:

```emacs-lisp
(add-hook 'fundamental-mode-hook #turn-on-toggle-truncate-line)
```

# Anonymous functions

It is also possible to use anonymous functions:

```emacs-lisp
(add-hook 'mark-down-mode-hook (lambda () (auto-fill-mode -1)))
```

Anonymous functions are fine, but you have to remove them as a whole
if you want to modify them, whereas adding a symbol (a function's
name) gives you an indirection: you can change the function without
updating the hook.


# Associating major modes to file types

This leads us to a final question: what determines the major mode for
a file or a functionality like `magit` or `dired`?

For files, it all revolves aroung the variable `auto-mode-alist`.

If you inspect its value (`C-h v auto-mode-alist RET`) you will find a
very large value, containing items such as:

```emacs-lisp
: (
...
(\.fs[iylx]?\' . fsharp-mode)
(\.hsc\' . haskell-mode)
(\.cabal\'\|/cabal\.project\|/\.cabal/config\' . haskell-cabal-mode)
(\.py[iw]?\' . python-mode)
(\.cs\' . csharp-mode)
(\.java\' . java-mode)
...
(\.org\' . org-mode)
(\.tgz\' . tar-mode))
```

So, it is an alist of file name patterns vs corresponding major mode
functions. Visiting a file whose name matches the regular expression
will run the corresponding  mode function.

Modes are also run when a buffer is not visiting a file. That's the
case, for example, of `magit` and `dired`. You will usually find this
as just a function call with the name of the major mode.

For those cases, mode functions are explicitly run from the code. This
is for example a snippet from `dired-internal-noselect`:

```emacs-lisp
(if mode (funcall mode)
  (dired-mode dir-or-list switches))
```

# References

- [Protesilaos Stavrou][prot]
- [aggressive-indent-mode][aggressive-indent-mode]
- [olivetti-mode][olivetti-mode]
- [helpful][helpful]

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/29)


[aggressive-indent-mode]: https://github.com/Malabarba/aggressive-indent-mode
[olivetti-mode]: https://github.com/rnkn/olivetti
[helpful]: https://github.com/Wilfred/helpful
[prot]: https://protesilaos.com/coach/
[toggle-truncate-lines]: https://emacsdocs.org/docs/emacs/Line-Truncation

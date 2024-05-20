---
layout: post
title: "Emacs as a F# IDE - Setup"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- emacs
- lisp
- fsharp
---
So you want to code in F# with Emacs?  
I can relate, I also love both. Oh dear, if I love them!

So, presto! Let's make Emacs your next F# IDE!  <!--more-->
# TD;DR
* Install [Eglot][eglot], [fsharp-mode][fsharp-mode-melpa] and [eglot-fsharp][eglot-fsharp-melpa].
* That's it. It is really that simple.

## Summary
You need 4 components:

| Package                                | Purpose                                                                                        |
|----------------------------------------|------------------------------------------------------------------------------------------------|
| [fsautocomplete][fsautocomplete-nuget] | The F# LSP Server                                                                              |
| [Eglot][eglot]                         | The Language Server Protocol client                                                            |
| [eglot-fsharp][eglot-fsharp-melpa]     | Integrates Eglot with `fsharp-mode`.<br/>It provides completion, syntax checking and the like. |
| [fsharp-mode][fsharp-mode]             | The major mode responsible for syntax highlighting and indentation                             |

Optionally, you might also want to have:

| Package           | Purpose                               |
|-------------------|---------------------------------------|
| [corfu.el][corfu] | It provides a pop-up for IntelliSense |

# Steps
## 1. Install Eglot

```emacs-lisp
(use-package eglot
  :ensure t)
```

[M-x Eglot][eglot], *E*macs Poly*glot* &mdash; or just Eglot &mdash;
is the official Language Server Protocol client for Emacs. It is built-in since version 29.  
It is not the only LSP client available. You might prefer using
[lsp-mode][lsp-mode]. This post, though, covers Eglot only. If you are
interested in an lsp-mode version, drop me a message, I will find the
time to extend the post.

### Notes

An LSP client is that piece of software that communicates with the
underlying Language Server to provide features like auto-completion
("IntelliSense" in the Microsoft lingo), go-to-definition,
find-references, and the like.

As a client, Eglot is server agnostic, but you will need a specific
package for glueing it with `fsharp-mode`. The integration is provided
by [eglot-fsharp][eglot-fsharp-melpa], a separate package. We will see this in the next
step.

As you have imagined, LSP is based on the client-server
architecture. Therefore, an LSP client needs a corresponding running
server. As a matter of fact, a server covers one single language, so
you will need an LSP server for F#, one for Haskell and so on.  
The LSP server for F# is called [FsAutoComplete][fsautocomplete],
which is part of the [Ionide][ionide] tool family.

You can install it via `dotnet tool` or let `eglot-fsharp` do this for
you. I will cover both approaches.

## 2. Install fsharp-mode
```emacs-lisp
(use-package fsharp-mode
  :defer t
  :ensure t)
```
`:defer t` enhances the startup speed by delaying the loading of the package until it is actually needed.
 
`:ensure t` conveniently downloads the package from the internet.

Once `fsharp-mode` is installed, you should see F# files properly
syntax-highlighted. Indentation will also work.

## 3. Install eglot-fsharp:
Let's connect Eglot with `fsharp-mode`:

```emacs-lisp
(use-package eglot-fsharp
  :ensure t
  :after fsharp-mode
  :config
  (add-hook 'fsharp-mode-hook #'eglot-ensure))
```

The hook makes sure that when `fsharp-mode` is activated, Eglot is
also loaded.


## 4. Let eglot-fsharp install fsautocomplete
Now, you just need to install the F# Language Server.  
There are 2 ways to do this:

1. Use `dotnet`.
2. Let `eglot-fsharp` perform the installation.

For the latter:

* Execute `M-x eglot`.
* When asked, select `fsharp-mode`.
* Wait for `eglot-fsharp` to download `fsautocomplete`.

`fsautocomplete` will be downloaded and saved in `~/.config/emacs/FsAutoComplete/netcore`.

### Notes
Although this is the standard procedure, I'm not super happy with it
and I prefer a different approach. Read about it in [fsautocomplete
installed via dotnet](#fsautocomplete-installed-via-dotnet).


## 5. Enable the IntelliSense pop-up with corfu.el

```emacs-lisp
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :config
  (setq corfu-min-width 250
        corfu-min-height 750
        corfu-count 20
        corfu-auto t
        corfu-cycle t
        corfu-separator ?\s
        corfu-preview-current "insert"
        corfu-scroll-margin 25
        ;; enable corfu on TAB
        tab-always-indent 'complete
        ;; shows documentation after `corfu-popupinfo-delay'
        corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1)

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)) )
```

## 6. Optionally, make corfu.el beautiful with nerd-icons

```emacs-lisp
;; Icons
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-marginalia-setup)
  (nerd-icons-completion-mode 1))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
```


# Alternative: install fsautocomplete via dotnet
What I don't like of `eglot-fsharp` installing `fsautocomplete` is the
following:

* It requires to manually run `eglot` the first time.
* `fsautocomplete` will not be available from the terminal.
* Installing `fsautocomplete` with `dotnet` would download a second
  copy.
  
Therefore, I would rather install `fsautocomplete` the way [the
official NuGet page][fsautocomplete-nuget] recommends to:

```bash
dotnet tool install --global fsautocomplete
```

This stores `fsautocomplete.exe` in `.dotnet/tools`, which is nothing
specific to Emacs.

Now, it's just a matter letting `eglot-fsharp` know that it needn't
download its own copy of `fsautocomplete`. This is simply done by
setting the variable `eglot-fsharp-server-install-dir` to `nil`:

```emacs-lisp
(use-package eglot-fsharp
  :ensure t
  :after fsharp-mode
  :config
  (setq eglot-fsharp-server-install-dir nil))
```

That should make the trick.

# Now what?
Now profit!  
I will cover which extra functionalities are provided by these
packages in one of the next posts. I also have to learn!  
Stay tuned! Happy coding.


# References
* [fsharp-mode][fsharp-mode]
  * [fsharp-mode-melpa][fsharp-mode-melpa]
* [eglot][eglot]
* [][eglot-fsharp-melpa]
* [lsp-mode][lsp-mode]
* [corfu.el][corfu]
* [nerd-icons.el][nerd-icons]
* [FsAutoComplete][fsautocomplete]
  * [FsAutoComplete on nuget.org][fsautocomplete-nuget]
* [Ionide][ionide]
* [Protesilaos Stavrou][prot]


[prot]: https://protesilaos.com/coach/
[fsharp-mode]: https://github.com/fsharp/emacs-fsharp-mode
[eglot]: https://github.com/joaotavora/eglot
[eglot-fsharp-melpa]: https://melpa.org/#/eglot-fsharp
[fsharp-mode-melpa]: https://melpa.org/#/fsharp-mode
[lsp-mode]: https://emacs-lsp.github.io/lsp-mode/
[corfu]: https://github.com/minad/corfu
[nerd-icons]: https://github.com/rainstormstudio/nerd-icons.el
[fsautocomplete]: https://github.com/ionide/fsautocomplete
[fsautocomplete-nuget]: https://www.nuget.org/packages/fsautocomplete
[ionide]: https://github.com/ionide

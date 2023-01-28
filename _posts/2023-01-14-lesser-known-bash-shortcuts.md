---
layout: post
title: "Lesser Known Bash shortcuts"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- bash
- zsh
---
**TL;DR**




# Undo
`C-/ (undo)`


Bash has got an undo feature. While not as powerful as those in Emacs and Vim (e.g. it can only revert actions, there is no `redo` command), it is still handy. Curiously, there are 3 keybindings to activate it: `C-/`, `C-_` or `C-x C-u`. On the US layout, `C-/` is the most convenient, as it requires only 2 keys. 


Plot:
1. yay --sync --refresh --sysupgrade
2. replace with -Syu
3. undo


## Reset
If you mess up with a line retrieved from the command history, you can revert it back to its original value with `M-r`.
* `M-r (revert-line)`

Plot:
1. Recover line `git clone --depth 1 --single-branch https://github.com/doomemacs/doomemacs ~/.config/emacs && ~/.config/emacs/bin/doom install`
2. replace URL with git@github.com:neovim/neovim.git
3. remove depyth and singlebranch
4. add uninstall emacs
5. `M-r`


# Macro
* `C-x (`
* `C-x )`
* `C-x e`

plot 
1. ls
2. `\$(!!)`` expand
3. tar -cfvz
4. repeat



# Edit line with editor
Bash's macros are cool, but they leave a little to be desired. For example, they cannot capture [character-search](#search).

How cool would it be if Bash was as powerful as Emacs or Vim? 
Well, if Bash will not go to the mountain, the mountain must come to Bash.

Press `C-x C-e` and quickly open the current line in your preferred editor.

* `C-x C-e`

plot:

start with VISUAL=nvim

1. `git clone --depth 1 --single-branch https://github.com/doomemacs/doomemacs ~/.config/emacs && ~/.config/emacs/bin/doom install`
2. Edit
3. save

explain that the editor is defined with VISUAL
1. visual=nvim (reference the trick with the negative argument)
3. make it uppercase
2. C-x C-e
3. 



# Search
Move around faster, searching back and forth for the occurrences of a specified character on the current line.

`C-]` `M-C-]`



# Universal argument
* `M-<digit>`
You can repeat any command prefixing it with Alt followed by the desidered multiplication factor.



## Negative argument
* `M--` to invert command

You might wonder what is the effect of prefixing a command with a negative number. In many cases, it inverts the result.

* if `M-d` deletes the *next* word, `M-- M-d` deletes the *previous* one;

* `C-k` deletes from cursor to *end* of line; `M-- C-k` deletes from cursor to *beginning* of line (not so handy, though: the shorter `C-u` does the same)

* you can use `C-r` to search for a string on the *left* of cursor; `M-- C-r` searches on the *right*

I often use `M-- M-u` to upcase the word I just typed.

Plot:
* see the examples above


# Instant Comment
`M-#`
One of my preferred Zsh's shortcuts is `ESC q`. How many times have I written a longhty command line, only to discover I could not run it, because I was in the wrong directory, or I forgot to run some other command beforehand? `ESC q` comes to rescue, providing an instant new empty line, and restoring the suspended one right after its execution.<br/>
`M-# (insert-comment)` is not exactly the same, but it can be used in similar situations.


Video
show in Zsh and then in Bash

```bash
$ mv atom-heart-mother-cow-1024x768.jpg summer68/
$ cd music/pinkfloyd
# mkdir summer68 (C-r)
$ mv atom-heart-mother-cow-1024x768.jpg summer68/
```

## In Zsh
`Esc q`


# Clipboard
Bash's support for the clipboard is quirky, but for some aspects it packs a punch in terms of power.

* `C-space` (set-mark) to select
* `C-x C-x` to select the other marker (and to make selection visible)
* `C-x C-x` how does it work?


`C-y` to paste ("yank" in Emacs lingo)
`M-w` to copy (curiously: "yank", in Vim lingo)
`C-w` to cut

* `M-.` or `M-_`, yank last argument

* Video (C-k / C-u + C-_)
`ln -s destination source` cut, paste in right order


## Paste last argument
`M-.`
`M-_`

## Paste n-th argunent
* `M-n C-M-y` (yank n-th argument)


* video invert order



## Bash's hidden Multiclipboard
`M-y` instead of `C-y`

## Expand
* `M-C-e`

`!-1 M-C-e` for re-inserting previous command

show with $PATH, $HOME, $RANDOM
$(!!) instead of copy pasting the result

!* all arguments
!^ first argument
!$ first argument
!# duplicate line


# Text Completion
## (History-based) Intellisense
* `M-/`

## Showing Completions
* `M-?``, like Tab, shows possible completions without trying to complete

## Insert All Completions 
* `M-*` insert all the possible completions (cool!)


# Deleting
* `C-k`
* `C-u` or `C-x backspace`, complementary to `C-k`
* `M-d`
* `C-w` cut previous word (from Vim) (C-y to paste), complementary to M-d

* video

# Transforming words
## Swapping
* `C-t`
* `M-t`
`

## Changing the case
* `M-u` Upper case
* `M-l` Lower case
* `M-c` Capital letter

## Suspending a stream
* `C-s` suspend
* `C-q` unsuspend

example with a `tail -f`


# A faster clear
`C-l`

## A faster and more aggressive clear
`C-M-l`
# History
* `M-<`` and `M->`` (Alt-Shift , .) beginning and end of history


# Inline search
* C-r search in current line! (move to exit) (only backword)



### Literal chars
* `C-v` for literal chars (i.e. Return, what's the use?)
for example for inserting a tab


## Return 
* `C-j`
## Aborting line
* `C-c``
# Quitting
`C-d`




# Lesser unknown
https://readline.kablamo.org/emacs.html


# sistemare
`~/.inputrc`

## readline
https://www.man7.org/linux/man-pages/man3/readline.3.html
This is the library that handles reading input when using an interactive shell, unless the --noediting option is given at shell invocation


## Unclear
### Execute commands
* C-p/C-l C-o (execute command, repeat) boh!






## Notes
Everywhere, `Esc` can be used instead of `Alt`.



# Summary

| Shortcut | Command               | Function                          |
|----------|-----------------------|-----------------------------------|
| `M-u`    | `upcase-word`         | Convert next word to UPPERCASE    |
| `C-k`    | `kill-line`           | Cut from cursor to end of line    |
| `C-l`    | `recenter-top-bottom` | Clear screen                      |
| `C-M-l`  |                       | Clear screen and off-screen lines |
| `C-y`    | `yank`                | Paste                             |
| `C-w`    | `kill-region`         | Cut                               |
| `M-w`    | `kill-ring-save`      | Copy                              |
| `M-y`    | `yank-pop`            | Cycle and paste                   |
| `C-a`|||
| `C-e`|||
| `M-d`|||
| `C-f`|||
| `M-f`|||
| `C-b`|||
| `M-b`|||
| `C-r`|||
| `C-d`|||
| `C-u`|||

---
layout: post
title: "Lesser Known Bash shortcuts"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- bash
- zsh
---
**TL;DR**


<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Undo](#undo)
- [Reset](#reset)
- [Macro](#macro)
- [Edit line with editor](#edit-line-with-editor)
- [Character Inline Search](#character-inline-search)
- [String Inline Search](#string-inline-search)
- [Expand](#expand)
- [(History-based) Intellisense](#history-based-intellisense)
- [Insert All Completions](#insert-all-completions)
- [Universal argument](#universal-argument)
- [Negative argument](#negative-argument)
- [Instant Comment](#instant-comment)
- [Clipboard](#clipboard)
- [Special paste options](#special-paste-options)
- [Paste last argument](#paste-last-argument)
- [Paste n-th argunent](#paste-n-th-argunent)
- [Bash's hidden Multiclipboard](#bashs-hidden-multiclipboard)
- [Deleting](#deleting)
- [Transforming words](#transforming-words)
- [Swapping](#swapping)
- [Changing the case](#changing-the-case)
- [Suspending a stream](#suspending-a-stream)
- [A faster clear](#a-faster-clear)
- [A faster and more aggressive clear](#a-faster-and-more-aggressive-clear)
- [History](#history)
- [Literal chars](#literal-chars)
- [Return](#return)
- [Aborting line](#aborting-line)
- [Quitting](#quitting)
- [Lesser unknown](#lesser-unknown)
- [sistemare](#sistemare)
- [readline](#readline)
- [Unclear](#unclear)
- [Execute commands](#execute-commands)
- [Notes](#notes)
- [Summary](#summary)

<!-- markdown-toc end -->


# Undo

Bash has got an undo feature. While not as powerful as those in Emacs and Vim (e.g. it can only revert actions, there is no `redo` command), it is still handy. Curiously, there are 3 keybindings to activate it: `C-/`, `C-_` or `C-x C-u`. On the US layout, `C-/` is the most convenient, as it requires only 2 keys. 

 <video width="100%" controls>
    <source 
        src="/static/img/lesser-known/undo.mp4" 
        type="video/mp4" />Your browser does not support the video tag />
</video>

Notes:
* speed up the undo part
* maybe add a reference to the lack of redo


## Revert line
`M-r (revert-line)`

There is a special version of undo called `revert-line`. It comes in handy all the times you mess up with a line retrieved from the command history, and you wish to revert it back to its original value.

<video width="100%" controls>
    <source 
        src="/static/img/lesser-known/revert-line.mp4" 
        type="video/mp4" />Your browser does not support the video tag />
</video>


# Macro
* `C-x ( (start-kbd-macro)` 
* `C-x ) (end-kbd-macro)`
* `C-x e (call-last-kbd-macro)`

Macros are just the coolest, right? If you're already an Emacs or Vim power user, you know what I mean. If you're on VS Code, well, it's time to branch out and give those editors a try: you won't regret it.

In the meanwhile, you might be surprised to know that Bash does support macros. Not perfectly, but decently.


<video width="100%" controls>
    <source 
        src="/static/img/lesser-known/macros.mp4" 
        type="video/mp4" />Your browser does not support the video tag />
</video>


# Edit line with editor
Bash's macros are cool, but they leave a little to be desired. For example, they cannot capture [character-search](#search).

How cool would it be if Bash was as powerful as Emacs or Vim? 
Well, if Bash will not go to the mountain, the mountain must come to Bash.

Press `C-x C-e` and quickly open the current line in ~~Emacs~~ your preferred editor.

* `C-x C-e` (edit-and-execute-command)

plot:

start with VISUAL=nvim

1. `git clone --depth 1 --single-branch https://github.com/doomemacs/doomemacs ~/.config/emacs && ~/.config/emacs/bin/doom install`
2. Edit
3. save

Explain that the editor is defined with VISUAL
1. visual=nvim (reference the trick with the negative argument)
3. make it uppercase
2. C-x C-e
3. 


$VISUAL, $EDITOR, and emacs



# Character Inline Search
* `C-] (character-search)`
* `M-C-] (character-search-backward)`

Move around faster, searching back and forth for the occurrences of a specified character on the current line.

Use `C-]` for searching forward, add Alt for inverting the direction.

If you are a Vim or an Emacs user, you might wonder how you could live in Bash without this functionality since now.


# String Inline Search
* `C-r` search in current line! (move to exit) (only backword)
* `M-- C-r`

`C-r` is widely known for searching the command history, but few know that the same shortcut, when used in a not empty line, searches in that line. `C-r` searches backword. Of course, prefixing it with a [Negative Argument](#negative-argument) would switch to forward search.

Notice: conclude your search with `C-j`: hitting `Return` will also execute the line, which is not necessarily your desidered outcome.


# Expand
* `M-C-e`

This is a surprisignly unknown, pretty neat feature: it takes whatever before the cursor, evaluates it and insert the result inline. Indulge with your fantasy and you will appreciate how powerful it is<br/>

You can expand variables, such `$PATH` and `$HOME`. Try with `$RANDOM`.

Some sleek ideas how to use it is with the following:

| Write   | Result                                                                                          |
|---------|-------------------------------------------------------------------------------------------------|
| `$(!!)` | This inserts the result of the previous command in the current line.<br/>Use the mouse no more. |
| `!-1`   | Inserts the previous command itself, even if the current line is not empty                      |
| `!*`    | All the arguments of the previous command                                                       |
| `!^`    | Only the first argument                                                                         |
| `!$`    | Only the last argument (Yes: `^` and `$` are reminescent of Vim)                                |
| `!#`    | Inserts a duplicate of the current line                                                         |

<br/>
Interestingly, it also works with commands. Try it with `$(ls)`.<br/>
I often use it with `$(grep whatever /some/place)`: it performs the search, and then it kindly delivers the result right in the line, ready for me to operate on. How cool.





<video width="100%" controls>
    <source 
        src="/static/img/lesser-known/expand.mp4" 
        type="video/mp4" />Your browser does not support the video tag />
</video>

plot:
* show that in Zsh a simple return expands

| `!^`    | Only the first argument                                                                         |
| `!$`    | Only the last argument (Yes: `^` and `$` are reminescent of Vim)                                |



# (History-based) Intellisense
* `M-/ (dabbrev-expand)`

Once this functionality is enabled adding

```
"\e/": dabbrev-expand
```

in `~/.inputrc`, it is like having a little magic auto-completion tool at your fingertips: it completes any partially input word, based on all the previously input text stored in the command history.

plot: 

## Insert All Completions 
* `M-*` insert all the possible completions (cool!)
* `M-?``, like Tab, shows possible completions without trying to complete



# Universal argument
* `M-<digit>`
You can repeat any command prefixing it with Alt followed by the desidered multiplication factor. This works for most of the commands.

demo

1. Repeat * 80 times
2. go to 12th chars
3. delete 3 words
4. ""something M-100 C-t


## Negative argument

* `M--` to invert command

You might wonder what happens prefixing a command with a negative number. In many cases, this would invert the result.

* if `M-d` deletes the *next* word, `M-- M-d` deletes the *previous* word;
* `C-k` deletes from cursor to
*end* of line; `M-- C-k` deletes from cursor to *beginning* of line (not so handy, though: the shorter `C-u` does the same)

* `C-r` searches for a string on the *left* of cursor; `M-- C-r` searches on the *right*

I often use `M-- M-u` to upcase the word I just typed.

Plot:
* see the examples above


# Instant Comment
`M-# (insert-comment)`
One of my preferred Zsh's shortcuts is `ESC q`. How many times have I written a longhty command line, only to discover I could not run it, because I was in the wrong directory, or because I forgot to run some other command beforehand? `ESC q` always came to rescue, providing an instant new empty line, and restoring the suspended one right after execution.<br/>

A demo is worth 100 descriptions:

Video
show in Zsh and then in Bash


Bash does not have `ESC q`. The related `M-# (insert-comment)` is not exactly the same, but it can be used in similar situations.


```bash
$ mv atom-heart-mother-cow-1024x768.jpg summer68/
$ cd music/pinkfloyd
# mkdir summer68 (C-r)
$ mv atom-heart-mother-cow-1024x768.jpg summer68/
```

# Clipboard
Bash's support for the clipboard packs a punch in terms of power. In many editors, the selection is defined holding down the Shift key and using the cursor keys. In Bash, just like in Vim and Emacs, you instead define the selection placing 2 handles to mark its beginning and end. The handles can be moved independently, and their position can be adjusted applying whatever movement command, including character and string search, not only using the cursor keys. The only oddity is, the selection is usually not visible.


* `C-space` (set-mark) to select
* `C-x C-x` to select the other marker (and to make selection visible)

`C-y` to paste ("yank" in Emacs lingo)
`M-w` to copy (curiously: "yank", in Vim lingo)
`C-w` to cut

* Video (C-k / C-u + C-_)
`ln -s destination source` cut, paste in right order


## Special paste options
### Paste last argument
`M-.`
`M-_`

### Paste n-th argunent
* `M-n C-M-y` (yank n-th argument)


* video invert order



## Bash's hidden Multiclipboard
`M-y` instead of `C-y`

The Bash clipboard is a multi-clipboard: it keeps a history of the copied texts enabling you to easily cycle it when it's time to paste.



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




### Literal chars
* `C-v` for literal chars (i.e. Return, what's the use?)
for example for inserting a tab


## Return 
* `C-j`
## Aborting line
* `C-c``
# Quitting
`C-d`





# Readline
All these funtionalities come from [GNU Readline][readline], a library created in the late 1980s by [[Brian Fox][brian-fox], the same author of Bash. Over the years, readline has become vastly used in a lot of command-line tools such as Tmux, MySQL and [Python][python-readline]. So, those keybindings are almost ubiquitous. Even the GUI text fields in macOS, such as the address bar in browsers, use a Readline-like library, `libedit`, which is based on the Readline's code. No surprises that some of the shortcuts above work everywhere in macOS.

Zsh, instead, does not depend on readline, and implemented a line-editing library from the scratch.

You can find the complete list of commands and features supported Readline in [readline(3)][readline-man].

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


# References
* [GNU Readline][readline]
* [Brian Fox][brian-fox]
* [Python readline][python-readline]

[readline]: https://tiswww.case.edu/php/chet/readline/rltop.html
[readline-man]: https://www.man7.org/linux/man-pages/man3/readline.3.html 
[python-readline]: https://docs.python.org/3/library/readline.html
[brian-fox]: https://en.wikipedia.org/wiki/Brian_Fox_%28computer_programmer%29

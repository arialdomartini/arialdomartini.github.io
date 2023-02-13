---
layout: post
title: "Lesser Known Bash shortcuts"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- bash
- zsh
---
Did you know Bash has got macros, a multi-clipboard, an undo feature, and a word completion based on history?
<!--more-->

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Expand](#expand)
  - [(History-based) Intellisense](#history-based-intellisense)
- [Undo](#undo)
  - [Revert line](#revert-line)
- [Macros](#macros)
<!-- - [Vim mode](#vim-mode) -->
- [Open line in editor](#open-line-in-editor)
- [Digit argument](#digit-argument)
  - [Negative argument](#negative-argument)
- [Character Inline Search](#character-inline-search)
- [String Inline Search](#string-inline-search)
- [Instant Comment](#instant-comment)
- [Clipboard](#clipboard)
  - [Special paste options](#special-paste-options)
  - [Paste last argument](#paste-last-argument)
  - [Paste n-th argunent](#paste-n-th-argunent)
- [Deleting](#deleting)
- [Transforming words](#transforming-words)
  - [Swapping chars and words](#swapping-chars-and-words)
  - [Changing the case](#changing-the-case)
- [Suspending a stream](#suspending-a-stream)
- [A faster clear](#a-faster-clear)
  - [A faster and more aggressive clear](#a-faster-and-more-aggressive-clear)
- [History](#history)
- [Literal chars](#literal-chars)
- [Return](#return)
- [Aborting line](#aborting-line)
- [Quitting](#quitting)
- [Readline](#readline)

<!-- - [Lesser unknown](#lesser-unknown) -->
<!-- - [Unclear](#unclear) -->
<!-- - [Execute commands](#execute-commands) -->
- [Summary](#summary)
- [References](#references)

<!-- markdown-toc end -->

### Legenda

| Symbol | Meaning |
|--------|---------|
| `C-`   | Control |
| `M-`   | Alt     |
| `S-`   | Shift   |

<br/>Everywhere, `Esc` can be used instead of `Alt`.


# Expand

| Shortcut | Command               | Description                           |
|----------|-----------------------|---------------------------------------|
| `M-C-e`  | `(shell-expand-line)` | Expand the line as the shell do would |

This is a surprisignly unknown, pretty neat feature: it takes whatever is in the line, it evaluates it and it finally inserts the result inline. 


<video width="100%" controls>
    <source 
        src="/static/img/lesser-known/expand.mp4" 
        type="video/mp4" />Your browser does not support the video tag />
</video>




You can expand variables such as `$PATH` and `$HOME` (try with `$RANDOM`).

Here are some sleek ideas for using it:

| Write   | Result                                                                                              |
|---------|-----------------------------------------------------------------------------------------------------|
| `$(!!)` | This inserts the output of the previous command in the current line.<br/>Never use the mouse again! |
| `!-1`   | Inserts the previous command itself, even if the current line is not empty                          |
| `!*`    | All the arguments of the previous command                                                           |
| `!^`    | Only the first argument                                                                             |
| `!$`    | Only the last argument (Yes: `^` and `$` are reminescent of Vim)                                    |
| `!#`    | Inserts a duplicate of the current line                                                             |


<br/>
Interestingly, it also works with commands. Try it with `$(ls)`.<br/>
I often use it with `$(grep whatever /some/place)`: it performs the search, and then it kindly delivers the result right in the line, ready for me to operate on. How cool.

Indulge with your fantasy and you will appreciate how powerful this little gem is<br/>

# (History-based) Intellisense

| Shortcut | Command            | Description                                                     |
|----------|--------------------|-----------------------------------------------------------------|
| `M-/`    | `(dabbrev-expand)` | Attempts to complete the text before point, based on past input |


Once this functionality is enabled adding

```
"\e/": dabbrev-expand
```

in `~/.inputrc`, it is like having a little magic auto-completion tool at your fingertips: it completes any partially input word, based on all the previously input text stored in the command history.

<video width="100%" controls>
    <source 
        src="/static/img/lesser-known/dabbrev-expand.mp4" 
        type="video/mp4" />Your browser does not support the video tag />
</video>

# Undo

| Shortcut | Command  | Description        |
|----------|----------|--------------------|
| `C-/`    | `(undo)` | Incrementally undo |

Bash has got an undo feature. While not as powerful as those in Emacs and Vim (e.g. it can only revert actions, there is no `redo` command), it is still handy. Curiously, there are 3 keybindings to activate it: `C-/`, `C-_` or `C-x C-u`. On the US layout, `C-/` is the most convenient, as it requires only 2 keys. 

<video width="100%" controls>
    <source 
        src="/static/img/lesser-known/undo.mp4" 
        type="video/mp4" />Your browser does not support the video tag />
</video>

## Revert line

| Shortcut | Command         | Description                                                    |
|----------|-----------------|----------------------------------------------------------------|
| `M-r`    | `(revert-line)` | Undo all the changes, reverting the line to its initial state. |

There is a special version of undo called `revert-line`. It comes in handy all the times you mess up with a line retrieved from the command history, and you wish to revert it back to its original value.

<video width="100%" controls>
    <source 
        src="/static/img/lesser-known/revert-line.mp4" 
        type="video/mp4" />Your browser does not support the video tag />
</video>


# Macros

| Shortcut | Command                 | Description             |
|----------|-------------------------|-------------------------|
| `C-x (`  | `(start-kbd-macro)`     | Start recording a macro |
| `C-x )`  | `(end-kbd-macro)`       | End the macro recording |
| `C-x e`  | `(call-last-kbd-macro)` | Execute the macro       |


Macros are just the coolest, right? If you're already into Emacs or Vim, you know what I mean. If you're on VS Code, well, it's time to branch out and give those editors a try: you won't regret it.

In the meanwhile, you might be surprised to know that Bash does support macros. Not perfectly, but decently.


<video width="100%" controls>
    <source 
        src="/static/img/lesser-known/macros.mp4" 
        type="video/mp4" />Your browser does not support the video tag />
</video>


<!-- # Vim mode -->

<!-- | Shortcut | Command             | Description                                       | -->
<!-- |----------|---------------------|---------------------------------------------------| -->
<!-- | `M-C-j`  | `(vi-editing-mode)` | Enable Vim modal editing                          | -->
<!-- | `C-x )`  | `(end-kbd-macro)`   | Switche back to the good, default Emacs' bindings | -->



# Open line in editor

| Shortcut  | Command                      | Description                            |
|-----------|------------------------------|----------------------------------------|
| `C-x C-e` | `(edit-and-execute-command)` | Edit the line in your editor of choice |

Bash's macros are cool, but they leave a little to be desired. For example, they cannot capture [character-search](#search). Also, Bash's Vim emulation is remarkable, but your NeoVim and Doom are another level.

Well, if Bash will not go to the mountain, the mountain must come to Bash.<br/>
Press `C-x C-e` and quickly open the current line in ~~Emacs~~ your preferred editor.

Define `VISUAL=<your-editor>` in `.bashrc` to select which editor to use


<video width="100%" controls>
    <source 
        src="/static/img/lesser-known/edit-execute.mp4" 
        type="video/mp4" />Your browser does not support the video tag />
</video>


# Digit argument

| Shortcut  | Command                      | Description                            |
|-----------|------------------------------|----------------------------------------|
| `M-<digit>` | `(digit-argument)` | Repeat the next command `n` times |

You can repeat any command prefixing it with `Alt`` followed by the desidered multiplication factor. This works for most the commands.

Actually, this is a way to pass a numeric argument to the subsequent command. In practice, almost all the commands interprets a numeric argument as the number times they should repeat.

<video width="100%" controls>
    <source 
        src="/static/img/lesser-known/universal-argument.mp4" 
        type="video/mp4" />Your browser does not support the video tag />
</video>


## Negative argument

| Shortcut | Command               | Description             |
|----------|-----------------------|-------------------------|
| `M--`    | `(negative-argument)` | Invert the next command |

You might wonder what happens providing a command with a negative argument. In many cases, this would invert the result.

* If `M-d` deletes the *next* word, `M-- M-d` deletes the *previous* word;
* `C-k` deletes from cursor to the *end* of line; `M-- C-k` deletes from cursor to the *beginning* of line (not so handy, though: the shorter `C-u` does the same)
* `C-r` searches for a string on the *left* of cursor; `M-- C-r` searches on the *right*

I often use `M-- M-u` to upcase the word I just typed.

<video width="100%" controls>
    <source 
        src="/static/img/lesser-known/negative-argument.mp4" 
        type="video/mp4" />Your browser does not support the video tag />
</video>



# Character Inline Search

| Shortcut | Command                        | Description                               |
|----------|--------------------------------|-------------------------------------------|
| `C-]`    | `(charachter-search)`          | Jump to the next occurrence of a char     |
| `M-C-]`  | `(charachter-search-backward)` | Jump to the previous occurrence of a char |

Move around faster, searching back and forth for the occurrences of a specified character on the current line.

Use `C-]` for searching forward, add `Alt`` for inverting the direction.

If you are a Vim or an Emacs user, you might wonder how you could live in Bash without this functionality since now.

<video width="100%" controls>
    <source 
        src="/static/img/lesser-known/character-search.mp4" 
        type="video/mp4" />Your browser does not support the video tag />
</video>


# String Inline Search

| Shortcut  | Command                    | Description                                 |
|-----------|----------------------------|---------------------------------------------|
| `C-r`     | `(reverse-search-history)` | Jump to the previous occurrence of a string |
| `M-C-r`   |                            | Jump to the next occurrence of a string     |
| `M-- C-r` |                            | As above                                    |

`C-r` is widely known for searching the command history, but few know that the same shortcut, when used in a not empty line, searches in that line. `C-r` searches backword. Of course, prefixing it with a [Negative Argument](#negative-argument) will switch to forward search.

Notice: conclude your search with `C-j`: hitting `Return` will also execute the line, which is not necessarily your desidered outcome.

<video width="100%" controls>
    <source 
        src="/static/img/lesser-known/string-search.mp4" 
        type="video/mp4" />Your browser does not support the video tag />
</video>




# Instant Comment

| Shortcut | Command            | Description                  |
|----------|--------------------|------------------------------|
| `M-#`    | `(insert-commend)` | Comment out the current line |

One of my preferred Zsh's shortcuts is `ESC q`. How many times have I written a longhty command line, only to discover I could not run it, because I was in the wrong directory, or because I forgot to run some other command beforehand? `ESC q` always came to rescue, providing an instant new empty line, and restoring the suspended one right after execution.<br/>

Bash does not have `ESC q`. The related `M-# (insert-comment)` is not exactly the same, but it can be used in similar situations.


# Clipboard

| Shortcut  | Command                 | Description                                           |
|-----------|-------------------------|-------------------------------------------------------|
| `C-space` | `(set-mark)`            | Place a selection mark                                |
| `M-w`     | `(copy-region-as-kill)` | Copy (curiously: "yank", in Vim lingo)             |
| `C-w`     | `(kill-region)`         | Cut                                                |
| `C-y`     | `(yank)`                | Paste from the clipboard (from "yank" in Emacs lingo) |
| `M-y`     | `(yank-pop)`            | Cycle the multi-clipboard                             |
| `C-x C-x` | `((exchange-point-and-mark)`             | Select the other marker (and to make selection visible) |


Bash's support for the clipboard packs a punch in terms of power. In many editors, the selection is defined holding down the Shift key and using the cursor keys. In Bash, just like in Vim and Emacs, you instead define the selection placing 2 handles to mark its beginning and end. The handles can be moved independently, and their position can be adjusted applying whatever movement command, including character and string search, not only using the cursor keys.

Oddly, though, the selection is usually not visible (it is in Zsh).

Notice that the Bash clipboard is a multi-clipboard: it keeps a history of the copied texts enabling you to easily cycle (using `M-y`) it when it's time to paste.

<video width="100%" controls>
    <source 
        src="/static/img/lesser-known/clipboard.mp4" 
        type="video/mp4" />Your browser does not support the video tag />
</video>

Be aware that `(copy-region-as-kill)` and `(kill-region)` must be enabled adding:

```bash
"\ew": copy-region-as-kill
"\C-w": kill-region
```

to `~/.inputrc`.




## Special paste options
### Paste last argument

| Shortcut        | Command           | Description                                     |
|-----------------|-------------------|-------------------------------------------------|
| `M-.`<br/>`M-_` | `(yank-last-arg)` | Paste the last argument of the previous command |

### Paste n-th argunent

| `M-<digit> C-M-y | `(yank-nth-argument)` | Paste the n-th argument of the previous command |


# Deleting

| Shortcut                                 | Command                                              | Description               |
|------------------------------------------|------------------------------------------------------|---------------------------|
| `C-k`                                    | `(kill-line)`                                        | Cut up to the end of line |
| `C-u`<br/> `C-x Backspace`<br/>`M-- C-k` | Complementary to `C-k`: cut to the beginning of line |                           |
| `M-d`                                    | `(kill-word)`                                        | Cut next word             |
| `C-w`                                    | Complementary to `M-d`: cut previous word (from Vim) |                           |


# Transforming words
## Swapping chars and words

| Shortcut | Command             | Description                                           |
|----------|---------------------|-------------------------------------------------------|
| `C-t`    | `(transpose-chars)` | swaps the char on the left with the char on the right |
| `M-t`    | `(transpose-words)` | swaps the word on the left with the word on the right |


<video width="100%" controls>
    <source 
        src="/static/img/lesser-known/swap.mp4" 
        type="video/mp4" />Your browser does not support the video tag />
</video>



## Changing the case

| Shortcut | Command             | Description               |
|----------|---------------------|---------------------------|
| `M-u`    | `(upase-word)`      | Upcase next word          |
| `M-l`    | `(downcase-word)`   | Make next word lower case |
| `M-c`    | `(capitalize-word)` | Capitalize the next word  |

## Suspending a stream

| Shortcut | Command | Description        |
|----------|---------|--------------------|
| `C-s`    |         | Suspend the stream |
| `C-q`    |         | Unsuspend          |

Try to use it while streaming the output of `tail -f`.


# A faster clear

| Shortcut | Command          | Description                                  |
|----------|------------------|----------------------------------------------|
| `C-l`    | `(clear-screen)` | Saves you from writing `clear` all the times |


## A faster and more aggressive clear

| Shortcut | Command           | Description                                       |
|----------|-------------------|---------------------------------------------------|
| `M-C-l`  | `(clear-display)` | Like `clear`, but also clear the off-screen lines |

# History

| Shortcut          | Command                  | Description                                   |
|-------------------|--------------------------|-----------------------------------------------|
| `M-<`<br/>`M-S-,` | `(beginning-of-history)` | Move to the first item in the command history |
| `M->`<br/>`M-S-.` | `(end-of-history)`       | Move to the last item in the command history  |


### Literal chars

| Shortcut | Command           | Description                     |
|----------|-------------------|---------------------------------|
| `C-v`    | `(quoted-insert)` | Add the next character verbatim |

I'm still figuring out what is the use, honestly.


## Return 

| Shortcut | Command         | Description               |
|----------|-----------------|---------------------------|
| `C-j`    | `(accept-line)` | That's an alias of Return |

## Aborting line

| Shortcut | Command   | Description      |
|----------|-----------|------------------|
| `C-c`    | `(abort)` | Abandon the line |

`C-c` is usually used to stop the execution of a command. Few know that it can also be used to quickly cancel and abandon the line being edited.


# Quitting

| Shortcut | Command         | Description |
|----------|-----------------|-------------|
| `C-d`    | `(end-of-file)` | Exit Bash   |

This saves you from writing `exit`.




# Readline
All these funtionalities come from [GNU Readline][readline], a library created in the late 1980s by [[Brian Fox][brian-fox], the same author of Bash. Over the years, readline has become vastly used in a lot of command-line tools such as Tmux, MySQL and [Python][python-readline]. So, those keybindings are almost ubiquitous. Even the GUI text fields in macOS, such as the address bar in browsers, use a Readline-like library, `libedit`, which is based on the Readline's code. No surprises that some of the shortcuts above work everywhere in macOS.

Zsh, instead, does not depend on readline, and implemented a line-editing library from the scratch.

You can find the complete list of commands and features supported Readline in [readline(3)][readline-man].

<!-- # Lesser unknown -->
<!-- https://readline.kablamo.org/emacs.html -->


<!-- ## Unclear -->
<!-- ### Execute commands -->
<!-- * C-p/C-l C-o (execute command, repeat). -->



# Summary

| Shortcut          | Command                        | Description                                             |
|-------------------|--------------------------------|---------------------------------------------------------|
| `C-k`             | `kill-line`                    | Cut from cursor to end of line                          |
| `C-u`             | `(unix-line-discard)`          | Complementary to `C-k`: cut to the beginning of line    |
| `C-x Backspace`   |                                | As above                                                |
| `M-- C-k`         |                                | As above                                                |
| `C-M-l`           |                                | Clear screen and off-screen lines                       |
| `C-y`             | `yank`                         | Paste                                                   |
| `C-w`             | `kill-region`                  | Cut                                                     |
| `M-w`             | `kill-ring-save`               | Copy                                                    |
| `M-y`             | `yank-pop`                     | Cycle and paste                                         |
| `C-a`             | `(beginning-of-line)`          | Move to the first character of the line                 |
| `C-e`             | `(end-of-line)`                | Move to the last character of the line                  |
| `M-d`             | `(kill-word)`                  | Cut the next word                                       |
| `C-f`             | `(forward-char)`               | Move to the next char                                   |
| `M-f`             | `(forward-word)`               | Move to the next word                                   |
| `C-b`             | `(backward-char)`              | Move the previous char                                  |
| `M-b`             | `(backward-word)`              | Move to previous word                                   |
| `C-d`             | `(delete-char)`                | Like Canc, delete the next character                    |
| `M-<digit>`       | `(digit-argument)`             | Repeat the next command `n` times                       |
| `M--`             | `(negative-argument)`          | Invert the next command                                 |
| `C-]`             | `(charachter-search)`          | Jump to the next occurrence of a char                   |
| `M-C-]`           | `(charachter-search-backward)` | Jump to the previous occurrence of a char               |
| `C-r`             | `(reverse-search-history)`     | Jump to the previous occurrence of a string             |
| `M-C-r`           |                                | Jump to the next occurrence of a string                 |
| `M-#`             | `(insert-commend)`             | Comment out the current line                            |
| `C-space`         | `(set-mark)`                   | Place a selection mark                                  |
| `M-w`             | `(copy-region-as-kill)`        | Copy (curiously: "yank", in Vim lingo)                  |
| `C-w`             | `(kill-region)`                | Cut                                                     |
| `C-y`             | `(yank)`                       | Paste from the clipboard (from "yank" in Emacs lingo)   |
| `M-y`             | `(yank-pop)`                   | Cycle the multi-clipboard                               |
| `C-x C-x`         | `((exchange-point-and-mark)`   | Select the other marker (and to make selection visible) |
| `M-.`<br/>`M-_`   | `(yank-last-arg)`              | Paste the last argument of the previous command         |
| `C-k`             | `(kill-line)`                  | Cut up to the end of line                               |
| `M-d`             | `(kill-word)`                  | Cut next word                                           |
| `C-w`             | `(unix-word-rubout)`           | Complementary to `M-d`: cut previous word (from Vim)    |
| `C-t`             | `(transpose-chars)`            | swaps the char on the left with the char on the right   |
| `M-t`             | `(transpose-words)`            | swaps the word on the left with the word on the right   |
| `M-u`             | `(upase-word)`                 | Upcase next word                                        |
| `M-l`             | `(downcase-word)`              | Make next word lower case                               |
| `M-c`             | `(capitalize-word)`            | Capitalize the next word                                |
| `C-s`             |                                | Suspend the stream                                      |
| `C-q`             |                                | Unsuspend                                               |
| `C-l`             | `(clear-screen)`               | Saves you from writing `clear` all the times            |
| `M-C-l`           | `(clear-display)`              | Like `clear`, but also clear the pages off-screen       |
| `M-<`<br/>`M-S-,` | `(beginning-of-history)`       | Move to the first item in the command history           |
| `M->`<br/>`M-S-.` | `(end-of-history)`             | Move to the last item in the command history            |
| `C-v`             | `(quoted-insert)`              | Add the next character verbatim                         |
| `C-j`             | `(accept-line)`                | That's an alias of Return                               |
| `C-c`             | `(abort)`                      | Abandon the line                                        |
| `C-d`             | `(end-of-file)`                | Exit Bash                                               |


# References
* [GNU Readline][readline]
  * [man page][readline-man]
* [Bash manpage][bash-man]
* [Brian Fox][brian-fox]
* [Python readline][python-readline]


[readline]: https://tiswww.case.edu/php/chet/readline/rltop.html
[readline-man]: https://www.man7.org/linux/man-pages/man3/readline.3.html 
[bash-man]: https://manpages.org/bash
[python-readline]: https://docs.python.org/3/library/readline.html
[brian-fox]: https://en.wikipedia.org/wiki/Brian_Fox_%28computer_programmer%29

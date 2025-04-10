---
layout: post
title: "Emacs: let's zoom"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- emacs
- lisp
---
Let's develop `squint` a little package for controlling the font
height, so you won't need to squint your eyes when you are on smaller
screens.  
It will look like this:


<img width="100%" src="/static/img/emacs/squint/squint.gif" />

It will give us the chance to touch on:

- Face attributes.
- Asking the user to choose interactively from a list of options.
- The powerful `consult--read`.
- Lisp's Dynamic and Lexical Binding.
- Few other little topics here and there.

<!--more-->

## Faces

In Emacs, the "face" is the visual styling attribute that can be
applied to text: faces define how text appears on the screen,
including the font, the colors, and the weight. And the height, the
attribute we want to control in this little tutorial.

There is a notion of "default face" in Emacs: it's basically the
parent from which all the other faces inherit their attributes, unless
they specify a specialization. Changing an attribute of the default
face will propagate the change everywhere in Emacs.

Try yourself. Open your scratch buffer (`M-x scratch-buffer RET`) and
write:

```elisp
(face-attribute 'default :height)
```

Evaluate it: move right after the last `)` and hit `C-j` (or `M-x
eval-print-last-sexp RET`). This reveals the current height of the
default face. In my case, it prints `180`.

Good. Now, try to change it:

```elisp
(set-face-attribute 'default nil :height 100)
```

Evaluate it: again, move right after the last `)` and hit `C-x
C-e`. As expected, any text in Emacs will inherit the new defined
height.

Cool, you learnt how to programmatically control the global text
height.  
There are other attributes you can control, such as the foreground and
background colors, the weight and the font. You can learn more about
this topic from the chapter [Faces][faces] in the Emacs Manual.  
But let's focus on height.

## Controlling the current buffer faces

`set-face-attribute` operates globally. If you want to control the
faces of the current buffer only, you need to play with
*remappings*. I will skate over this topic, but just quickly:

```elisp
(setq cookie
      (face-remap-add-relative 'default :height 280))
```

changes the height of the default face *in the current buffer
only*. Indeed, it creates a *remap* of the face, that is a way to
temporarily redirecting how a face is displayed.

It returns a *cookie*, that we save in the `cookie` variable. When you
want to remove the remap, you have to provide that cookie to
`face-remap-remove-relative`:

```elisp
(face-remap-remove-relative cookie)
```

We won't need to worry about this: our `squint` package will operate
globally.

## Controlling the height interactively

There are 2 ways for interactively modifying the size of characters on
the screen:

| Command                      | Keybindings                                                        | Description                                |
|------------------------------|--------------------------------------------------------------------|--------------------------------------------|
| `(text-scale-adjust)`        | `C-x C-0` <br/> `C-x C-=` <br/> `C-x C--` <br/> `C-x C-+`          | Adjust the font size in the current buffer |
| `(global-text-scale-adjust)` | `C-x C-M-0` <br/> `C-x C-M--` <br/> `C-x C-M-=` <br/> `C-x C-M-+.` | Adjust the font size in globally           |

They are straightforward to use: activate them, then follow the
instructions on the screen, hitting:

| Key   | Purpose                      |
|-------|------------------------------|
| `+`   | Increase the height          |
| `-`,  | Decrease the height          |
| `0`   | Reset the height             |
| `ESC` | Quit the interactive session |


That easy.

## Grab your keeb
Enough with theory. Our goal is to create a new command, `squint`,
offering a list of height presets to choose from interactively, with
narrowing and realtime preview.

We will proceed incrementally, each time improving the result.

### Step 1: set height from key/value pairs

The basic idea is to have a collection of heights in an alist (see
[Association Lists][alists]), that is a list of key-value pairs, such
as:

```elisp
(setq squint-heights
      '(("office" . 100)
        ("laptop" . 180)
        ("programming" . 200)
        ("presentation" . 300)))
```

and to set the corresponding height through its label, with:

```elisp
(squint "presentation")
(squint "laptop")
```

It does not sound as a very challenging problem, does it? We need to
define a function `squint` that:

- Given a label, retrieves the corresponding height from
  `squint-heights`.
- Once obtained the height, uses the `set-face-attribute` we saw
  before to adjust the font height.


Retrieving a value given a key from an alist is simply done with
`alist-get`:

```elisp
(alist-get "programming" squint-heights nil nil #'equal)
```

Here's the signature:

```
(alist-get 
    KEY     ;; the key to search for
    ALIST   ;; the alist to search in
    DEFAULT ;; the value to return in case KEY is not found
    REMOVE  ;; whether the pair should be removed in case of success
    TESTFN) ;; the equality function to use for comparing keys
```

`TESTFN` defaults to `eq`, which is a bit unfortunate: `eq` does not
return true if the two args are equal, but only if they are the very
same Lisp object. In other words,

```elisp
(eq "ciao" "ciao")
```

is false: although the 2 strings are equal, they are not the same
object.  
This:

```elisp
(setq ciao "ciao")
(eq ciao ciao)
```

returns true.  
Instead of `eq` we'd better use `equal` which, more conveniently,
returns true if two objects have similar structure and contents.  
Since `TESTFN` is a positional parameter, we are also forced to
provide 2 `nil` values for the other parameters `DEFAULT` and
`REMOVE`.

That's all you need to know to finally define:

```elisp
(setq squint-heights
      '(("office" . 100)
        ("laptop" . 180)
        ("programming" . 200)
        ("presentation" . 300)))

(defun squint (label)
  "Adjust font height based on predefined settings.
LABEL is a string that corresponds to a key in `squint-heights'."
  (let ((height (alist-get label squint-heights nil nil #'equal)))
    (set-face-attribute 'default nil :height height)))
```

Evaluate the code above (`C-x C-e` after each expression), then run:

```elisp
(squint "presentation")
```

Such a large font! Sweet!

### Step 2: symbols instead of strings
The quirk about using `equal` can be avoided using [symbols][symbols]
(such as `'office` and `'laptop`) instead of strings such as
`"office"` and `"laptop"`.

With strings one must be careful when to use `eq` and when to use `equal`:

| Expression                  | Value | Comment                                      |
|-----------------------------|-------|----------------------------------------------|
| `(eq "office" "office")`    | `nil` | Although equal, the 2 are different objects. |
| `(equal "office" "office")` | `t`   | `equal` compares the value.                  |

With symbols, equality is much easier:

| Expression                | Value | Comment                                                  |
|---------------------------|-------|----------------------------------------------------------|
| `(eq 'office 'office)`    | `t`   | All symbols called `'office` are the very same instance. |
| `(equal 'office 'office)` | `t`   | `'office`'s value is `'office`, so `equal` is true too.  |


Let's try redefining `squint-heights` using symbols instead of strings
for its keys:

```elisp
(setq squint-heights
      '((office . 100)
        (laptop . 180)
        (programming . 200)
        (presentation . 300)))
```

The usage of `alist-get` is a bit shorter:

```elisp
(alist-get 'programming squint-heights)
```

Notice the use of `'programming` instead of `"programming"`.  
This leads to the following simplified implementation:

```elisp
(setq squint-heights
      '((office . 100)
        (laptop . 180)
        (programming . 190)
        (presentation . 200)))

(defun squint (label)
  "Adjust font height based on predefined settings.
LABEL is a symbol that corresponds to a key in `squint-heights'."
  (let ((height (alist-get label squint-heights)))
    (set-face-attribute 'default nil :height height)))
```

Invoke it with:

```elisp
(squint 'presentation)
```

Still works.

### Step 3 - Helper functions
Indulge me while I extract a few private functions. They will come in handy
when we will change the behavior in the next steps.  
In:

```elisp
(defun squint (label)
  "Adjust font height based on predefined settings.
LABEL is a symbol that corresponds to a key in `squint-heights'."
  (let ((height (alist-get label squint-heights)))
    (set-face-attribute 'default nil :height height)))
```

we would like to have a function `squint--height-from-label` returning
the height given the label. That's easy:

```elisp
(defun squint--height-from-label (label)
  (alist-get label squint-heights))
```

Then, we could abstract the details of changing the face attribute
with:

```elisp
(defun squint--set-height (height)
  (set-face-attribute 'default nil :height height))
  
(defun squint--set-height-from-label (label)
  (squint--set-height (squint--height-from-label (label))
```

Putting all together:


```elisp
(setq squint-heights
      '((office . 100)
        (laptop . 180)
        (programming . 190)
        (presentation . 200)))

(defun squint--height-from-label (label)
  (alist-get label squint-heights))

(defun squint--set-height (height)
  (set-face-attribute 'default nil :height height))

(defun squint--set-height-from-label (label)
  (squint--set-height (squint--height-from-label label)))

(defun squint (label)
  "Adjust font height based on predefined settings.
LABEL is a symbol that corresponds to a key in `squint-heights'."
  (squint--set-height-from-label label))
```

That's all. Verify that:

```elisp
(squint 'programming)
```

still works.


### Step 4 - Making squint interactive
Although you have defined `squint`, there is no sign of it in the list
of commands displayed when you hit `M-x`. Yet, the function is defined
globally: invoking `M-x describe-function RET squint RET` you can even
read its documentation.

The fact is: `M-x` only lists *commands*, i.e., those functions that
are explicitly declared as interactive.

Well, let's make `squint` interactive then:


```elisp
(defun squint (label)
  (interactive)
  "Adjust font height based on predefined settings.
LABEL is a symbol that corresponds to a key in `squint-heights'."
  (squint--set-height-from-label label))
```

Is this enough? Try yourself: hit `M-x squint RET` and you will get
back the error:

```
funcall-interactively: Wrong number of arguments: 
  #[(label) (nil (squint--set-height-from-label label)) nil nil nil nil], 0
```

The problem is: the poor `(interactive)` function might even
understand that it needs to ask the user for a `label` argument, but
it cannot possibly figure out if this is a file, a string, a number or
a symbol from a list, such as in our case. You must give it a hint.  
Enter `completing-read`.

### completing-read
Try:

```elisp
(completing-read
 "Choose your poison: "
 '("arsenic" "digitalis" "strychnine" "belladonna"))
```

Evaluate it, make your choice and notice how it returns the selected
string.  
Does it work with symbols too?

```elisp
(setq symbols
      '(office laptop programming presentation))

(completing-read
 "Choose your poison: "
 symbols)
```

Yes, it does! But notice: you gave it a symbols, it returns a
string. Weird, but it is like it is.  
What if you feed it with an alist?

```elisp
(setq squint-heights
      '((office . 100)
        (laptop . 180)
        (programming . 190)
        (presentation . 200)))

(completing-read
 "Choose your poison: "
 squint-heights)
 ```

It still works, and it still returns the key as a string. Yes, it
would have been easier if it returned the value rather then the key,
but that's life.

You can combine `interactive` with `completing-read`:

```elisp
(setq squint-heights
      '((office . 100)
        (laptop . 180)
        (programming . 190)
        (presentation . 200)))

(defun squint--height-from-label (label)
  (alist-get label squint-heights))

(defun squint--set-height (height)
  (set-face-attribute 'default nil :height height))

(defun squint--set-height-from-label (label)
  (squint--set-height (squint--height-from-label label)))

(defun squint (label)
  (interactive 
   (completing-read "Desired height: " squint-heights))
  "Adjust font height based on predefined settings.
LABEL is a symbol that corresponds to a key in `squint-heights'."
  (squint--set-height-from-label label))
```

Try `M-x squint` now. Good: now it shows an interactive menu to choose
from.  
Make your choice, hit `RET` and be disappointed seing the debug window
screaming:

```
squint--set-height: Default face height not absolute and positive
```

What's wrong?  
There are 2 issues:

- The first is how we use `interactive`. If only we spent the time to
  consult its documentation (`M-x describe-function RET interactive
  RET`) we would have seen this line:
  
```
If the argument is not a string, it is evaluated to get a list of
 arguments to pass to the command.
```

We have:

```elisp
  (interactive 
   (completing-read "Desired height: " squint-heights))
```

and we know that `completing-read` returns a string (indeed: the
selected key, as a string), not a list. Indeed, `interactive` must
work for an arbitrary number of parameters, so it makes sense it
expects a list.  
We can easily fix this surrounding `completing-read` with a list
constructor:

```elisp
(defun squint (label)
  (interactive (list
                (completing-read "Desired height: " squint-heights)))
  (squint--set-height-from-label label))
```

- The second problem could also be easily forseen: `completing-read`
  returns a string, but our `squint--height-from-label` expects a
  symbol, because the `squint-heights` alist uses symbols as keys.  
  Here is where having refactored those helper functions away pays
  off. We can use the function `intern`, that returns a symbol given a
  string:
  
Just change:

```elisp
(defun squint--height-from-label (label)
  (alist-get label squint-heights))
```

to:

```elisp
(defun squint--height-from-label (label)
  (alist-get (intern label) squint-heights))
```

Putting all together:

```elisp
(setq squint-heights
      '((office . 100)
        (laptop . 180)
        (programming . 190)
        (presentation . 200)))

(defun squint--height-from-label (label)
  (alist-get (intern label) squint-heights))

(defun squint--set-height (height)
  (set-face-attribute 'default nil :height height))

(defun squint--set-height-from-label (label)
  (squint--set-height (squint--height-from-label label)))

(defun squint (label)
  (interactive (list
                (completing-read "Desired height: " squint-heights)))
  (squint--set-height-from-label label))
```

Try `M-x squint RET` (or, if you developed a taste for writing lisp,
evaluate `(call-interactively #'squint)`): it will equally work.

### Step 5 - Preview! Preview!
`completing-read` does already a lot.

- It lets you select the preset with arrow keys.
- It sorts the options accordingly to their most recent usage.
- It lets you filter by typing (try typing  `"prog""` to see the list
  interactively narrow down to `programming`).

The next cool feature you might desire to have is a real-time
preview. That is, the very moment you move on a candidate, even before
confirming the selection, you would like to see its effect in
place. Then:

- Confirming the selection would keep the new height.
- Canceling the selection, it would be nice to reset to the previous
  height.
  
This is much more challenging to implement. But maybe we can start by
reusing what the magnificent [consult][consult] package offers.

If you don't have consult installed yet, show deep remorse, publicly express
shame and amend by adding something like this to your init file:

```elisp
(use-package consult
  :ensure t
  :demand t
  :bind (("M-g M-g" . consult-goto-line)
         ("C-x b" . consult-buffer)
         ("C-s"   . consult-line)
         ("C-S-s" . isearch-forward)
         ("C-x r b" . consult-bookmark)
         ("M-y" . consult-yank-pop)
         ("C-c r r" . consult-ripgrep)
         ("C-c g g" . consult-git-grep)
         ("C-c f l" . consult-focus-lines)))
```

You will not regret it.

Then, in your `squint` little package, replace `completing-read` with
`consult--read`. Let's see how.

#### consult--read
To break the ice, I suggest you to experiment with the following:

```elisp
(consult--read
 '("arsenic" "digitalis" "strychnine" "belladonna")
 :prompt "Choose your poison: "
 :state (lambda (action candidate)
          (pcase action
            ('preview (message "preview: %s" candidate))
            ('return (if candidate
                         (message "return: %s" candidate)
                       (message "return: Nothing"))))))
```

`consult--read` is a bit more complex than `completing-read`, but for
our goal it should suffice to know the following:

- Remember that `completing-read` happily worked with lists of
  strings, lists of symbols or even alists? Well, `consult--read` is
  slightly pickier, as it does not like lists of symbols. Curiously,
  it accepts association lists whose keys are symbols. So, you can
  keep feeding it with `squint-heights` as the list of candidates,
  just like you used to to with `completing-read`.
  
- Just like `completing-read`, after the candidate has been chosen, it
  returns the selected key as a string.
  
- For any action performed by the user, the lambda function defined
  at `:state` is invoked, with 2 arguments: `action`, and `candidate`.
  
This lambda requires a bit of explanation.  
It is invoked during several stages of the execution. The argument
`action` specifies which. The value of `candidate` indicates the
selected candidate, or `nil` if no candidate matches or if the user
decided to cancel. Or in few other cases. And this makes things a bit
hairy.  
In practice, just know that the couple of `action` / `candidate` shall
be interpreted as follows:

| Value of `action` | Value of `candidate` | Meaning                                                                                 |
|-------------------|----------------------|-----------------------------------------------------------------------------------------|
| `setup`           | `nil`                | `consult--read` has just been invoked.                                                  |
| `preview`         | some candidate       | The user focused on a candidate.                                                        |
| `preview`         | `nil`                | The user either typed something not matching any candidate, or completed the operation. |
| `exit`            | `nil`                | `cosult--read` is preparing to close the minibuffer.                                    |
| `return`          | some candidate       | The user confirmed with a valid candidate.                                              |
| `return`          | `nil`                | The user quit, without selecting a valid candidate.                                     |


I might be wrong, but although I think consult is a masterpiece, I
would have prefered `consult--read` to be a bit more
straightforward. Indeed, I find it a bit confusing.  
Play with the example I provided above, keeping the `*Messages*`
buffer open (`M-x view-echo-area-messages` or `C-h e`) and notice the
following:

- As you select a candidate, the lambda is invoked passing `preview`
  as the action and the candidate name. This is what we will use to
  preview the selected `squint-height`.
  
- When you confirm the selection:

  - `preview` is invoked a last time, with `nil` as the
    candidate. What the hell?
  - Luckily, `return` is also invoked, with the selected
    candidate. Nice, it's likely we will use this for the final
    selection.
    
- Now, try to cancel the selection, using `C-g`:

  - Also in this case, `preview` is invoked a last time, with `nil` as
    the candidate. It's likely that we will need to workaround those
    funny empty calls.
  - Also in this case, `return` is finally invoked. This time, though,
    with `nil` as the candidate.
    
- Finally, try inputing garbage so that no candidate matches:

  - You will see the very same behavior as for legit candidates. This
    is also confusing.
  

A convenient option that helps handling some cases is `:require-match
t`, which prevents the user from confirming the selection with a
non-existing candidates. Unfortunately for our goals, `preview` is
still invoked passing non-matching candidtes. Try yourself:


```elisp
(consult--read
 '("arsenic" "digitalis" "strychnine" "belladonna")
 :prompt "Choose your poison: "
 :require-match t
 :state (lambda (action candidate)
          (pcase action
            ('preview (message "preview: %s" candidate))
            ('return (if candidate
                         (message "return: %s" candidate)
                       (message "return: Nothing"))))))
```

In conclusion: it seems that for the `preview` case we have to
manually make sure that the input candidate is actually one of the
valid choises. Since the candidate list `squint-heights` is an
association list, we could use `(assq key alist)`, a function
returning the key/value pair if `key` is a key in `alist`, `nil`
otherwise. This leads us to:

```elisp
(setq squint-heights
      '((office . 100)
        (laptop . 180)
        (programming . 190)
        (presentation . 200)))
        
(consult--read
 squint-heights
 :prompt "Choose your poison: "
 :require-match t
 :state (lambda (action candidate)
          (pcase action
            ('preview 
                (if (assq (intern candidate) squint-heights)
                  (message "preview: %s" candidate)
                  (message "Nothing to preview")))
            ('return (if candidate
                         (message "return: %s" candidate)
                       (message "return: Nothing"))))))
```

An alternative could be to get the height directly, with out
`squint--height-from-label`, then combining the definition of a
variable and an if/then/else clause with `if-let`:


```elisp
(setq squint-heights
      '((office . 100)
        (laptop . 180)
        (programming . 190)
        (presentation . 200)))

(consult--read
 squint-heights
 :prompt "Choose your poison: "
 :require-match t
 :state (lambda (action candidate)
          (pcase action
            ('preview 
                (if-let (height (squint--height-from-label candidate))
                  (message "selected: %s, previewing with height: %s" candidate height)
                  (message "Nothing to preview")))
            ('return (if candidate
                         (message "return: %s" candidate)
                       (message "return: Nothing"))))))
```

Cool. It seems to work.  
By the way, having obtained a `height` already at this point will
allow us to use `squint--set-height` directly, getting rid of the
`squint--set-height-from-label` helper function.


Let's keep on analyzing. Remember that `preview` is being called a
last time, with a `nil` candidate? That's the case when we don't need
to preview anything. We'd better replace the `if-let` case:


```elisp
                (if-let (height (squint--height-from-label candidate))
                  (message "selected: %s, previewing with height: %s" candidate height)
                  (message "Nothing to preview")))
```

with `when-let`, which does the same of `if-let`, without an else leg:

```elisp
                (when-let (height (squint--height-from-label candidate))
                  (message "selected: %s, previewing with height: %s" candidate height)))
```

##### Reset
We are almost done. We just need to cover the `return`
case.

Think about this: we will preview the selected height by invoking our
`squint--set-height`: by itself, this is a destructive operation. I
mean, although the intention is only to *preview* the new height,
there is no magic way to ask Emcs "I joked: undo what you just
did". Most likely, instead, we will need to keep track of the initial
face height, and to invoke `squint--set-height` with that value, if
the user cancels.

So:

- If the user confirms the selection, we do nothing: we can keep the
  already previewed face height.
- If the user cancels, we need to reset the height to the initial one.
  
  
This all means that we definitely need to store the initial height
somewhere before invoking `consult--read`. We could either use a
global variable, or a local variable, which we can conveniently
capture in a closure.

Let's play with a closure. Here's a possible approach:

```elisp
(let ((previous-height (face-attribute 'default :height)))

  (defun squint--reset-height ()
    (message "%s" previous-height)
    (squint--set-height 180))

  (defun squint ()
    (interactive)
    (consult--read
     squint-heights
     :prompt "Choose your poison: "
     :require-match t
     :state (lambda (action candidate)
              (pcase action
                ('preview (squint--set-height (squint--height-from-label candidate)))
                ('return (when (null candidate)
                             (squint--reset-height))))))))
```

For closures to work as you expect, [Lexical Binding][lexical-binding]
must be active. You ither need to add:

```elisp
;; -*- lexical-binding: t -*-
```

as th very first line of your file, or to set:

```elisp
(setq lexical-binding t)
```

You probably like to have a little explanation here. Let's see.

#### Dynamic and Lexical Binding
Consider this:

```elisp
;; -*- lexical-binding: t -*-

(let ((v "captured value"))
  (defun run-me ()
    (message "v = %s" v)))
```

It first defines a variable `v` with some value. `v` is not a global
variable; it only exists in the scope defined by its `let`, so
basically in the 2 lines under `let` itself.

Inside that scope, a function `run-me` is defined. Notice that
`run-me`'s body makes use of `v`, and that `v` itself, from the
`run-me` function standpoint, is defined in the outer scope.

We say that `run-me` captures `v` from its surrounding scope.

Now: you know that local variables are accessible only from within the
scope they are defined. So, the following does not work:

```elisp
;; -*- lexical-binding: t -*-

(let ((v "captured value"))
  (defun run-me ()
    (message "v = %s" v)))

(message "Do we have a v here? v = %s" v)
```

`v` does not exist outside its `let` scope.  
Interestingly, though, this works:

```elisp
;; -*- lexical-binding: t -*-

(let ((v "captured value"))
  (defun run-me ()
    (message "v = %s" v)))

(run-me) ;; v = captured value
```

This works because, as we said, `run-me` *captured* the variable
`v`. In other words, `run-me` keeps a reference to the scope it was
created in, so that scope does not die when the `let` expression
finishes its execution.  
This is basically the idea of *closures*.

Now, consider this slightly different version:


```elisp
;; -*- lexical-binding: t -*-

(let ((v "captured value"))
  (defun run-me ()
    (message "v = %s" v)))

(let ((v "some other value"))
  (run-me))
```

Not surprisingly, the last expression still emits `v = captured
value`.

In fact: we are creating another, separate variable `v`, with a
different value. When `run-me` is evaluated, though, it still
reference its own `v`. Although sharing the same name, the two `v`
variables exist and live in separate, independent scopes. Which scope
a specific piece of code is using can be easily determined by looking
at *where* it is defined in the code structure: in other
words, it can inferred from their *lexical context*.  
This behavior is called *lexical binding*.

Emacs also supports *dynamic binding*. See the difference:

```elisp
(let ((v "captured value"))
  (defun run-me ()
    (message "v = %s" v)))

(let ((v "some other value"))
  (run-me))
```

Notice that I removed the:

```elisp
;; -*- lexical-binding: t -*-
```

I could even enabled explicitly the dynamic binding with `(setq
lexical-scope nil)`.

Surprisingly, now the last expression emits `some other value`. 

In Dynamic Binding, variables are resolved based on the *runtime
environment* rather than on the *definition-time environment*.  
When `run-me` is defined, it doesn't *capture* `v`. Instead, it just
remembers that it needs to use a variable named `v`.  
When finally `run-me` is invoked, it looks for the current value of
`v` *in the caller scope*, at run-time. It finds a `v` set to `some
other value`, and it uses it.

You will find very few languages supporting this style. You can learn
more on this reading [Lexical Binding][lexical-binding-wiki] in the
Emacs Wiki.

So, here's the code we ended up with:

```elisp
;; -*- lexical-binding: t -*-

(setq squint-heights
      '((office . 100)
        (laptop . 180)
        (programming . 190)
        (presentation . 200)))

(defun squint--height-from-label (label)
  (alist-get (intern label) squint-heights))

(defun squint--set-height (height)
  (set-face-attribute 'default nil :height height))

(defun squint--get-labels (squint-heights)
  "Return a list of string labels from squint-heights."
  (mapcar (lambda (pair) (symbol-name (car pair)))
          squint-heights))

(let ((previous-height (face-attribute 'default :height)))

  (defun squint--reset-height ()
    (message "%s" previous-height)
    (squint--set-height 180))

  (defun squint ()
    (interactive)
    (consult--read
     squint-heights
     :prompt "Choose your poison: "
     :require-match t
     :state (lambda (action candidate)
              (pcase action
                ('preview (squint--set-height (squint--height-from-label candidate)))
                ('return (when (null candidate)
                             (squint--reset-height))))))))
```

Evaluate it, and you will have your beautiful `squint` command at
hand.

### Step 6 - Make it a package

It would be nice to have `squint` loaded when Emacs starts. I'm also
sure that, as you get better and better with Lisp, you will improve
this little function making it so beautiful that every single Emacs
user in the world will want to use it. Eventually, you will need to
publish it on Melpa, and it will overtake Magit's monthly downloads.

So, you need to make it a package.  
This is indeed a very easy task.

First, add a very last line declaring that the file *provides* a
package:

```elisp
(provide 'squint)
```

Second, make sure that the package name (`squint`) is consistent with
the file name (`squint.el`).

Finally, store the file in one of the directories Emacs parses when it
looks for packages. Inspect the variable `load-path` to know which
ones (`M-x describe-variable RET load-path RET`).  
In my case I get a huge list like this:

```elisp
("/usr/share/emacs/site-lisp/"
 "~/.config/emacs/local-packages"
 
 "/home/arialdo/.config/emacs/elpa/ag-20201031.2202"
 "/home/arialdo/.config/emacs/elpa/aggressive-indent-20230112.1300"
 "/home/arialdo/.config/emacs/elpa/auto-hide"
 "/home/arialdo/.config/emacs/elpa/avy-20230420.404"
 "/home/arialdo/.config/emacs/elpa/buffer-expose-0.4.3"
 "/home/arialdo/.config/emacs/elpa/corfu-20241030.1005"
 ...
 "/usr/share/emacs/31.0.50/site-lisp"
 "/usr/share/emacs/site-lisp"
 "/usr/share/emacs/31.0.50/lisp"
 "/usr/share/emacs/31.0.50/lisp/vc"
 "/usr/share/emacs/31.0.50/lisp/use-package"
 "/usr/share/emacs/31.0.50/lisp/url"
 "/usr/share/emacs/31.0.50/lisp/textmodes"
 "/usr/share/emacs/31.0.50/lisp/progmodes"
 ...
 "/usr/share/emacs/31.0.50/lisp/calc"
 "/usr/share/emacs/31.0.50/lisp/obsolete")
```

Notice the second item: `"~/.config/emacs/local-packages"`: this is a
custom directory I created for my personal packages. here's what
you need to do:

- Create a directory in your home or in the standard XDG directory
  (`~/.config/emacs`) where to keep your personal packages.
- Then, in your init file, instruct Emacs to add this directory to the
  list of directories to search in:
  

```elisp
  (add-to-list 'load-path "<YOUR_DIRECTORY_HERE")
```

That's it. Now you can load `squint` with:

```elisp
(require 'squint)
```

If you prefer `use-package` like I do, go with:

```elisp
(use-package squint
  :ensure nil
  :custom
  (squint-heights '((office . 1000)
                    (laptop . 180)
                    (programming . 190)
                    (presentation . 200)))
  :bind ("C-c s q" . squint))
```

Never mind that your code already contains a definition for
`squint-heights`: `use-package` is smart enough to use the value
you provide in `:custom`.

Cool! Your first package!  
Of course, there would be so much to add and study about packages
&mdash; such as how to write a proper documentation, how to declare
dependencies on other packages and the like. But you can be proud of
yourself already, can't you?

## Where to go from here?
Why should `squint` work with font heights only? Why not to have
presets for any arbitrary face attribute?  
The scaffold is set up: you will not find hard to extend what you
wrote so far.

If you want to get inspiration and to learn from a real master, go
checkout [Fontaine][fontaine], by [Protesilaos][prot]. It does exactly
that.  
In fact, this post is an elaboration of what I learnt from some lessons
I got from him, as I wanted to better understand how his packages work.

That's all. Take care. Happy lisping!

# References

* Emacs Manual
  * [Text Scale][text-scale]
  * [Faces][faces]
  * [Association Lists][alists]
  * [Symbols][symbols]
  * [Lexical Binding][lexical-binding]
    * [Lexical Binding - Wiki][lexical-binding-wiki]
* [consult][consult]
* [Fontaine][fontaine]
* [Protesilaos Stavrou][prot]

[text-scale]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Text-Scale.html
[faces]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Faces.html
[alists]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Association-Lists.html
[symbols]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Symbols.html
[lexical-binding]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Lexical-Binding.html
[lexical-binding-wiki]: https://www.emacswiki.org/emacs/LexicalBinding
[consult]: https://github.com/minad/consult
[fontaine]: https://protesilaos.com/emacs/fontaine
[prot]: https://protesilaos.com/

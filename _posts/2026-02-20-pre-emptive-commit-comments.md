---
layout: post
title: Pre-emptive Commit Messages
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- git
- jujutsu
---
1. Write commit messages before coding.
2. Describe what the software does, not what you have done.

<!--more-->

## Behaviours, Not Tests

Dan North changed my life.

I always recommend his [Introducing
BDD](https://dannorth.net/blog/introducing-bdd/). It explains how
Behaviour-Driven Development started from the intuition that TDD is
not merely about testing. The word "*test*" itself points developers
in the wrong direction, he says, toward verification, toward the
past. He prefers using "*behaviour*".

It's a dramatic change of perspective. For Dan North tests document
the system's behaviour from the outside, from the point of view of
Business Users; therefore, they describe what the system *should do*,
using narrative sentences. They are in fact business requirements,
written in the business language and, of course, conceived before the
implementation even exists.

## Why Test-First?

Some find it counterintuitive, if not crazy, writing tests before the
system under test even exists. Yet it's the opposite approach that
should sound crazy. What are we supposed to do? To write the code
without knowing where to go and then, when we are done, to figure out
what the requirements were?

If only TDD was called Requirement-Driven Development, no one would
find it counterintuitive. "*Test*" suggests the idea of verifying,
something that is already done. Not the most fortunate pick, Kent...

As Dan North wrote:

> A really useful way to stay focused was to ask:  
> "What's the next most important thing the system doesn’t do?"

This question defines your next test; the test becomes a statement of
intent, a promise. With the test in place, you *commit* (here's the
word!) to a specific, future behaviour for your product.

## A Commit Is A Promise

I started thinking to commit messages the same way, and
something clicked. Maybe the word "*commit*" in the Git lingo is not casual.
Maybe making a commit means making a promise too.

It's funny how Git lets you commit to something when the code is
already complete. A bit late, indeed.  Even funnier how in
[Jujutsu][jujutsu] it's the exact opposite to be idiomatic: you first
`jj commit`, then you write the code.  
We'll get this in few seconds. Back to the message.

## How To Describe A Promise?

Messages such as:

```
- Add Foo
- Remove method Baz
- Use Set instead of List
```

are neither commitments nor behaviour descriptions: they are activity
reports. And yet a commit message could be written in the style of a BDD method:

```
When the user saves the file, a preview is shown
```

That's not a cosmetic difference. The formers are about the actions
performed by the programmer, the past, the *how*. The latter describes
the feature, the present, the *what*.

## Tell Me What The Software Does, Not What You Have Done

The second form is what I would like to read in the Git history, and
is in line with the idea of [Conventional
Commits][conventional-commits], which promises to "[Automatically
generate CHANGELOGs](https://www.conventionalcommits.org/en/v1.0.0/#why-use-conventional-commits)".

When I checkout a commit, I know that someone worked hard on their
keyboard to make the software behave like it should. I'm grateful, but
when it's my turn to work on top of that commit there's no point
answering the question:

    What did the programmers do during that work session?
    
It's more usesul to have an answer to:

    What's the project behaviour NOW?

A comment like:

    "Fix Gitk, cache commit info array"
    
is worth less than:

    When the history exceeds 10k commits, Gitk doesn't crash

The principles one might derive from this are simple:

- Talk about the program behaviour, not about yourself.
- Don't tell me what the software's was, describe what it is now, in
  present tense.
- There's no need to add time references: the commit already has a
  timestamp, it's by definition about "now".

## Write It First
Then I stumbled upon this tweet by [Eric Willeke](https://twitter.com/erwilleke):

![Eric Willeke Tweet](static/img/pre-emptive-commit-messages/erik-git-comments1.png)

A point of view of disarming simplicity. It's **test-first applied to
version control**. It instantly resonated with me.

As I was accustomed to write tests before the implementation, it was
easy to start writing commit messages before coding. Messages became
statement of intents, commits became commitments.


This is how the [Squash
Workflow](https://steveklabnik.github.io/jujutsu-tutorial/real-world-workflows/the-squash-workflow.html)
works in Jujutsu. It revolves around the idea of creating a new empty
revision, together with its description, even before touching any
file. Not a coincidence that the command to do this is `jj
commit`. The commit message as the intent and the unit of work.

First you write it, then you make it true.

# What Happens When You Do It

In 2012, with my teammates we started applying this technique, with
Git. It was a matter of doing 

```
git commit --allow-empty -m <message>
```

followed by 

```
git commit --amend
```

when we were done. Some Git GUIs makes this easy. Today I still do
this, with jj.

Over the years, this is what I experienced:

- **It aligns pair members**. Whether I am the driver or the
  navigator, I must agree with my pair on the message and on the
  goal. This stimulates a conversation. The moment we start
  programming, we genuinely agree on what we want to achieve.  
  When I code alone, I have to find an agreement with my own
  understading. It's equally challenging and it equally pays off.

- **It's easier to focus**: tests and commit message are the coding
  session's center of gravity. When the conversation or my thougths
  digress, the promise in the commit message is there to bring me back to the point.  
  I'm less prone to lose focus.

- **It sets a micro scope**: Before I used to ask myself: "*Should I
  stop coding now? How much is enough? Am I done yet?*".  
   Checking if I reached the planned goal is just easier.

- **Simpler commit reviews**: I always review my changes before making
  them final. Instead of asking "*Well well well, let's find out, what
  have I done here? I don't even remember...*" I can ask myself "*Have
  I done all and only[^1] what I committed to?*".  
  It's a much easier and targeted question to answer. 
  

- **More accurate and faithful statements**: if I have to write the
  message before coding, I cannot help but beying very specific. It's
  easy to refrain from being vague and writing `Fix` or `Update
  Repository.cs`. The message and the resulting commit content
  naturally match, because the message was the goal since the very
  beginning.

- **Each message triggers a micro design session**: the mere act of
  defining the commit message requires reaching an agreement with my
  pairing partner. We need to concord on the words to describe the
  desired behaviour, on the names to give to domain concepts and,
  ultimately, on which component owns the behaviour. These are all
  design questions, and we cannot write the message until we've
  addressed them.  
  Starting a pairing session, there's nothing nicer than discussing
  about design.

- **It creates a natural timebox**: I make my best to commit to
  baby-step progresses, from a stable state to the next little
  goal. It's easy to verify if a chance belongs or not to the goal.

- **It goes well with short-lived feature branches**: messages define
  micro-goals, and they live in the context of the wider goal defined
  by the branch name. Which is also pre-emptive, by design.  
  These 2 levels of goals help me steering the direction.

- **The commit history gains natural granularity**. Each commit has a
  single goal (one could say it respects the Single Responsibility
  Principle). A feature branch becomes a readable sequence of
  behaviours, each commit a small step in a larger story.  
  A natural changelog emerges.

***

This experiment was started in 2012 by Arialdo Martini, Mattia
Piccinetti, Gian Marco Gherardi, Guglielmo Brasile, Francesco
Pichierri, and Giuseppe Mariano. It was initially described in
[Pre-emptive Commit Comments][pre-emptive-commit-comments].  
It is described in details in the book [Git
Essentials][git-essentials] by [Ferdinando Santacroce][nando].


[jujutsu]: https://docs.jj-vcs.dev/latest/
[conventional-commits]: https://www.conventionalcommits.org
[pre-emptive-commit-comments]: https://arialdomartini.wordpress.com/2012/09/03/pre-emptive-commit-comments/
[git-essentials]: https://www.goodreads.com/book/show/25533645-git-essentials
[nando]: https://jesuswasrasta.com/
[3-rules]: http://www.butunclebob.com/ArticleS.UncleBob.TheThreeRulesOfTdd

# References

* [Jujustu][jujutsu]
* [Robert Martin - The Three Rules of TDD][3-rules]
* [Conventional Commits][conventional-commits]
* [Pre-Emptive Commit Comments][pre-emptive-commit-comments]
* [Ferdinando Santacroce][nando]
  * [Git Essentials - Ferdinando Santacroce][git-essentials]

[^1]: This "all and only" reminds me of the [3rd rule of TDD][3-rules]: I am not allowed to write any more production code than is sufficient to pass the one failing unit test.

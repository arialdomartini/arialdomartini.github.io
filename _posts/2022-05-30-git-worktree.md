---
layout: post
title: "git worktree"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- Git
---
[git stash][git-stash] is very convenient to quickly *switch* to another branch without loosing your changes.<br/>
What if you wish to browse *both* your changes *and* another branch?

<!--more-->
# Temporary Branch
If you need to temporary switch to another branch, and you don't want to loose the outstanding work, you might think of creating a temporary branch:

```bash
git checkout -b temp
git add .
git commit -m "WIP restart from here"
git checkout another-branch
```

When you are done with `another-branch`, you can switch back to the original branch, and `cherry-pick` from `temp`.<br/>
You will need to remember using `--amend` in your next commit, and to eventually removing the temporary branch:

```bash
git checkout original-branch
git cherry-pick --no-commit temp
git branch -D temp
```

That's a mouthful.<br/>
And it also has some annoying limitations: when back in the original branch, all the unstaged changes end up being staged, which is probably not what you wanted.


# git stash
[git stash][git-stash] does the same, saving a lot of keystrokes:

```bash
git stach push -um "WIP restart from here"
...
git stash pop -u 0
```

The option `-u` even restores the unstaged files as they were left.

So, `stash` is often the best option for switching back and forth between branches without loosing any pending changes.

What if wanted to *browse* your current working directory *and* another branch, at the same time?


# show and ls-tree
As a matter of fact, you don't need to *checkout* a branch or a commit to visit it. `git show` is already enough to display a branch content. For example,

```bash
git show B:your-file.txt
```

will display the content of `your-file.txt` at branch `B`.

You can `ls` a directory with `git ls-tree`

```bash
git ls-tree B:your-directory/
```
 
This is often enough for quick inspections.<br/> 
Most of the GUI tools let you browse the whole filesystem without checking it out. 
 
But more often than not, having a real checkout is what you need.


# Workdir
Starting from Git 2.5, one can actually have multiple branches checked out at the same time. This is a not so well known feature, which might come in handy for this specific case.

`worktree` allows you to create an arbitrary number of directories ("worktrees"), each checked out to an independent branch. Indeed, a *worktree* is like an additional Git clone, sharing the same `.git` directory of the main clone.


```bash
git worktree add ../another-branch
cd ../another-branch
git checkout B
```

If you enter your `another-branch`, you will see that it's an ordinary Git clone, with the notable exception that the `.git` directory is actually a text file, containing a reference to your original clone. 

```bash
$ cat .git
gitdir: /home/arialdo/prg/markdown/arialdomartini.github.io/.git/worktrees/another-branch
```

So, a working tree is a lighter alternative to cloning a repo twice.

The `git worktree` command comes with a bunch of convenient directives, whose use is straightforward: 

![The available options in git-worktree](static/img/git-worktree/git-worktree.png)

Interestingly, all those commands work from whatever worktree: you needn't be in the main one.

[git-stash]: https://www.git-scm.com/docs/git-stash

---
layout: post
title: "No Reason To Squash"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- git
---
**TL;DR**
* Even without squashing, **Pull Requests already include a squashed commit** 
* You can get an on-the-fly **squashed view of history** using `--first-parent`
* Squashing **would not save space**
* Squashing makes **git bisect less effective**
* Once squashed, details are lost forever
* Joining is easier than separating
* Squashing may promote **sloppy habits**
* Don't squash, **rebase**

<!--more-->

<!-- # TOC -->
<!-- * [Squash! 1-commit PRs are easier to review](#squash--1-commit-prs-are-easier-to-review) -->
<!--   * [Except that PR contains alreasy a squashed commit](#except-that-pr-contains-alreasy-a-squashed-commit) -->
<!-- * [Squashing makes history cleaner](#squashing-makes-history-cleaner)  -->
<!--   * [Except that you get the same, on-the-fly](#except-that-you-can-get-the-same--on-the-fly) -->
<!-- * [At least, squash after the PR merge! Nobody will need the single commits after](#at-least-squash-after-the-pr-merge--nobody-will-need-the-single-commits-after) -->
<!--   * [Except: good luck using git bisect](#except--good-luck-using-git-bisect) -->
<!-- * [Squashing cleans up WIP and temporary commits](#squashing-cleans-up-wip-and-temporary-commits) -->
<!--   * [Except that this promotes sloppy behaviors](#except-that-this-promotes-sloppy-behaviors) -->
<!-- * [Squashing saves disk space](#squashing-saves-disk-space) -->
<!--   * [Except it won't](#except-it-won-t) -->
<!-- * [I'm telling you: squash! Look how awful](#i-m-telling-you--squash--look-how-awful) -->
<!--   * [Except, there are saner workflows](#[except--there-are-saner-workflows) -->

# • Squash! 1-commit PRs are easier to review
If you squash, Pull Request reviewers will just have to read one single commit. Easier.

## ‣ Except that PR contains already a squashed commit 
You don't have to squash: the merge commit already contains the whole branch, squashed.

![The merge commit contains the whole branch, squashed](static/img/squash/merge-commit.png)


When the PR is **not squashed**, you can review **both** the final result **and** each single step. You can comment, amend, or exclude each single commit. Still, you can see the PR in one single, unified change from the merge commit.

On the contrary, if the PR is squashed, you just have the final result, and **all the single steps are lost forever**.<br/>
Hard to expect painstaking precision when details have been molten. 


# • Squashing makes history cleaner
A history like:

![Squashed git history](static/img/squash/squashed.png)

is cleaner than one displaying all the single commits of each pull requests:

![Not squashed git history](static/img/squash/not-squashed.png)


## ‣ Except that you can get the same, on-the-fly
You don't need to squash to hide details. Just use `--first-parent`.

![SmartGit's follow-only-first-parent option](static/img/squash/smartgit.png)

This works from the command line:

![result of git log --first-parent](static/img/squash/git-log.png)

and with some Git GUI clients such as [SmartGit][smartgit] and [Magit][magit]. Not all the Git frontends support `--first-parent`, though.

### Side note
"[T]he problem isn't the extra information: it's that the information isn't displayed in a way that shows them what they're interested in" ([David Chudzicki][david-chudzicki]). In other words, **making the history clean is mostly a matter of data *display*, not data *collection*.** You can store all the details and still be able to only show the merge commits.

That's why Git provides the option **`--first-parent`** in the first place.


<hr/>


# • At least squash after the PR merge! Nobody will need the single commits after
Ditto. Who cares?

## ‣ Except: good luck using git bisect
You will probably regret having squashed the history the next time you troubleshoot.

[git bisect][git-bisect] is your best friend when searching which commit introduced a bug: if the devs stick with the good habit of commiting early, often and small, `git bisect` will have all the chances to return you the very specific line of code containing the issue.<br/>

With squashed and large commits, you are left alone troubleshooting by hand.


<hr/>

# • Squashing cleans up WIP and temporary commits
The reviewer cares about the net effect of the PR, not about the half implemented commits, the broken ones that not even compile, the fixed typos, the amendments and the like.

## ‣ Except that this promotes sloppy behaviors
Don't commit broken code in the first place. 

Conscientous developers do review their work before submitting a pull request, and each and every of their commits builds, has green tests and is potentially deployable. 

Git offers the scrupulous developers all the tools for tidying up their commits

* `commit --amend` and `fixup` for amending commits
* `rebase --interactive` for deleting, reordering, squashing commits

There is really no excuse for pushing a pull request with not-compiling commits.

If the policy can be read as:

    Don't worry, no matter the mess, all your commits will be squashed into one
    
you can be sure that no one will break their backs for avoiding the mess.

I saw this happening: mandatory squashing rules eventually translated to tolerated sloppy habits.



<hr/>

# • Squashing saves disk space
If you don't squash, all those commits will knock Git down!

## ‣ Except they won't
Reducing Scala repository (38,098 commits) to one (1) single commit just saves 47% of space:

Try yourself:

```bash
repo=scala
squashed=${repo}-squashed

rm -fr ${repo} ${squashed}

git clone https://github.com/scala/${repo}.git
cd ${repo} && git gc --prune --aggressive
cd ..

mkdir ${squashed}
cd ${squashed}

git init
git fetch --depth=1 -n ../${repo}
git reset --hard $(git commit-tree FETCH_HEAD^{tree} -m "initial commit")

git gc --prune --aggressive

cd ..
du -sh ${repo} ${squashed}
```

![The whole Scala repository squashed to a single commit](static/img/squash/scala-squashed.png)




<hr/>
 

# • I'm telling you: squash! Look how awful 
All valid arguments. But the reality speaks for itself. That's the Scala repository, which **does not** use squashing:

<figure>
    <img src="static/img/squash/scala.png" />
    <figcaption>The <a href="https://github.com/scala/scala">Scala repository</a></figcaption>
</figure>


Look instead how Typescript went **from not squashing** (on the left) **to squashing** (on the right):

<figure>
    <img  style="width:100%" src="static/img/squash/typescript.png" />
    <figcaption>The <a href="https://github.com/microsoft/typescript">Typescript repository</a>, before and after squashing PRs</figcaption>
</figure>


# ‣ Except, there are saner workflows
In all honesty, if the alternative to squashing is having horrible Git histories like those, I'm all for squashing.

But there's a reason why they are so convoluted: in those repositories **PRs are merged without rebase**. When PRs are rebased before merging, the result is like the Haskell Cabal's repository:

<figure>
    <img src="static/img/squash/cabal.png" />
    <figcaption>The <a href="https://github.com/haskell/cabal">Cabal repository</a></figcaption>
</figure>


With a sane and disciplined workflow, it's not hard to have **both** all the details **and** a clean history.

But this deserves a separate article.

<!-- # delete -->
<!-- Squash, makes history shorter -->
<!-- Devs who need to read history would have an easier life: seeing PRs in their entirety is as easy as deep diving into the sigle change. -->


<!-- Not squashing devs  -->

<!-- * can read the merge commit if they want to have the overview -->
<!-- * or dive in each single commit, if the want the detail -->

<!-- Again, if the history is squashed, **details are lost forever**. -->


# References
* [Git bisect][git-bisect]
* [David Chudzicki - Git First-Parent-- Have your messy history and eat it too][david-chudzicki]
* [Syntevo.com - SmartGit][smartgit]
* [Magit][magit]

* Counter arguments:
  * [Microsoft - Squash: A Whole New Way to Merge Pull Requests](https://devblogs.microsoft.com/devops/squash-a-whole-new-way-to-merge-pull-requests/)
  * [stackexchange.com - Why squash git commits for pull requests?](https://softwareengineering.stackexchange.com/a/263172/121151)
  * [Scott Vandehey - Squashing Your Pull Requests](https://cloudfour.com/thinks/squashing-your-pull-requests/)


# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/17)


[david-chudzicki]: https://www.davidchudzicki.com/posts/first-parent/
[smartgit]: https://www.syntevo.com/smartgit/
[magit]: https://magit.vc/
[git-bisect]: https://git-scm.com/docs/git-bisect

---
layout: post
title: "A more reasonable approach to pre-baked .gitignore files"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- Git
most_read: true
---
Some tools and Git hosting services promote the addition of pre-baked `.gitignore` files to repositories at their creation. <br/>
GitHub, for example, offers the option to add long, 300+ lines, pre-compiled `.gitignore` files with a single click.

While initializing a repository with a README and a License is a very good idea, using pre-baked `.gitignore` files isn't. It violates YAGNI and it may cause puzzling situations.

A better alternative is a global `.gitignore` file.

<!--more-->
What's wrong with pre-baked Git ignore files?

## It's premature optimization
Pre-baked `.gitignore` files are appealing because they include out-of-the-box most of the possible tooling files name patterns.<br/>
The [GitHub's ignore file for C#](https://github.com/github/gitignore/blob/master/VisualStudio.gitignore), for example, is a 357 lines file, including very legit items, such as the compilation directories and the IDE's user specific files `*.suo` and `*.user`.

The problem is it also contains a lot of exoteric stuff such as:

{% highlight perl %}
# Chutzpah Test files
_Chutzpah*

# BeatPulse healthcheck temp database
healthchecksdb

# NVidia Nsight GPU debugger configuration file
*.nvuser
{% endhighlight %}

If you don't use the Nsight GPU debugger, that line is garbage, just like dead code. You might need it in the future, but probably not quite now. Adding it right now, just in case, is like a sort of premature optimization.

# It's garbage that can hurt you
Some items can even bite you: you might happen to create a namespace called `Express`, and then discover that your next commit lacks of all its class files, because they had been happily ignored by  the pre-baked `.gitignore` including:

{% highlight perl %}
# Installshield output folder
[Ee]xpress/
{% endhighlight %}

It doesn't help that you were not using InstallShield at all: you probably jut didn't notice that line, as you didn't write it.

# What's the alternative?
I tend to honor the XP's [YAGNI principle](https://en.wikipedia.org/wiki/You_aren%27t_gonna_need_it) and add items only when necessary: it's much less likely to incur in similar suprises, and it gets to dramatically smaller `.gitignore` files.

## Ok, but what about tooling files?
The problem with YAGNI and with adding items incrementally comes with editor and tooling files: it's just too annoying to add all of them over and over.<br/>
The solution is to distinguish between the 2 categories of tools:

* Project tools
* Developer tools

### Project tools
If the project uses a particular tool (such as npm, pip and Docker) or a particular executing environment (such as Java and Python), it is just ok to include its specific items in `.gitignore`.<br/>
It makes sense to add them *contextually* to the other tool's files: for example, the first moment `package.json` file is added, it makes sense to contextually add `node_modules` to `.gitignore`; the moment you add InstallShield to your project, you add a line with `Express/` in `.gitignore`.

The change will be self-contained, documented, reversable and well tracked in a commit.<br/>
That simple.

### Developer tools
This should not apply to tools the project does not depend on, but which are a personal choice of one or more developers.

In fact, it's much less understandable that a `.gitignore` file mentions Vim backup files only because one of the developers is a Vim enthusiast. It would be nice to keep this information in that specific developer's environment.

While the Project tools are just the same for all the developers, the CI infrastructure and the like, the tools *I* personally use, as a developer, on my personal box, are probably different than the ones *you* use. My ignore list would include settings for Emacs. So, if I ever happen to collaborate with you on one of your projects, I will probably need to add:

{% highlight perl %}
# Emacs backup files
\#*
*~
{% endhighlight %}

to the project's `.gitignore` file.<br/>
Another developer would probably like to add

{% highlight perl %}
globalconfig.crunch.v3.xml
_NCrunch_*\*.*
*.v3.ncrunchsolution.user
nCrunchTemp_*
*.v3.ncrunchsolution
*.v3.ncrunchproject
{% endhighlight %}

because they run tests with NCrunch.<br/>
A last one would make a PR adding

{% highlight perl %}
*.tmlanguage.cache
*.tmPreferences.cache
*.stTheme.cache
{% endhighlight %}

because they are SublimeText fans.

I'm sure you wouldn't love to see your `.gitignore` file treated like a dustbin.

It would be nicer to store this information as a personal setting.

There are 2 solutions to this:

#### The global `.gitignore`
Use `~/.gitconfig`: it's is your personal file, in your personal home directory. It's not part of any projects (besides maybe your dotfiles repository). You can store whatever information you like in it, without affecting other developers.

`~/.gitconfig` cannot directly contain ignored items. But it can reference git ignore file. Just add:

{% highlight perl %}
[core]
excludesfile = ~/.gitignore-global
{% endhighlight %}

The file `~/.gitignore-global` can be edited just like an ordinary `.gitignore` file, but its directives will be applied *globally* to every repositories in your local environment. It's the right place for your exoteric and personal ignore directives.

You will probably wish to track that file down in a personal [dotfiles repository](https://github.com/arialdomartini/dotfiles/blob/master/.gitignore-global).

#### The `exclude` directory
Otherwise, just add items to:

{% highlight perl %}
.git/info/exclude
{% endhighlight %}

This is less general than a global ignore file, as it is specific to a single repository, but it can be very handy with some corner cases.

For example, say that you want to add a text file for taking notes in a project. Just do:

{% highlight perl %}
touch notes.txt
echo notes.txt >> .git/info/exclude
{% endhighlight %}

Neat, isn't it?


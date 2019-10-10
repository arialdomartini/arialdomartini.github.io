---
layout: post
title: An alternative to pre-baked .gitignore files
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- Git
---
Some tools and Git hosting services add pre-baked `.gitignore` files to repositories at their creation. <br/>
GitHub, for example, lets the developer add long, 300+ lines, pre-compiled `.gitignore` files with a single click.

While initializing a repository with a README and a License is a very good idea, using pre-baked `.gitignore` files isn't.

A better alternative is a global `.gitignore` file.

<!--more-->
# What's wrong with pre-baked Git ignore files?
They contain too much information.

Pre-baked `.gitignore` files are appealing because they are configured out-of-the-box with most of the possible tooling files names.<br/>
The GitHub's ignore file for C#, for example, is a 353 lines file, including obvious and very legit items such as the compilation directories and IDE's user specific files such as `*.suo` and `*.user`.

It also contains a lot of exoteric stuff such as:

{% highlight perl %}
# Chutzpah Test files
_Chutzpah*

# BeatPulse healthcheck temp database
healthchecksdb

# NVidia Nsight GPU debugger configuration file
*.nvuser
{% endhighlight %}

If you don't use the Nsight GPU debugger, that line is garbage, just like dead code.

Some items can even bite you: you might happen to create a namespace called `Express`, and then discover that all its class files will be happily ignored because the pre-baked `.gitignore` file includes:

{% highlight perl %}
# Installshield output folder
[Ee]xpress/
{% endhighlight %}

It doesn't help if you are not using InstallShield at all.

I tend to honor the XP's [YAGNI principle](https://en.wikipedia.org/wiki/You_aren%27t_gonna_need_it) and add items only when necessary: it's much less likely to incur in similar suprises, and it gets to dramatically smaller `.gitignore` files.

## Tooling files

The problem comes with editor and tooling files: it's just annoying to add all of them over and over. <br/>
The pre-baked ignore files try to be complete, so they include the tooling files names for as many editors as possible.

If the project uses a particular tool (such as npm), it is ok to include npm specific items in `.gitignore`.

It's less understandable that a `.gitignore` file mentions Vim backup files only because one of the developers is a Vim enthusiast. This should be considered a personal setting. It would be nice to keep this information in that specific developer's environment.

The tools I use are probably different than the ones you use: my ignore list would include settings for NCrunch, IntelliJ and Emacs. If I ever happen to collaborate with you on one of your projects, I will probably need to add:

{% highlight perl %}
# Emacs backup files
\#*
*~
{% endhighlight %}

to your `.gitignore` file, because I use Emacs. Other developers would probably like to add statements for VisualStudio Code, Vim and whatever exoteric tool is pollutting their local clone. I'm pretty sure you wouldn't love to see your `.gitignore` file a dustbin.

It would be nicer to store this information as a personal setting.

### The global `.gitignore`
A convenient approach is to add:

{% highlight perl %}
[core]
excludesfile = ~/.gitignore-global
{% endhighlight %}

to `~/.gitconfig`.<br/>
The file `~/.gitignore-global` is an ordinary `.gitignore` file whose directives are globally applied to every repositories in your local environment.

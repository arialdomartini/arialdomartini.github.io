---
layout: post
title: "Don't get fooled: git cherry-pick doesn't move commits"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- git
most_read: true
---
`git cherry-picks` seems to be detaching a commit from its parent and to be attaching it to a new parent. But this is just an illusion: as a matter of fact, `cherry-pick` moves patches, not commits. That's why cherry-picking a commit you can have conflicts.

Actually, it's simpler that it seems. Bear with me.

<!--more-->

Say you have this repo:

{% highlight bash %}
A-B-C
   \
    X
{% endhighlight %}

By running:

{% highlight bash %}
git diff B X
{% endhighlight %}

you would get all the changes occurred between `B` and `X`, that is all you did as a programmer to get from `B` to `X`. Say, for example, you added the function `Foo()` to file `bar.js`.<br/>
Well:
       
{% highlight bash %}
git diff B X
{% endhighlight %}

would show a representation of your development activities, something like:

{% highlight bash %}
index 703df5e..f739c6b 100644
+++ b/bar.js
@@ -10,7 +10,7 @@ some very important changes;
       
+    Foo(x, y, y) => Console.log("whatever");
{% endhighlight %}

Let's call this development activity a *patch*.
       
Now, a commit is a pointer to a tree object, which in turn is a representation of the whole content of the project. So, if the commit `B` is the whole representation of your project *before* your editing, and `X` is the representation of your project *after* your editing, we could say that the line `\` between `B` and `X` can be seen as the diff above, that is, the changes that you, as a developer, did on top of `B` to get to `X`.
       
This should already give you a hint: the line between `B` and `X` is the *patch*. And `git cherry-pick` applies the *patch* to another commit.
       
Let's see this in detail. Let's perform:
       
{% highlight bash %}
git checkout C
git cherry-pick X
{% endhighlight %}

With these commands, it's like the following conversation occurs:
       
**You**: Git, go to commit `C`<br/>
**Git**: ok. I will forget all about the current situation, and time-travel to `C`, whatever it contains.<br/>
**You**: Now, take into consideration the last development activities I did to get to commit `X`, that is, the line `\` between `B` and `X`; in other words, take the `patch` that gets to `X`<br/>
**Git**: Ok, you mean when you added the function `foo()` to `bar.js`?<br/>
**You**: Exactly! Just do the same, now. Save me from retyping that once again. Do it for me: please add the function `foo()` to `bar.js`, just like I did on `X`<br/>
**Git**: Ok, I'm trying<br/>
                               
It should be clear that:
                               
* Git will *try* to re-type what you typed when you added `foo()`, but it may easily fail. The development activities you are asking git to repeat might have no sense at all on the current commit.<br/>In our example, the command "*add the function `foo()` to `bar.js`, just like I did on `X`*" might fail for a number of reasons. The simplest that come to mind are:
                                   
  * there is no file `bar.js` in commit `C`
                                           
  * there is already a function `foo()` at the same line
                                                   
* In case of failure, Git will ask for your intervention. That's the conflict you might see.
                                                       
* The commit you get after a `git cherry-pick` is not the old commit detached and moved to a different parent. It might be completely different! As a matter of fact, you are just "adding `foo()`" on top of an arbitrary commit, with an arbitrary content. Why should be the cherry-picked `X'` be equal to `X`? All we know is that it will (probably) end-up containing the function `foo()`.
                                                           
* `cherry-pick` moves patches, not commits. It might help reading it as a *git, please repeat the development activities I did to get to commit `X` independently to the current content*.

---
layout: post
title: "How to make the MiniShift's admin user a cluster administrator"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- OpenShift
- Docker
---
You might have noticed that after a fresh MiniShift installation the `admin` user cannot see system projects
`openshift`, `openshift-infra`, `kube-public`, `kube-system` and `default`,
so, trying to switch to one of them results in an error:

{% highlight bash %}
$ oc login -u admin -p admin
$ oc project openshift
error: You are not a member of project "openshift".
You have one project on this server: My Project (myproject)
To see projects on another server, pass '--server=<server>'.
{% endhighlight %}
<!--more-->

The `admin` console appears sadly empty:

![primitives are sealed classes](static/img/openshift-admin/before.png)

Trying to assign the `cluster-admin` role to `admin` also results in an error as well:

{% highlight bash %}
$ oc adm policy add-cluster-role-to-user cluster-admin admin --as=system:admin
Error from server (Forbidden): User "admin" cannot "impersonate" "systemusers.user.openshift.io" with name "system:admin" in project ""
{% endhighlight %}

It turns out the previous command is right, but it needs to be run with the `system:admin` user:

{% highlight bash %}
$ oc login -u system:admin
$ oc adm policy add-cluster-role-to-user cluster-admin admin --as=system:admin
cluster role "cluster-admin" added: "admin"
{% endhighlight %}

The console will become:

![primitives are sealed classes](static/img/openshift-admin/after.png)

and `admin` will be happily able to switch to any projects:

{% highlight bash %}
$ oc login -u admin -p admin
$ oc project openshift
Now using project "openshift" on server "https://192.168.64.11:8443".
{% endhighlight %}

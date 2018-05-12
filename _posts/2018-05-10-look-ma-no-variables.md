---
layout: post
title: Look ma, no variables!
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- SICP
- Lisp
- Scala
- C#
---

[SICP](https://mitpress.mit.edu/sites/default/files/sicp/index.html) doesn't cease to amaze me. Here's a challenge for you to solve, which I found in the chapter *Building Abstractions with Data*. I will provide a solution in C#.

The quiz is: 

> Would you be able to implement a 2-tuple using neither variables nor fields, only leveraging functions and lambdas?

I mean, you should not write anything like:

{% highlight csharp%}
class MyTuple
{
    public int First { get; set; }  // Don't use properties!
    private int _second;            // Don't use fields!
    
    static int a;                   // don't use static fields neither!
    
    public int Foo()
    {
        var a = ...                 // do not use variables!
    }
...
{% endhighlight %}

Seems impossible at first, isn't it?


<!--more-->
# 2-tuple

A 2-tuple, or pair, is a data structure that holds 2 values. For example, a pair of integers is easily obtained in C# with:

{% highlight csharp %}
var pair = new Tuple<int, int>(10, 20);
{% endhighlight %}

It is equally easy to retrieve the values with:

{% highlight csharp%}
var first = pair.Item1;
var second = pair.Item2;
{% endhighlight %}

or even better, using pattern matching and tuple deconstruction, with:

{% highlight csharp%}
(var first, var second) = pair;
{% endhighlight %}

## Build a 2-tuple yourself
I bet you can see how easy would be to sum 2 pairs of integers. I'm also sure you could easily write a custom implementation of pair of integers, may be storing the 2 integer values in 2 class fields.

The challenge is: implement one, but use no variable and no class field. Only rely on functions.<br />

You should pass the following tests:

{% highlight csharp %}
public class MyTupleIntTest
{
  [Fact]
  public void should_retain_values()
  {
    var tuple = MyTuple.Build(44, 100);

    tuple.First().Should().Be(44);
    tuple.Second().Should().Be(100);
  }

  [Fact]
  public void should_add_tuples()
  {
    var tuple1 = MyTuple.Build(2, 5);
    var tuple2 = MyTuple.Build(100, 50);

    var result = tuple1.Add(tuple2);

    result.First().Should().Be(102);
    result.Second().Should().Be(55);
  }
}
{% endhighlight %}

# Hint #1

The pair must retain the 2 integer values, somehow or other. And in order to store a value you need a variable, a class field or a property, don't you?

Actually, there are other means for storing a value and retrieving it later. You could use functions. Follow this example, it might give you a suggestion. Take this function:

{% highlight csharp%}
Func<string> revealSecret = delegate(string message)
     {
        return $"My name is James Bond and I tell you: {messagge}";
     };
{% endhighlight %}

or, with a lambda:

{% highlight csharp%}
var secretRevealer = (message) =>  $"My name is James Bond and I tell you: {message}.";
{% endhighlight %}

Since in C# functions are first-class citizens, you might use `secretRevealer` as a method's return value:

{% highlight csharp%}
Func<string, string> GetSecretRevealer()
{
  var secretRevealer = (message) =>  $"My name is James Bond and I tell you: {message}.";
  
  return secretRevealer;
}
{% endhighlight %}

Doing so, when you can call `GetSecretRevealer()` you get back a function that you can later invoke, which in turn will provide you with a string.

{% highlight csharp%}
var secretRevealer = GetSecretRevealer();
  
var secret = secretRevealer("secret message");
// "My name is James Bond and I tell you: secret message."
{% endhighlight %}

So far, nothing special.<br />
Enter closures. What if we change the method like the following:

{% highlight csharp%}
Func<string, string> GetSecretRevealer()
{
  var name = "James Bond";
  return (message) =>  $"My name is {name} and I tell you: {message}";
}
{% endhighlight %}

When you invoke `GetSecretRevealer()`, the variable `name` is undefined, because we are outside its scope (`GetSecretRevealer`'s body'). Yet, its value is somehow retained. In fact:

{% highlight csharp%}
GetSecretRevealer()("this is a secret message");
{% endhighlight %}

returns `My name is James Bond and I tell you: this is a secret message`.

Does it give you any suggestion?

# Hint #2
Ok, let's try with this. What if we convert the variable `name` to a method's parameter? See the following:

{% highlight csharp%}
Func<string, string> GetSecretRevealer(string name)
{
  return (message) =>  $"My name is {name} and I tell you: {message}";
}
{% endhighlight %}

or, more concisely, as expression-bodied member:
{% highlight csharp%}
Func<string, string> GetSecretRevealer(string name) =>
  (message) =>  $"My name is {name} and I tell you: {message}.";
{% endhighlight %}

Now, there are no variables at all in `GetSecretRevealer`. Yet, the value passed in input is retained, just like the variable `name` in the previous example. So, when the returned function is invoked, the value can be retrieved again:

{% highlight csharp%}
var secretRevealer = GetSecretRevealer("James Bond");

// ...

var result = secretRevealer("some message");
// "My name is James Bond and I tell you: some message."
{% endhighlight %}


The value of `name` has been captured by the closure `GetSecretRevealer`. You could leverage this fact to store values, and use closures to build data structures, without using variables nor fields.


# Solution
Here we go:

{% highlight csharp %}
static class MyTuple
{
    static Func<Func<int, int, int>, int> Build(int a, int b) => f => f(a, b);

    static int First(this Func<Func<int, int, int>, int> tuple) => tuple((a, _) => a);
    
    static int Second(this Func<Func<int, int, int>, int> tuple) => tuple((_, b) => b);
    
    static Func<Func<int, int, int>, int> Add(
        this Func<Func<int, int, int>, int> a, 
        Func<Func<int, int, int>, int> b) => Build(a.First() + b.First(), a.Second() + b.Second());
}
{% endhighlight %}

# Explaination

It's simpler than you think once you wrap your head around the idea of functions that returns function that takes functions as parameters, and the like.

Let's start from `Build`: `Build`'s goal is to create a pair of integers, so it must retain the value of `a` and `b`. If `Build` could speak, it would say:

> Invoke me, and pass me 2 integers. I will return you back a magic closure, that will capture those two integers.<br />
> Store that magic closure and whenever you want those 2 integers back, invoke it.<br />
> I cannot give you back the 2 intgers as a tuple, since we deliberatelly decided to only rely on functions. Here's a trick we might use: pass the magic closure a function, and the closure will invoke it passing it exactly the 2 integers you need..

What the functions we should pass the magic closure might look like?<br />
Let's talk about the function to get the first value. It should be like:

{% highlight csharp%}
int FirstOfTwoParameters(int a, int b)
{
    return a;
}
{% endhighlight %}

If it was a lambda, it would be:

{% highlight csharp%}
Func<int, int, int> (a, b) => a;
{% endhighlight %}

The function to get the second value would be:

{% highlight csharp%}
Func<int, int, int> (a, b) => b;
{% endhighlight %}

You should recognise their body in:

{% highlight csharp %}
static class MyTuple
{
    static int First(...) => tuple((a, _) => a);
    
    static int Second(...) => tuple((_, b) => b);
}
{% endhighlight %}

where `_` is used to signify that particular value won't be used;

If the magic closure receives `first` or `second`, all is has to do is to invoke it and return back the result. The magic closure is then:

{% highlight csharp %}
int MagicClosure(Func<int, int, int> f) => f(a, b);
{% endhighlight %}

The signature of `MagicClosure` is:

{% highlight csharp %}
Func<Func<int, int, int>, int>
{% endhighlight %}

that is:


{% highlight csharp %}
Func<                    // I am a function
   Func<int, int, int>,  // Provide me the lambdas first or second
   int                   // and I will invoke it and return its result
>
{% endhighlight %}

`Build` should get `a` and `b` and return the Magic Closure:

{% highlight csharp %}
static Func<Func<int, int, int>, int>   // the Magic Closure's signature
    Build(int a, int b) =>                     // it gets the 2 integers
      f => f(a, b);                            // and returns the Magic Closure
{% endhighlight %}

Now, we are almost done. We can define `First` and `Second` as extension methods of the Magic Closure.

Now, think about it: the Magic Closure **is** our tuple. We get it from `Build`. We can invoke `First` and `Second` on it. Regardless it is implemented with functions, it is in fact a data structure.

To demonstrate it, let's implement a sum of tuples.


{% highlight csharp %}

  static Func<Func<int, int, int>, int>  // returns a tuple
     Add(
    this Func<Func<int, int, int>, int> a,      // it gets two tuples
    Func<Func<int, int, int>, int> b)
        => Build(a.First() + b.First(),         // it builds a new tuple
                 a.Second() + b.Second());
{% endhighlight %}

To reason about the hard-to-read signature, whenever you see `Func<Func<int, int, int>, int>` you should read `MyTuple`. Doing so, the above would read as:

{% highlight csharp %}
static MyTuple Add(this MyTuple a, MyTuple b)
      => Build(a.First() + b.First(), a.Second() + b.Second());
{% endhighlight %}

Straighforward, isn't it? And actually, you could simplify the whole implementation, using a `delegate` to alias `Func<Func<int, int, int>, int>`:

{% highlight csharp %}
delegate int FirstOrSecond(int a, int b);
delegate int MagicClosure(FirstOrSecond firstOrSecond);
    
static class MyTuple
{
    static MagicClosure Build(int a, int b) => f => f(a, b);

    static int First(this MagicClosure tuple) => tuple((a, _) => a);
    
    static int Second(this MagicClosure tuple) => tuple((_, b) => b);
    
    static MagicClosure Add(
        this MagicClosure a, 
        MagicClosure b) => Build(a.First() + b.First(), a.Second() + b.Second());
}
{% endhighlight %}

And since the `Magic Closure` is our tuple, we can just rename it to `Tuple`:

{% highlight csharp %}
delegate int F(int a, int b);
delegate int Tuple(F f);
    
static class MyTuple
{
    static Tuple Build(int a, int b) => f => f(a, b);

    static int First(this Tuple tuple) => tuple((a, _) => a);
    
    static int Second(this Tuple tuple) => tuple((_, b) => b);
    
    static Tuple Add(this Tuple a, Tuple b) => 
        Build(a.First() + b.First(), a.Second() + b.Second());
}


var tuple1 = MyTuple.Build(2, 5);
var tuple2 = MyTuple.Build(100, 50);

var result = tuple1.Add(tuple2);

result.First().Should().Be(102);
result.Second().Should().Be(55);
{% endhighlight %}


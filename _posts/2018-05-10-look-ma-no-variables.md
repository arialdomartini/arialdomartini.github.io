---
layout: post
title: Look ma, no variables!
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- SICP
- Lisp
- Scala
- C#
most_read: true
---

[SICP](https://web.mit.edu/6.001/6.037/sicp.pdf) doesn't cease to amaze me. Here's a challenge for you to solve, which I found in the chapter *Building Abstractions with Data*. I will provide a solution in C#.

The quiz is: 

> Would you be able to implement a 2-tuple (a pair) of integers, only leveraging functions and lambdas, without using neither variables nor class fields?

<!--more-->

I mean, you should store 2 integers in a tuple, without writing anything like the following:

{% highlight csharp %}
class MyTuple
{
    public int First { get; set; }  // Don't use properties!
    private int _second;            // Don't use fields!
    
    static int a;                   // don't use static fields neither!
    
    public int Foo()
    {
        var a = ...                 // do not use variables!
    }

{% endhighlight %}

At first, it seems impossible, isn't it?

# The tests to pass

A 2-tuple, or pair, is a data structure that holds 2 values. For example, a pair of integers is easily obtained in C# with:

{% highlight csharp %}
var pair = new Tuple<int, int>(10, 20);
{% endhighlight %}

It is equally easy to retrieve the original values with:

{% highlight csharp%}
var first = pair.Item1;
var second = pair.Item2;
{% endhighlight %}

or even better, using pattern matching and tuple deconstruction, with:

{% highlight csharp%}
(var first, var second) = pair;
{% endhighlight %}

In this quiz I assume a wider definition for tuple: anything we can provide 2 integers and that is able to give them back when we invoke the functions `First` and `Second` on it. It should also be possible to sum two tuples.

In other words, the structure to build should pass the following tests:

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

I'm sure you could easily write a custom implementation of pair of integers using classes, may be storing the 2 integer values in 2 class fields. The challenge is: implement a purely-functional version, with no variable and no class field. Only rely on functions.

# Hint #1

Somehow or other, the pair must retain the 2 integer values. In order to store a value you need a variable, a class field or a property, don't you? Actually, there are other means for storing a value and making it available for a later use. You can use functions. The following example might give you a suggestion. Let's start from this function:

{% highlight csharp%}
Func<string> revealSecret = delegate(string message) {
    return $"My name is James Bond and I tell you: {messagge}";
};
{% endhighlight %}

or, with a lambda:

{% highlight csharp%}
var secretRevealer = (message) =>  $"My name is James Bond and I tell you: {message}.";
{% endhighlight %}

Its type is:

{% highlight csharp%}
Func<string, string>
{% endhighlight %}


Since in C# functions are first-class citizens, you might use `secretRevealer` as a method's return value:

{% highlight csharp%}
Func<string, string> GetSecretRevealer()
{
  var secretRevealer = (message) =>  $"My name is James Bond and I tell you: {message}.";
  
  return secretRevealer;
}
{% endhighlight %}

Doing so, when you can call `GetSecretRevealer()` you get back a function that you can later invoke, which in turn will return a string:

{% highlight csharp%}
var secretRevealer = GetSecretRevealer();
  
var secret = secretRevealer("secret message");
// "My name is James Bond and I tell you: secret message."
{% endhighlight %}

So far, nothing special.<br />
Enter closures. What if we introduce a variable in `GetSecretRevealer`'s body?

{% highlight csharp%}
Func<string, string> GetSecretRevealer()
{
  var name = "James Bond";
  return (message) =>  $"My name is {name} and I tell you: {message}";
}

var secretRevealer = GetSecretRevealer();

secretRevealer("this is a secret message");
// "My name is James Bond and I tell you: this is a secret message."
{% endhighlight %}

The moment `secretRevealer` is invoked, the variable `name` is undefined, because we are outside its scope. Yet, its value is somehow retained, as it is present in the resulting string. This is because the function returned by `GetSecretRevealer` is a closure: it has captured the variable `name` value.

Does it give you any suggestion?

# Hint #2
Ok, let's try with this. What if we convert the variable `name` to a method's parameter?

{% highlight csharp%}
Func<string, string> GetSecretRevealer(string name)
{
  return (message) =>  $"My name is {name} and I tell you: {message}";
}
{% endhighlight %}

or, more concisely, using an expression-bodied member:
{% highlight csharp%}
Func<string, string> GetSecretRevealer(string name) =>
  (message) =>  $"My name is {name} and I tell you: {message}.";
{% endhighlight %}

It still works. Although there are no variables at all in `GetSecretRevealer`, the value passed in input is retained, just like the variable `name` in the previous example:

{% highlight csharp%}
var secretRevealer = GetSecretRevealer("James Bond");    // we provide a value
                                                         // and we get back a function

// ...

var result = secretRevealer("some message");             // when the function is invoked
// "My name is James Bond and I tell you: some message." // the original value is used
{% endhighlight %}


The value of `name` has been captured by the closure returned by `GetSecretRevealer`. You could leverage this fact to store values, and use closures to build data structures, with no need of variables and fields.


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
        Func<Func<int, int, int>, int> b) => 
            Build(a.First() + b.First(), a.Second() + b.Second());
}
{% endhighlight %}

# Explaination

It's simpler than you think once you wrap your head around the idea of functions that return functions that take functions as parameters.

Let's start from `Build`: `Build`'s goal is to create our purely-functional version of tuple of integers, so it must get the value of `a` and `b` in input. If `Build` could speak, it would say:

> Invoke me, and pass me the 2 integers you want to store. I will return you back a Magic Closure, that can capture those two integers.<br />
> Store that Magic Closure and whenever you want those 2 integers back, invoke it.<br />
> This is all I do. If you want to know what Magic Closure does, ask it.

Let's ask Magic Closure:

> I'm created by `Build` when you give it 2 integer values. Since I'm a closure, I can capture their values.
> Unfortunately, I cannot give you them back to you as a tuple, since we deliberatelly decided to only rely on functions. Here's a trick we might use: pass me a function, and I will invoke it providing it exactly the 2 integers you need as parameters.

Let's talk about the functions we should pass the Magic Closure: what should they look like?<br />
One of them can be a function to get the first value. Something like:

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

Simingly, the function to get the second value would be:

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

where `_` is used to signify that that particular value won't be used;

If the Magic Closure receives `first` or `second`, all it has to do is to invoke them passing them the 2 integers, and return back the result. The Magic Closure implementation is then:

{% highlight csharp %}
int MagicClosure(Func<int, int, int> f) => f(a, b);
{% endhighlight %}

The type of `MagicClosure` is:

{% highlight csharp %}
Func<Func<int, int, int>, int>
{% endhighlight %}

that we can explain as:


{% highlight csharp %}
Func<                    // I am a function
   Func<int, int, int>,  // Provide me a lambda like the function first
   int                   // and I will invoke it and return its result
>
{% endhighlight %}

`Build` should get `a` and `b` and return the Magic Closure:

{% highlight csharp %}
static Func<Func<int, int, int>, int>   // the Magic Closure's type
    Build(int a, int b) =>              // it gets the 2 integers
      f => f(a, b);                     // and returns the Magic Closure
{% endhighlight %}

Now, we are almost done. We can define `First` and `Second` as extension methods of the Magic Closure.

{% highlight csharp %}
static int First(this Func<Func<int, int, int>, int> tuple) => tuple((a, _) => a);
    
static int Second(this Func<Func<int, int, int>, int> tuple) => tuple((_, b) => b);
{% endhighlight %}

They invoke the Magic Closure, providing them the lambda to get the first value, and the lambda to get the second one.

Now, think about it: the Magic Closure **is** our tuple: we create it using a sort of factory method, `Build`, and we can invoke `First` and `Second` on it to get its internal state. In orher words, regardless if it is implemented with functions, it is in fact a data structure.

To demonstrate it, let's implement a sum of tuples.

{% highlight csharp %}

  static Func<Func<int, int, int>, int>     // returns a tuple
     Add(
    this Func<Func<int, int, int>, int> a,  // it gets two tuples
    Func<Func<int, int, int>, int> b)
        => Build(a.First() + b.First(),     // it builds a new tuple
                 a.Second() + b.Second());
{% endhighlight %}

To reason about the hard-to-read signature, you can mentally replace `Func<Func<int, int, int>, int>` with `Tuple`. Doing so, the above would read as:

{% highlight csharp %}
static int First(this Tuple tuple) => tuple((a, _) => a);
static int Second(this Tuple tuple) => tuple((_, b) => b);

static MyTuple Add(this MyTuple a, MyTuple b)
      => Build(a.First() + b.First(), a.Second() + b.Second());
{% endhighlight %}

Straighforward, isn't it? 

## Use a delegate alias

Actually, you can simplify the whole syntax using a `delegate` to alias `Func<Func<int, int, int>, int>`:

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

And since the `Magic Closure` is our tuple, we can really rename it to `Tuple`:

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

Notice how 

* `Build` acts just like a data structure constructor, or a class factory method;
* the delegate `Tuple` is a functional replacement of a class;
* the closures returned by `Build`, `First`, `Second` and `Add` are the functional equivalent of class instances.

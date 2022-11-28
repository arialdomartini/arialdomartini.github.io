---
layout: post
title: "Diff, don't drop"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- Testing
- TDD
- DB
---
Or: how I learnt to stop cleaning the DB after each integration test.

**TL;DR**

* **Cleaning database tables** after each integration test is **not stictly needed**
* It's often not even desirable
* Tests can be easily made **independent from the initial DB state** and **from each other**
* Just use **unique identifiers** and **avoid absolute asserts** 
* You can also play with ***Delta Assertions***

* Tests designed like this can be run **in parallel**
* They run **faster**
* And they are more resilient and realistic

<!--more-->
## The *clean-db-after-test* dogma
Like many others, over the years I have spent a sheer amount of energy figuring out how to properly clean up the database after each integration test. I've used all the classical techniques:

* leveraging **DB transactions**
* using in-memory DBs
* **truncating the tables**
  * (of course, **in the right order**, because of the foreign keys)
* manually **reverting** each change
* dropping and **recreating the database**
* Oh, yes! Always avoiding running **tests in parallel**.

That was rarely easy: some DBs do not support nested transactions, some fail dropping because of locks. In general, it's either a lot of infrastructure code, or relying on magic libraries. 

In recent months, I started pondering and experimenting a different perspective.<br/>
Which starts from a basic question.

## Why do we need to reset the DB back to a clean state in the first place? 
Well, why? Maybe we don't?<br/>
The classical answers are

* bacause otherwise tests would start from an **unknown, unpredictable state**
* because otherwise tests would **depend on each other**

Until recently, I never argued these assumptions, and I have always blindly observed the dogma. But, honestly speaking, I still have to find a very compelling justification.

### The classical answers as a smell of a design issue
I propose a different point of view, based on 2 observations:

**Starting from an unknown state is indeed a desirable trait**: if tests only work against either an empty or an artfully preconfigured database, they are designed to operate in an ideal condition which is never met in reality. In production our code will be faced with very different circumstances. And that's a pity, since Integration Tests were supposed to ["use the actual components that the app uses in production"][integration-in-asp].

**If tests depend on each other, maybe that's a test design issue we should solve, not circumvent**: after all, in the real world our production code \*is\* used in concurrency. Having tests that break when run in parallel is most likely a design flaw.<br/>`[CollectionDefinition(DisableParallelization = true)]` is not a fix: it's sweeping the dirt uder the carpet. 

In other words:

* The necessity of an empty DB is a strong and often unrealistic assumption.
* Fictional, overly-simplistic assumptions lead to covering fictional, overly-simplistic use cases. 
* The more production-like integration tests are, the better.

This bears the question: what if we design tests to run in parallel, independently from each other and from any arbitrary initial DB state?

The good news is: designing such tests is actually easier than one might think.

# Diff, don't drop
Basically, they just have:

* to use **random** or **unique identifiers**
* to assert based on differences ([Delta Assertions][delta-assertions]) rather than on absolute claims

## Show me the code
Before seing the benefits, here are a couple of examples. You can easily figure out other cases, and how to tackle them.

### Product
Say you have:

```csharp
void saves_a_product()
{
    DropAndRecreateDb();
    var product = new Product("Beer", 20);
     
    _repository.Save(product);
    var products = _repository.GetProducts();
     
    Assert.Equal(1, products.Count());
}
```

The problem with this test is apparent: it assumes the DB starts empty, and that it contains 1 single product when it eventually ends. We can make it independent from the other tests actually using the newly created id:

```csharp
void saves_a_product()
{
    var product = new Product("Beer", 20);

    _repository.Save(product);
    var onDb = _repository.GetProduct(product.Id);

    Assert.Equal(20, onDb.Price);
}
```

### Unique constraints
What if we cannot insert `"Beer"` twice, because there is a unique constraint? Instead of deleting the DB, we could make the test independent from the (arbitrary) initial state by generating random values:

```csharp
string RandomString => Guid.New().ToString();

void saves_a_product()
{
    var name = RandomString;
    var product = new Product(name, 20);

    _repository.Save(product);
    var onDb = _repository.GetProduct(product.Id);

    Assert.Equal(20, onDb.Price);
}
```

Libraries such as [AutoFixture][auto-fixture] and [Bogus][bogus] may come in handy.

Those are trivial examples, but I hope you get the point.

### Blog
Here's an other example, this time from [Microsoft Learn strong- Testing against your production database system][microsoft-testing].
 
```csharp
[Fact]
public void AddBlog()
{
    using var context = Fixture.CreateContext();
    context.Database.BeginTransaction();

    var controller = new BloggingController(context);
    controller.AddBlog("Blog3", "http://blog3.com");

    context.ChangeTracker.Clear();

    var blog = context.Blogs.Single(b => b.Name == "Blog3");
    Assert.Equal("http://blog3.com", blog.Url);
}
```

This test assumes the blog `"blog3"` does not exists when the test starts.<br/>
This is in itself interesting: 

* this overly-simplistic assumption does not help us discovering the use case of `AddBlog` failing because of a duplicated blog name
* the code testing the domain logic is annoyingly interleaved with the code handling the transaction
* the transaction itself only comes into play during testing; in production, it does not

We can improve on the design and designing the test to be more production-like, making it independent from initial state and from other tests simply playing with a random name:

```csharp
string RandomString => Guid.NewGuid().ToString();
string RandomUrl => $"http://{RandomString}.com");

[Fact]
public void AddBlog()
{
    var controller = new BloggingController(_context);
    var name = RandomString;
    var url = RandomUrl;
    controller.AddBlog(name, url);

    var blog = _context.Blogs.Single(b => b.Name == name);
    Assert.Equal(url, blog.Url);
}
```

Now, when the test ends, why should we feel immediately compelled to delete the created blog? The real user would not. Nor the real testers, in their manual exploratory testing activities.

On the contrary: in the wake of exercising the application like a real user would, we could get rid of the backdoor for directly accessing the DB, using the real API instead:


```csharp
[Fact]
public void AddBlog()
{
    var name = RandomString;
    var url = RandomUrl;
    
    var response = _testClient.Post("/api/posts/", new {Name = name, Url = url});

    var blog = _testClient.Get<Blog>($"/api/posts/{name}");
    
    Assert.Equal(url, blog.Url);
}
```


## The benefits?
You might like these tests for the good traits they exhibit:

* They **run faster**, because they skip the DB cleanup/rebuild phase
* They can run **in parallel**, just like an actual user would<br/>
  (Testing concurrency is per se a desirable side effect)
* They are inherently **more realistic and solid**, since they are closer to emulating what real users would do with an ever changing database content
* The same tests can be used in production-like environments, as the basis for Load and Stress Testing



## xUnit Test Patterns
The idea of skipping the teardown phase is mentioned in xUnit Test Patterns in [Avoiding the need of Teardown][avoiding-the-need-of-teardown] in the paragraph Avoiding Fixture Collitions. Here is a summary excerpt:

> We need to do fixture teardown for three reasons:
> 
> 1. The accumulation of leftover fixture objects  can cause tests to run slowly
> 2. The leftover fixture objects can cause the SUT to behave differently or our assertions to report incorrect results
> 3. The leftover fixture objects can prevent us from creting the Fresh Fixture required by our tests
> 
> [..]
> 
> The second issue can be addressed by using [*Delta Assertions*][delta-assertions] (page 485) rather than "absolute" assertions.
> The third issue can be addressed by ensuring that each test generates a different set of fixture objects each time it is run.
> Thus any object that the > test needs to reate must be given totally unique identifiers -- that is, unique filenames, unique keys, and so on.

### Delta Assertions
In turns, the idea of Delta Assertions is about "*specifying assertions based on differences between the pre- and post-exercise state of the SUT*"

> Before exercising the SUT, we take a snapshot of relevant parts of the Shared Fixture. 
> After exercising the SUT, we specify our assertions relative to the saved snapshot.
> The Delta Assertions typically verify that the number of objects has changed by the right number 
> and the contents of collections of objects have been augmented by the expected objects. 
> [..]
> We can use a Delta Assertion whenever we don't have full control over the test fixture 
> and we want to avoid Interacting Tests (see Erratic Test).
> Using Delta Assertions will help us make our tests more resilient to changes in the fixture

[Gerard Meszaros - xUnit Test Patterns][xunit-test-patterns]


## Conclusion
Tests speak and send feedbacks which are often worth to be listened and contemplated.<br/>
When testing is hard, it's often a sign of either a design problem with the production code, or a wrong approach to testing.

So far, I find this approach a simplification, only providing benefits with neglibigle drawbacks. I'm looking forward to getting feedback from other fellow developers.. 

# References

* [Gerard Meszaros - xUnit Test Patterns][xunit-test-patterns]
  * [Delta Assertions][delta-assertions]
  * [Avoiding the Need of Teardown][avoiding-the-need-of-teardown]

* [AutoFixture][auto-fixture]
* [Bogus][bogus]
* [Microsoft - Testing against your production database system][microsoft-testing]
* [Microsoft - Integration tests in ASP.NET Core][integration-in-asp]

[xunit-test-patterns]: http://xunitpatterns.com/index.html
[delta-assertions]: http://xunitpatterns.com/Delta%20Assertion.html
[avoiding-the-need-of-teardown]: http://xunitpatterns.com/Persistent%20Fixture%20Management.html
[auto-fixture]: https://autofixture.github.io/
[bogus]: https://github.com/bchavez/Bogus
[microsoft-testing]: https://learn.microsoft.com/en-us/ef/core/testing/testing-with-the-database?source=recommendations
[integration-in-asp]: https://learn.microsoft.com/en-us/aspnet/core/test/integration-tests?view=aspnetcore-7.0

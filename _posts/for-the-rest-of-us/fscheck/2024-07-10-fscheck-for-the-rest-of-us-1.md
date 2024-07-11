---
layout: post
title: "State Monad For The Rest Of Us"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- F#
- Functional Programming
include_in_index: false
---

<!--more-->
FsCheck provides 6 main functionalities:

1. It generates random data.
2. It provides a way to algebraically combine data generators.
3. It explores the domain space extending its boundaries.
4. It shrinks data when it finds a counter example.
5. It interacts with the testing framework to run multiple tests.
6. 

[AutoFixture][autofixture] example

```csharp
[Fact]
void IntroductoryTest()
{
    Fixture fixture = new Fixture();

    int expectedNumber = fixture.Create<int>();
    MyClass sut = fixture.Create<MyClass>();

    int result = sut.Echo(expectedNumber);

    Assert.Equal(expectedNumber, result);
}
```

> This example illustrates the basic principle of AutoFixture: it can
> create values of virtually any type without the need for you to
> explicitly define which values should be used.

[Oxygenize][oxygenize], a not anymore supported library, could be used to generate a batch for tests.


## References

* [Bogus][bogus]
* [AutoFixture][autofixture]
* [FakeItEasy][fakeiteasy]
* [NBuilder][nbuilder]
* [Oxygenize][oxygenize]
* [random-test-values][random-test-values]

[bogus]: https://github.com/bchavez/Bogus
[autofixture]: https://autofixture.github.io/
[fakeiteasy]: https://fakeiteasy.github.io/
[nbuilder]: https://github.com/nbuilder/nbuilder
[oxygenize]: https://github.com/kamil-mrzyglod/Oxygenize
[random-test-values]: https://github.com/RasicN/random-test-values

## Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/31)


{% include fp-newsletter.html %}

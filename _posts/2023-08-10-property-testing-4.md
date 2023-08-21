---
layout: post
title: "Property-based Testing For The Rest Of Us - 2"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- tdd
- functional programming
- property-based testing
---
## Index
1. [Utterly opinionated introduction to Property Testing](2023-08-10-property-testing.md)
2. [Shut up and code!](2023-08-10-property-testing-2.md)
3. [It's properties all the way down](2023-08-10-property-testing-3.md)
4. The Prime Factors Kata

If you apply the practices of TDD relying on Property-based Tests rather than on Example-based ones, what you get is *Property-driven Development* (PDD).

I claim that PDD leads to a more correct, safer and faster development experience than the example-based TDD. I will argument my bold statement through the implementation of a classical, simple coding kata.

## The Prime Factors Kata

I'm using the [ThePrimeFactorsKata][the-prime-factor-kata], a little exercise that Bob Martin has been employing since 2005 to demo TDD. We will first follow the classical approach, mirroring what Bob Martin does; then we will repeat the same using PBT, commenting on the differences.

The requirement is:

> Write a function that takes an integer and returns the collection of its prime factors;<br/>
> 
> Factors of a number are integers that,<br/>
> when multiplied together, result in the original number.
>
> A number is prime if it has no divisors other than 1 and itself

Bob Martin solves it with few tests (from 7 to 10, depending on the session). You can watch him in action in the video [The Three Laws of TDD (Featuring Kotlin)][the-three-laws-of-tdd].

## The classic TDD approach

Bob Martin's approach boils down to this iterative process:

1. He starts from the most extreme simplification of the requirement, recuced to the simplest possible, trivial, problem
2. He resolves the requirement with a minimal implementation, making sure the test is green
3. He asks himself if the implementation is complete
4. If is not, he extends the requirement adding the next logical use-case, and he starts over from 2.

Here's a summary of the steps he performs in the video.

### Step 1
The simplest possible requirement is: the list of the prime factors of `1` is empty.

Now, technically speaking, this is a mathematical nonsense: prime factorization is defined for numbers `n >= 2` (see [Prime Factorization - Wolfram Mathworld][prime-factorization-wolfram]), so Bob should have been started from `2`. But let me ignore that.

The requirement to implement is:

```kotlin
@Test fun factors() {
  assertThat(factorsOf(1), `is` (emptyList()))
}
```

which can be resolved with:

```kotlin
private fun factorsOf(n: Int): List<int> {
  return emptyList()
}
```

Here Bob Martin applies "*Fake it until you make it*", an idiomatic technique described by Kent Beck in his seminal "[Test-Driven Development By Example][tdd-by-example]" as:

> return a constant and gradually replace constants with variables 
> until you have the real code

We will see later how PBT improves on this.



### Step 2
After `1`, the logical step is to assert the prime factors of `2`:

```kotlin
@Test fun factors() {
  assertThat(factorsOf(1), `is` (emptyList()))
  assertThat(factorsOf(2), `is` (factors(2) = listOf(2)))
}
```

which is implemented with:

```kotlin
val factors = mutableListOf<int>()

if (n>1)
  factors.add(2)
  
return factors 
```

Basically, Bob Martin keeps adding the next assert, each time incrementing the number by `1`. That's the application of "*Triangulation*", an other technique described in Kent Beck's book, which is meant to be complementary to "*Fake it*".

### Step 3
Asserting that `factorsOf(3)` is `listOf(3)` takes to replacing `2` with `n`:


```kotlin
val factors = mutableListOf<int>()

if (n>1)
  factors.add(n)
  
return factors 
```


### Step 4.
`factorsOf(4)` is `listOf(2, 2)`

leads to

```kotlin
var remainder = n
val factors = mutableListOf<int>

if (remainder>1) {
  if(remainder % 2 ==  0)  {
    factors.add(2)
	remainder /= 2
  }
}
if(remainder > 1)
  factors.add(remainder)

return factors 
```

### Step 5
It turns out that this implementation is already fine for `5`.

### Step 6
Also with `factorsOf(6)`, the algorithm works.

Does it mean we are done?<br/>
We don't know. The test suite is green and technically this should mean there are no evidences of bugs.<br/>
But we also know we have proceeded by arbitrary &mdash; and probably simplistic &mdash; examples. 

The said truth is: we cannot trust the test suite, at this point. Not being led by tests, we cannot but use our brains and commonsense, and keep adding a further test.


### Step 7
Again, all the tests are green.<br/>
Damn. Shall we finally stop? Maybe yes, the algorithm is complete, and there are no bugs.<br/>
But there are! The algorithm will not work with `8`. 

### Step 8
`factorsOf(8)` should be `listOf(2, 2, 2)`. 

Finally, a red test! We found how to proceed!<br/>
We should consider ourselves very lucky: what if the tests kept being green until `30` or `150`? I'm sure many would have just stopped trying.

Here's the improved (I would say, fixed) implementation:

```kotlin
var remainder = n
val factors = mutableListOf<int>

if (remainder>1) {
  while(remainder%2 ==  0) {
    factors.add(2)
	remainder /= 2
  }
}
if(remainder > 1)
  factors.add(remainder)

return factors 
```

### Step 9

`factors(9)` shall be `listOf(3, 3)`.

Luckily, the test fails, which brings us to:

```kotlin
var remainder = n
val factors = mutableListOf<int>


if (remainder>1)
{
  var divisor = 2
  while(reminder > 1)
  {
      while(remainder% divisor ==  0)
      {
        factors.add(divisor)
    	remainder /= divisor
      }
	  divisor ++
  }
}

return factors 
```

### Step 10
Green test.

Here Bob Martin senses that the algorithm must be complete, and challenges it with 

```kotlin
factorsOf(2*2*3*3*5*7*11*13)
```

getting again green test.


## Can we do better?
Kent Beck is my hero, and I'm an avid admirer of Bob Martin. I profoundly love TDD since I got infected, years ago, and I'm convinced I still have everything to learn on the topic.

So, it's with a heart full of respect that I try to challenge the approach above, looking for chances of progress.

Let me start from the test suite we got:

```kotlin
@Test fun factors() {
  assertThat(factorsOf(1), `is` (emptyList()))
  assertThat(factorsOf(2), `is` listOf(2)))
  assertThat(factorsOf(3), `is` listOf(3)))
  assertThat(factorsOf(4), `is` listOf(2, 2)))
  assertThat(factorsOf(5), `is` listOf(5)))
  assertThat(factorsOf(6), `is` listOf(2, 3)))
  assertThat(factorsOf(7), `is` listOf(7)))
  assertThat(factorsOf(8), `is` listOf(2, 2, 2)))
  assertThat(factorsOf(9), `is` listOf(3, 3)))
  assertThat(factorsOf(2*2*3*3*5*7*11*13), `is` (listOf(2, 2, 3, 3, 5, 7, 11, 13)))
}
```

It's fine, but it says nothing about the "prime factorization". Nowhere does the idea of "prime number" emerge. Besides the name `factorsOf()`, little intuition is expressed about what it is going to happend.

Foundamentally, it is a collection of input-output pairs:


| Input               | Output                       |
|---------------------|------------------------------|
| `1`                 | `[]`                         |
| `2`                 | `[2]`                        |
| `3`                 | `[3]`                        |
| `4`                 | `[2, 2]`                     |
| `5`                 | `[5]`                        |
| `6`                 | `[2, 3]`                     |
| `7`                 | `[7]`                        |
| `8`                 | `[2, 2, 2]`                  |
| `9`                 | `[3, 3]`                     |
| `2*2*3*3*5*7*11*13` | `[2, 2, 3, 3, 5, 7, 11, 13]` |

I guess that displayed like that, it would not be apparent to everyone that this is about extracting the prime factors.


### Capturing the requirement
How can this be improved?<br/>
Let's do a step back, starting from the original requirement:

> Write a function that takes an integer and returns the collection of its prime factors;<br/>
> 
> Factors of a number are integers that,<br/>
> when multiplied together, result in the original number.
>
> A number is prime if it has no divisors other than 1 and itself

Using a C# FsCheck property-test, this could be translated to:

```csharp
[Property]
bool boolean_factorization_in_prime_numbers(PositiveInt positiveNumber)
{
    var n = positiveNumber.Item;
        
    var factors = factorsOf(n);
        
    return factors.AreAllPrime() && factors.Multiplied() == n;
}
```

where `AreAllPrime()` and `Multiplied()` are implemented as a literal translation of their definitions:

```csharp
internal static bool IsPrime(this int n) => 
    Enumerable.Range(1, n)
        .Where(i => !(i == 1 || i == n))
        .All(i => n.CannotBeDividedBy(i));

private static bool CannotBeDividedBy(this int n, int i) => 
    n % i != 0;

internal static bool AreAllPrime(this IEnumerable<int> xs) => 
    xs.All(IsPrime);

internal static int Multiplied(this IEnumerable<int> xs) => 
    xs.Aggregate(1, (product, i) => product * i);
```

Notice that you don't need to put brains to write this test and its helping functions: there is no judment nor opinion, no commonsense evaluation which values to consider or which shortcuts to take. Indeed, this is the result of the verbatim transcription into C# of the specification. 

Yet, it captures the requirement, it *reads* like a requirement. It conveys a domain meaning and it's not arbitrary.

If tests are intended to serve as documentation, I argue that this particular property test imparts significantly more information than the original `10` examples.


### Covering the examples
Can this single test replace the original `10`, without loss of information?<br/>
As a matter of fact, yes: it explores a much larger domain space, and almost for free.

By default, it will be exercised with `100` cases, but it would be a matter of configuration to increase the number to `1.000` or `200.000`.


### But can it be used to *lead development*?
Tests in TDD are a design tool. I like to think that they end up forming a non-regression test harness almost as a side-effect: their main goal, the reason why they are written in the first place, is to guide the programmers during the development, and let a design emerge.

You might think that a single property test, not being *incrementally* built, could not be effective in supporting an *incremental* style of development.

Here is where the shrinker comes to play.

When you run a test such as

```csharp
var n = positiveNumber.Item;
        
var factors = factorsOf(n);
        
return factors.AreAllPrime() && factors.Multiplied() == n;
```

against an empty implementation

```
IEnumerable<int> factorize(int n) => 
    null;
```

the test will fail trying with random numbers, and the shrinker will eventually find the simplest case for you to troubleshoot.

```
Falsifiable, after 1 test (1 shrink) (StdGen (737062029,297223429)):
Original:
PositiveInt 2
Shrunk:
PositiveInt 1
```

That's interesting. `1` is exactly the number Bob Martin started with. No reasons not to proceed exactly how he did, then:

```csharp
IEnumerable<int> factorize(int n) => 
    new []{1};
```

Even more interestingly, the test fails, this time bringing your attention on the number `2`:

```
Falsifiable, after 2 tests (0 shrinks) (StdGen (1305811796,297223430)):
Original:
PositiveInt 2
```

There are 2 remarkable results here:

* First, the shrinker is smart enough to do the job Bob Martin did, selecting for you the next logical case to troubleshoot, the number `2`
* Second, and more importantly: the test is still red!

With the traditional TDD approach, at this stage you would see a green test suite.<br/>
Every time you got a green test before, you had to infer from your experience and your gut feeling what that meant: do you need to put some other use cases into exercise, or can you cross the fingers and declare the algorithm complete?<br/>
You can never be sure.<br/>
A green test suite during an incremental TDD session based on "*Fake it until you make it*" does not give any direct evidence when "*you made it*": it is all left to your educated analysis and experience.

PBT helps here.<br/>
When it comes to using "Fake it", PBT doesn't forgive: it split hairs, and chases you with the next failing case, relentlessy. No matter how many hard-coded cases you add, it will find the next counterexample.

Far from being perfect (it still isn't a theorem prover), when a PBT library gives up and declare to be unable to find counterexamples, you can be more confident that your algorithm is finally complete.


### Is it faster?
So, `2` fails, and you need to enhance the code the same way Bob did. `3` will fail next and so on, in the very same order Bob chose.

You will not be surprised to observe that FsCheck will not bother you with `5`, `6` and `7`. Remember the series of `3` green tests and the questions that we raised? That's not the case with PBT: the library will directly jump to `8`.

Neither you will have doubts after developing the implementation for `9`, when the library will immediately greet you with a splendig green, communicating that you are done.<br/>
Of course, you might still reassure yourself trying with the `2*2*3*3*5*7*11*13`, as before; but the chances are you will not feel the need at all.<br/>
More probably, instead, you will be curious to know how the random values generated by FsCheck cover the domain space. You will be happy to know there are specific tools for observing the test case statistical distribution. That's an example taken from [Time Travelling and Fixing Bugs with Property-Based Testing][time-travelling], by Oskar Wickström:

```
λ> check prop_invalid_age_fails
  ✓ <interactive> passed 100 tests.
    not yet born 18% ███▌················ ✓ 5%
    too young    54% ██████████▊········· ✓ 5%
    too old      46% █████████▏·········· ✓ 5%
```


### Is it safer?
Compare this 2 traits: 

* With TDD, we started with a simplification of the requirement (the function shall return `emptyList`). In each step, we enhanced the requirement. This means that, along the process, until we reached the completeness, the specification was incomplete and partially lying. 

* With PDD we practiced an incremental development letting the shrinker identify increasingly complex use cases. Doing this, we never needed to modify the requirement: on the contrary, the specification was durably stable and complete, since the beginning.

Generally, we pursue a green test suite for the confidence it instills. There are few exceptions where green tests do not necessarily imply we are done.<br/>
The approach of PDD tends to have lesser exceptions than TDD.

### What about Collateral Properties?
We started with a direct translation of the business requirement into an Essential Property.

It might be interesting to enrich the test suite with other valid observations on the algorithm. Even more, a proper set of collateral properties can even replace the original test.

This is what Johannes Link does in [Property-based Testing in Java: Property-driven Development][property-driven-development], using the very same Kata.

He lists the following:

```
factorize(prime) -> [prime]
factorize(prime^2) -> [prime, prime]
factorize(prime^n) -> [prime, ..., prime]
factorize(prime1 * prime2) -> [prime1, prime2]
factorize(prime1 * .... * primeN) -> [prime1, ..., primeN]
factorize(n < 2) -> IllegalArgumentException
factorize(2 <= number <= Integer.MAX_VALUE) -> no exception  
product of all returned numbers must be equal to input number
all numbers in produced list must be primes
```

Interestingly, the last 2 he mentions constitute the Essential Property we have used.

On the one hand, the Essential Property has the benefit of capturing the domain meaning of the original requirement; on the other hand, the Collateral Properties Johannes relies on for his implementation possibly give an extra solidity to the test suite. There is an excellent study on the effectiveness of bug hunting of different styles of properties in [Bug Hunting: How to Specify it! In Java!][bug-hunting].


<details>
  <summary><h1>&#9658;&nbsp;References</h1></summary>


**Articles and manuals**

* [Quickcheck][quickcheck]: the original (a bit outdated) manual of the Haskell library
* [The Design and Use of QuickCheck][design-and-use-of-quickcheck]
* [Property-based Testing in Java - Johannes Link][property-based-testing-in-java]
* [xUnit Theory: Working With InlineData, MemberData, ClassData][xunit-theory]
* [Concolic Testing][concolic-testing]
* [Hypothesis - Integrated vs type based shrinking][integrated-vs-type-based-shrinking] 
* [When properties are easier than examples - Mark Seemann][properties-are-easier]
* [Test Oracle - Wikipedia][test-oracle]
* [ThePrimeFactorsKata - Bob Martin][the-prime-factor-kata]
* [Prime Factorization - Wolfram Mathworld][prime-factorization-wolfram]
  
* **Discovering properties**
  * [Choosing properties for property-based testing - Scott Wlaschin][choosing-properties]
  * [How to Specify it! - John Hughes][how-to-specify-it]
  * [How to Specify it! In Java! - Johannes Link][how-to-specify-it-in-java]
  * [Patterns to Find Good Properties - Johannes Link][patterns-to-find-good-properties]

* **Model-based Testing**
  * [Model-based Testing][model-based-testing]
  * [Model-based Testing with Hedgehog][model-based-testing-hedgehog]
  * [Model-based Testing with FsCheck][model-based-testing-fsharp]
  * [Model-based Testing in Java with jqwik - Johannes Link][model-based-testing-java]
  * [Model-based Testing with Makina][model-based-testing-makina]

* **Universal Quantification**
  * [Universal Quantification][universal-quantification]
  * [Universal Quantifier - in ncatlab.org][universal-quantifier]


**Libraries**
  * [Hedgehog][hedgehog]
  * [FsCheck][fscheck]
  * [jquick][jquick]
  * [CrossHair][crosshair]
  * [Hypothesis][hypothesis]
  * [fast-check][fast-check]
  * [js-verify][js-verify]
  * [stream_data][stream_data]


**Books**
* [Test-Driven Development By Example][tdd-by-example]

**Videos**
* [The lazy programmer's guide to writing thousands of tests - Scott Wlaschin][lazy-programmer]
* [How to Specify it! - John Hughes][how-to-specify-it-video]
* [The Three Laws of TDD (Featuring Kotlin) - Bob Martin][the-three-laws-of-tdd]
* [Property-based Testing in Java: Property-driven Development  - Johannes Link][property-driven-development]
* [Triangulation in Test-Driven Development - Dmitri Pavlutin][triangulation-in-tdd]
* [Time Travelling and Fixing Bugs with Property-Based Testing - Oskar Wickström][time-travelling]
* [Bug Hunting: How to Specify it! In Java!][bug-hunting]

</details>


[quickcheck]: https://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html
[fscheck]: https://fscheck.github.io/FsCheck/
[hedgehog]: https://hedgehog.qa/
[jquick]: https://jqwik.net/
[hypothesis]: https://hypothesis.works/
[fast-check]: https://github.com/dubzzz/fast-check
[js-verify]: https://github.com/jsverify/jsverify
[stream_data]: https://github.com/whatyouhide/stream_data
[design-and-use-of-quickcheck]: https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html
[xunit-theory]: https://hamidmosalla.com/2017/02/25/xunit-theory-working-with-inlinedata-memberdata-classdata/ 
[universal-quantification]: https://en.wikipedia.org/wiki/Universal_quantification
[universal-quantifier]: https://ncatlab.org/nlab/show/universal+quantifier
[choosing-properties]: https://fsharpforfunandprofit.com/posts/property-based-testing-2
[model-based-testing]: https://en.wikipedia.org/wiki/Model-based_testing
[model-based-testing-fsharp]: https://fscheck.github.io/FsCheck//StatefulTestingNew.html
[model-based-testing-hedgehog]: https://jacobstanley.io/how-to-use-hedgehog-to-test-a-real-world-large-scale-stateful-app/
[model-based-testing-java]: https://johanneslink.net/model-based-testing/
[model-based-testing-makina]: https://hexdocs.pm/makina/readme.html#using-makina
[how-to-specify-it]: https://www.dropbox.com/s/tx2b84kae4bw1p4/paper.pdf
[how-to-specify-it-video]: https://www.youtube.com/watch?v=G0NUOst-53U
[test-oracle]: https://en.wikipedia.org/wiki/Test_oracle
[how-to-specify-it-in-java]: https://johanneslink.net/how-to-specify-it
[concolic-testing]: https://en.wikipedia.org/wiki/Concolic_testing
[crosshair]: https://github.com/pschanely/CrossHair
[property-based-testing-in-java]: https://blog.johanneslink.net/2018/03/24/property-based-testing-in-java-introduction/
[properties-are-easier]: https://blog.ploeh.dk/2021/02/15/when-properties-are-easier-than-examples/
[integrated-vs-type-based-shrinking]: https://hypothesis.works/articles/integrated-shrinking/
[patterns-to-find-good-properties]: https://blog.johanneslink.net/2018/07/16/patterns-to-find-properties/#patterns-to-find-good-properties
[lazy-programmer]: https://www.youtube.com/watch?v=IYzDFHx6QPY
[the-three-laws-of-tdd]: https://www.youtube.com/watch?v=qkblc5WRn-U
[the-prime-factor-kata]: http://www.butunclebob.com/ArticleS.UncleBob.ThePrimeFactorsKata
[prime-factorization-wolfram]: https://mathworld.wolfram.com/PrimeFactorization.html
[tdd-by-example]: https://www.pearson.com/en-us/subject-catalog/p/test-driven-development-by-example/P200000009421/9780321146533
[property-driven-development]: https://blog.johanneslink.net/2019/05/11/property-based-driven-development/
[triangulation-in-tdd]: https://dmitripavlutin.com/triangulation-test-driven-development/
[time-travelling]: https://wickstrom.tech/2019-11-17-time-travelling-and-fixing-bugs-with-property-based-testing.html
[bug-hunting]: https://johanneslink.net/how-to-specify-it/#5-bug-hunting

# Comments
[GitHub Discussions](https://github.com/arialdomartini/arialdomartini.github.io/discussions/22)

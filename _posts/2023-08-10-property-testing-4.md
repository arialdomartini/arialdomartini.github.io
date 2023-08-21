
---
layout: post
title: "Property-based Testing For The Rest Of Us - 2"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- bash
- zsh
---

[Property-based Testing in Java: Property-driven Development  - Johannes Link][property-driven-development]


If you apply the practices of TDD relying on Property-based Tests rather than on Example-based ones, what you get is *Property-driven Development* (PDD).

I claim that PDD is more correct, safer and faster than the example-based TDD. I will argument my bold statement through the implementation of a classical, simple coding kata.

## The Prime Factors Kata

I'm using the [ThePrimeFactorsKata][the-prime-factor-kata], a little exercise that Bob Martin is being using since 2005 to demo TDD. We will first follow the classical approach, mirroring what Bob Martin does; then we will repeat the same using PBT, commenting the differences.

The requirement is to write a function that takes an integer and returns the collection of the prime factors.

Bob Martin solves it with few tests (from 7 to 10, depending on the session). You can watch him in action in the video [The Three Laws of TDD (Featuring Kotlin) - Bob Martin][the-three-laws-of-tdd].

## The classic TDD approach

Bob Martin's approach boils down to this iterative process:

1. He starts from the extreme simplification of the requirement, recuced to the simplest possible, trivial, problem
2. He resolves the requirement with a minimal implementation, making sure the test is green
3. He asks himself if the implementation is complete
4. If is not, he extends the requirement, adding the next logical use-case, and he starts over from 2.

Here's a summary of the steps he performs.

### Step 1
The simplest possible requirement is: the list of the prime factors of `1` is empty.

Now, technically speaking, this is a mathematical nonsense: prime factorization is defined for numbers `n >= 2` (see [Prime Factorization - Wolfram Mathworld][prime-factorization-wolfram]), so Bob should have been started from `2`. But let's take it as good and let's keep following the original implementation.

The test to be passed is:

```kotlin
@Test fun factors() {
  assertThat(factorsOf(1), `is` (emptyList()))
}
```

which can be resolved with

```kotlin
private fun factorsOf(n: Int): List<int> {
  return emptyList()
}
```

Here Bob Martin is applying "Fake it until you make it", an idiomatic technique described by Kent Beck in his seminal "Test-Driven Development
By Example" as:

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

Basically, Bob Martin keeps adding the next assert, each time incrementing the number by `1`. That's the application of "Triangulation", a technique described in Kent Beck's book which is meant to be complementary to "Fake it".

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
But we also know we have proceeded by arbitrary, and probably simplistic examples. 

The said truth is: we cannot trust the test suite too much, at this point. Not being led by tests, we cannot but use our brains and commonsense, and keep adding a further test.


### Step 7
Again, all the tests are green.

Shall we finally stop? Maybe yes, the algorithm is complete, and there are no bugs.

But there are! The algorithm will not work with `8`. 

### Step 8
`factorsOf(8)` should be `listOf(2, 2, 2)`. 

Finally, we found how to proceed. We should consider ourselves very lucky: what if the algorithm continued being green until `100`? I'm sure many would have just stopped trying.

Here's the fixed / improved implementation:

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
Here Bob Martin senses that the algorithm must be complete, and challenges it with 

```kotlin
factorsOf(2*2*3*3*5*7*11*13)
```

getting a green test.



# Resources

* [ThePrimeFactorsKata - Bob Martin][the-prime-factor-kata]
* [Prime Factorization - Wolfram Mathworld][prime-factorization-wolfram]
* [Property-based Testing in Java: Property-driven Development  - Johannes Link][property-driven-development]
* [Triangulation in Test-Driven Development - Dmitri Pavlutin][triangulation-in-tdd]

## Videos
* [The Three Laws of TDD (Featuring Kotlin) - Bob Martin][the-three-laws-of-tdd]




[the-prime-factor-kata]: http://www.butunclebob.com/ArticleS.UncleBob.ThePrimeFactorsKata
[the-three-laws-of-tdd]: https://www.youtube.com/watch?v=qkblc5WRn-U
[prime-factorization-wolfram]: https://mathworld.wolfram.com/PrimeFactorization.html
[property-driven-development]: https://blog.johanneslink.net/2019/05/11/property-based-driven-development/
[triangulation-in-tdd]: https://dmitripavlutin.com/triangulation-test-driven-development/

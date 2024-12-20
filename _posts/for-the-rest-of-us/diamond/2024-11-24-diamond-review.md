---
layout: post
title: "There is no Emergent Design in TDD (and how to fix it) - Review"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- tdd
- functional programming
- property-based testing
ftrou: true
include_in_index: false
---

## Summaries

From Matteo Vaccari:

There’s no easy way to evolve the hardcoded values into general code.

* Alistair Cockburn said that this problem could be better solved by
  more mathematically-oriented thinking upfront about how many spaces
  should be generated where
* Emily Bache said that "recycling tests" is a valid way to iterate
  towards a difficult problem
* Other solutions came from Ron Jeffries, George Dinwiddie, Jon
  Jagger, again Ron Jeffries


### Francesco Cirillo
[Francesco Cirillo - The Diamond Kata  Anti-IF Workshop][cirillo] 

* Payed workshop.
* It promises that TDD and Anti-IF Programming "let design
  abstractions emerge [...] in a way that prioritizes intentional
  design over algorithmic complexity, allowing for an adaptable and
  emergent solution."
* It states: "No Thinking Ahead Required: Unlike conventional wisdom,
  this workshop will show you how to solve the Diamond Kata using TDD
  without the need for anticipatory thinking [...] showing how
  abstractions emerge naturally through TDD."

### Seb Rose
[Seb Rose - Recycling tests in TDD][seb-rose]

* Seb uses the Print Diamond Kata in his trainings.
* He relies on the classical test progression (first the `a` case,
  then the `b` case, etc).
* He shows how some implementations are hardcoded (applying Fake It
  Until You Make It).
* He mentions the typical struggle with Example-Based TDD: "The code
  is now screaming for us to refactor it, but to keep all the tests
  passing most people try to solve the entire problem at once. That’s
  hard, because we’ll need to cope with multiple lines, varying
  indentation, and repeated characters with a varying number of spaces
  between them."
* He suggests a second approach: "we start by decomposing the diamond
  problem into smaller constituent parts. This time I chose to write
  the following test:"
* He mentions the "the top half of the diamond", without making it
  explicit it the code.
* He claims that "by recycling the second test multiple times, each
  time modifying the expected output to become closer to what we
  actual want, we’ve allowed ourselves to be guided gradually to a
  solution."

Here are the used tests:

```csharp
[TestFixture]
public class DiamondTest
{
    [Test]
    public void A_should_generate_a_single_character()
    {
        Assert.AreEqual("A", Diamond.Create('A'));
    }
 
    [Test]
    public void B_should_generate_the_smallest_diamond()
    {
        Assert.AreEqual(" A\nB B\n A", Diamond.Create('B'));
    }
 
    [Test]
    public void C_should_generate_the_next_diamond()
    {
        Assert.AreEqual("  A\n B B\nC   C\n B B\n  A", Diamond.Create('C'));
    }
}
```

and:

```csharp
using NUnit.Framework;
 
[TestFixture]
public class DiamondTest
{
    [Test]
    public void A_should_give_single_character()
    {
        Assert.AreEqual("A\n", Diamond.Create('A'));
    }
 
    [Test]
    public void B_should_be_a_diamond()
    {
        Assert.AreEqual(" A\nB B\n A\n", Diamond.Create('B'));
        Assert.AreEqual(" A\n"  +
                        "B B\n" + 
                        " A\n", Diamond.Create('B'));
    }
 
    [Test]
    public void C_should_be_a_bigger_diamond()
    {
        string diamond = "  A\n"   + 
                         " B B\n"  +
                         "C   C\n" +
                         " B B\n"  +
                         "  A\n";
 
        Assert.AreEqual(diamond, Diamond.Create('C'));
    }
 
    [Test]
    public void D_should_be_an_even_bigger_diamond()
    {
        string diamond = "   A\n"   + 
                         "  B B\n"  +
                         " C   C\n" +
                         "D     D\n" +
                         " C   C\n" +
                         "  B B\n"  +
                         "   A\n";
 
        Assert.AreEqual(diamond, Diamond.Create('D'));
    }
}
```

[Seb Rose - Diamond recycling (and painting yourself into a
corner)][seb-rose-2]



### Alistair Cockburn
[Alistair Cockburn - Thinking before programming][alistair-cockburn]

* "[...] a few professors had the idea that they might teach
  programmers to think a bit before hitting the keycaps. [...]. They
  showed how to create programs (of a certain category) without error,
  by thinking about the properties of the problem and deriving the
  program as a small exercise in simple logic."
* Accorging to Alistair XP and TDD "when practiced well, involve a
  different kind of thinking – not about the problem, but about the
  code. TDD has become a pablum to avoid thinking."
* His approach is to combine the Dijkstra-Gries approach (thinking
  ahead) with TDD ("modern fine-grained incremental development").
* In his premises, he seems to describe PBT: "What happens if we think
  about the problem and look for patterns and properties before
  starting, and then instead of typing the whole thing in at once, we
  "grow" the program using fine-grained incremental development and
  TDD?"
* Mentions the struggles about incremental development with TDD: 

>    Step 1: Go out and buy really good sneakers. 
>
>    Step 2: Do warmup exercises. 
>
>    Step 3: Jump really really really high, like, to the moon. "

* He promises to reflect on the problem, not on the solution.
* He identifies some properties:
    * The diamond is `2x+1` per `2x+1`.
    * For each row, there are `2x-1` spacers.
    * There is a formula for leading and trailing spaces.
    
* Yet, he implements those ideas following the classical steps (case
  `0`, then `1` etc).
* The formulas he conceived are the implementation. So, the test did
  not guide the design, to the point that he writes the implementation
  before the first test.
* Interestingly, the name of his tests have the typical "ForAll" form
  of a property test:
  
```
def test_02_makes_and_fills_tray_of_any_size
```

* He claims that he did not type in all those tests at once, because
  "to do so is not only against the rules of TDD [...]), but is
  actually dangerous". I don't agree with this: in fact, his
  implementation (the fomula he conceived) was already complete. The
  additional tests did not help evolving the formula.
  
* Even more interestingly, despite the initial good intuitions, his
  tests are a description of the solution, not the problem:
  
```
:test_01_fills_first_and_last_slot_in_tray
:test_02_makes_and_fills_tray_of_any_size
:test_03_makes_bumper_for_row_in_diamond
:test_04_glues_bumpers_to_tray_making_complete_row_in_diamond
:test_05_makes_diamond_the_right_size
:test_06_puts_a_row_in_both_upper_and_lower_half_of_diamond
:test_07_digit_diamond_fills_correctly
```

* Notice the use of expressions like "the right size" and "fills
  correctly".



### Jon Jagger
[Jon Jagger - sliming and refactoring and deliberate duplication][jon-jagger]

* Classic progression: `a` case, `b` case, `c` case, all hardcoded.
* He claims "As the tests get more specific, the code should get more
  generic. I have three specific tests, but the code is equally
  specific."
* Beautful recursive implementation. Really brilliant.
* Yet, not induced by a test. Definitely, not an emergent design.





### George Dinwiddie
[George Dinwiddie -  Another Approach to the Diamond Kata][george-dinwiddie]

* He also starts from the trivial degenerate diamond (with a hardcoded
  implementation).
* "The next step is obviously to implement the `B` case". Why
  "obviously"?
* He refuses to hardcode `B`. His implementation, though, is not the
  minumum code strictly necessary to make the test pass, but already a
  generalization.

### Emily Bache
[Emily Bache - Iterative and Incremental TDD with the Diamond Kata][emily-bache]

* She believes that "recycling tests" is a valid way to iterate
  towards a difficult problem
* Nothing new in her [proposed repo][emily-bache-repo], though: tests
  are incrementally going from `A` to `D`.

### Matteo Vaccari
[Matteo Vaccari - The Diamond Kata Revisited][matteo-vaccari]

* He acknowledges that "the problem statement is simple, but solving
  it with TDD is not straightforward" because "there’s no easy way to
  evolve the hardcoded values into general code".
* Rather than trying to arrive at a "single algorithm" that will solve
  the problem in one shot, he suggests a compositional approach.
* He analyzes some traits of the problem (not the solution!),
  decomposing the problem into sub-problems and in fact capturing some
  properties, such as the symmetry of quadrants.

### Géza Mihala
[Géza Mihala - Diamond Kata][geza-mihala]

* Similarly to Matteo Vaccari, he plays with decomposing the problem
  (using functional decomposition).
* He also identifies 2 properties: horizontal and vertical symmetry.
* The 4 properties that define the problem are:
  -  a sequence of letters from ’A’ up to a given letter
  -  arranged diagonally in a square, padded with blanks
  -  mirrored to the left, not repeating the first column
  -  mirrored down, not repeating the bottom row
* The only missing step in his post is: he does not use TDD at all.


# References

* Resolved with Example-Based Testing
  * [Francesco Cirillo - The Diamond Kata  Anti-IF Workshop][cirillo] 
  * [Seb Rose - Recycling tests in TDD][seb-rose]
  * [Seb Rose - Diamond recycling (and painting yourself into a corner)][seb-rose-2]
  * [Alistair Cockburn - Thinking before programming][alistair-cockburn]
  * [Jon Jagger - sliming and refactoring and deliberate duplication][jon-jagger]
  * [George Dinwiddie -  Another Approach to the Diamond Kata][george-dinwiddie]
  * [Emily Bache - Iterative and Incremental TDD with the Diamond Kata][emily-bache]
  * [Emily Bache - DiamondKata repository][emily-bache-repo]
  * [Matteo Vaccari - The Diamond Kata Revisited][matteo-vaccari]
  * [Ron Jeffries - TDD on the Diamond Problem][ron-jeffries-1]
  * [Ron Jeffries - More about Diamond][ron-jeffries-2]
  * [Géza Mihala - Diamond Kata][geza-mihala]
  
* Resolved with Property-Based Testing
  * [Nat Pryce - Diamond Kata – TDD with only Property-Based Tests][nat-pryce]
  * [Mark Seemann - Diamond kata with FsCheck][mark-seemann]
  * [Uldis Sturms - property based testing in javascript - diamond kata][uldis-sturms]
  


[cirillo]: https://www.antiifprogramming.com/anti-if-workshop-the-diamond-kata.php
[seb-rose]: https://claysnow.co.uk/recycling-tests-in-tdd/
[seb-rose-2]: https://claysnow.co.uk/diamond-recycling-and-painting-yourself-into-a-corner/
[alistair-cockburn]: https://web.archive.org/web/20170621004437/http://alistair.cockburn.us/Thinking+before+programming
[jon-jagger]: https://jonjagger.blogspot.com/2012/06/sliming-and-refactoring-and-deliberate.html
[george-dinwiddie]: https://blog.gdinwiddie.com/2014/11/30/another-approach-to-the-diamond-kata/
[emily-bache]: https://coding-is-like-cooking.info/2015/04/iterative-incremental-tdd-diamond-kata/
[emily-bache-repo]: https://github.com/emilybache/DiamondKata
[matteo-vaccari]: https://matteo.vaccari.name/posts/the-diamond-kata-revisited/
[ron-jeffries-1]: https://ronjeffries.com/articles/tdd-diamond/
[ron-jeffries-2]: https://ronjeffries.com/articles/more-diamond/
[geza-mihala]: https://infinitary.org/stray_words/diamond_kata.html

[nat-pryce]: https://semaphoreci.com/community/tutorials/diamond-kata-tdd-with-only-property-based-tests
[mark-seemann]: https://blog.ploeh.dk/2015/01/10/diamond-kata-with-fscheck/
[uldis-sturms]: property based testing in javascript - diamond kata


{% include fp-newsletter.html %}

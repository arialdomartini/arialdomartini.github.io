---
layout: post
title: "Property-based Testing For The Rest Of Us - Property-driven Development"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
include_in_index: false
tags:
- tdd
- functional programming
- property-based testing
---
## Index
1. [Utterly opinionated introduction to Property Testing](property-testing)
2. [Shut up and code!](property-testing-2)
3. [It's properties all the way down](property-testing-3)
4. [Property-driven Development](property-testing-4)


# References

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
  * [junit-quickheck][junit-quickcheck]
  * [QuickTheories][quicktheories]
  * [ScalaCheck][scala-check]
  * [test.check][test.check]
  * [Kotest][kotest]
  
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


[quickcheck]: https://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html
[fscheck]: https://fscheck.github.io/FsCheck/
[hedgehog]: https://hedgehog.qa/
[jquick]: https://jqwik.net/
[junit-quickcheck]: https://pholser.github.io/junit-quickcheck/site/1.0/
[quicktheories]: https://github.com/quicktheories/QuickTheories
[scala-check]: https://scalacheck.org/
[test.check]: https://github.com/clojure/test.check
[kotest]: https://github.com/kotest/kotest
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

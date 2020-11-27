---
layout: post
title: "Y-Combinator in strictly evaluated Scheme"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- C#
- IoC
- AutoFac
---
Deriving the Y-Combinator from a recursive implementation of factorial
<!--more-->
# Recursive Y
1. Recursive Factorial
2. Inject continuation
3. Define recursive Y for lazy languages
4. Define recursive Y for strict languages

## **#1** - Recursive factorial

Let's start from a trivial recursive implementation of a factorial

``` scheme
(define (zero? n)
  (= n 0))
(define (decr n)
  (- n 1))
  
(define (fact n)
  (if (zero? n)
    1
    (* n (fact (decr n)))))
```

which we can define using a lambda:


``` scheme
(define fact
  (lambda (n)
    (if (zero?n)
      1
      (* n (fact (decr n))))))
```

## **#2** - Inject continuation
Since we aim to remove recursion, let's inject the continuation. We define a function `mk-fact`, which returns the original `fact` when provided with the right continuation.

``` scheme
;; creates fact given the right continuation f
(define mk-fact
  (lambda (f)
    (lambda (n)
      (if (zero? n)
          1
          (* n (f (decr n)))))))

;; a by-definition factorial (working up to n=5) to be used as a continuation
(define bydef
  (lambda (n)
    (cond
     ((= n 0) 1)
     ((= n 1) 1)
     ((= n 2) 2)
     ((= n 3) 6)
     ((= n 4) 24)
     ((= n 5) 120)
     (else -1))))


(define fact
  (mk-fact bydef))
```


## **3** - Define recursive Y for lazy languages
In a lazy language we could have:

``` scheme
(define Y
  (lambda (f)
    (f (Y f))))
    
(define fact
  (Y mk-fact))
```

This fails in a strict language, with an error about maximum recursion depth exceeded. 

## **4** - Define recursive Y for strict languages
In a strict language, this is fixed using a lambda.
``` scheme
(define Y
  (lambda (f)
    (f (lambda (x) ((Y f) x)))))
```


So far, we got to a recursive definition of Y. In other words, we factored the recursion away from `fact`.

<br/><br/><br/>

# Non-recursive Y
To get to a non-recursive `Y`, let's start over from the beginning.

## **#1** - Recursive factorial

``` scheme
(define (fact n)
  (if (zero? n)
    1
    (* n (fact (decr n)))))
```

## **#2** - Inject self

``` scheme
(define (part-factorial self n)
  (if (zero? n)
      1
      (* n (self self (decr n)))))

(define (fact n)
  (part-factorial part-factorial n))
```

## **#3** - Separate the function taking self (push `lambda n` down)

``` scheme
(define (part-factorial self)
  (lambda (n)
    (if (zero? n)
        1
        (* n ((self self) (decr n))))))
```

## **#4** - Refactor self self using a let expression

```scheme
(define (part-factorial self)
  (let ((f (self self)))
    (lambda (n)
      (if (zero? n)
          1
          (* n (f (decr n)))))))
```

In a strict language, use instead:

``` scheme
(define (part-factorial self)
  (let ((f (lambda (x) ((self self)x))))
    (lambda (n)
      (if (zero? n)
          1
          (* n (f (decr n)))))))
```

## **#5** - Convert let to lambda expression
```scheme
(let ((x (some-value)))
  (body))
```
becomes: 

```scheme
((lambda (x)
   (body))
 (some-value))
```

Then:

``` scheme
(define (part-factorial self)
  ((lambda (f)
     (lambda (n)
       (if (zero? n)
           1
           (* n (f (decr n))))))
   (lambda (x) ((self self) x))))
```

## **#6** - Extract factorial away

```scheme
(define (part-factorial self)
  (mk-factorial
    (lambda (x) ((self self) x))))

(define mk-factorial
  (lambda (f)
    (lambda (n)
      (if (zero? n)
          1
          (* n (f (decr n)))))))
```

## **#7** - Move self down in a lambda
```scheme
(define part-factorial
  (lambda (self)
    (mk-factorial
     (lambda (x) ((self self) x)))))

(define factorial
  (part-factorial part-factorial))
```

## **8** - Factorial as part part using a lambda
```scheme
(define factorial
  ((lambda (x) (x x))
   part-factorial))
```

## **9** - Inline part-factorial
```scheme
(define factorial
  ((lambda (x) (x x))
   (lambda (self)
     (mk-factorial
      (lambda (x) ((self self) x))))))

```

## **#10** - Extract mk-factorial
```scheme
(define Y
  (lambda (f)
    ((lambda (x) (x x))
     (lambda (x) (f (lambda (y) ((x x) y)))))))
```

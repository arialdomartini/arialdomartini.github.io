f
## Rest
Citations:


forall a. in Haskell means "for every type a".1 It's introducing a
type variable, and declaring that the rest of the type expression has
to be valid whatever choice is made for a.


You usually don't see it in basic Haskell (without turning on any
extensions in GHC), because it's not necessary; you just use type
variables in your type signature, and GHC automatically assumes there
are foralls introducing those variables at the start of the
expression.

zip :: forall a. ( forall b. ( [a] -> [b] -> [(a, b)] ))
zip :: forall a. forall b. [a] -> [b] -> [(a, b)]
zip :: forall a b. [a] -> [b] -> [(a, b)]
zip :: [a] -> [b] -> [(a, b)]

The above are all the same

forall mainly comes into play with extensions, because then you can
introduce type variables with scopes other than the default ones
assumed by GHC if you don't explicitly write them.
# References

* [Gabriella Gonzales][gabriella-gonzales]  - [Polymorphism For Dummies][polymorphism-for-dummies]

* [Relationship between Haskell's 'forall' and '=>'](https://stackoverflow.com/questions/33199180/relationship-between-haskells-forall-and)
* https://downloads.haskell.org/~ghc/6.12.2/docs/html/users_guide/other-type-extensions.html#explicit-foralls

* [Luca Cardelli][luca-cardelli], Peter Wegner -  [On understanding types, data abstraction, and polymorphism][on-understanding-types]

* https://wasp-lang.dev/blog/2021/09/01/haskell-forall-tutorial

* [What does the `forall` keyword in Haskell/GHC do?][what-does-forall-do]
  
* [System F][system-f] 

[what-does-forall-do]: https://stackoverflow.com/questions/3071136/what-does-the-forall-keyword-in-haskell-ghc-do

[gabriella-gonzales]: https://www.blogger.com/profile/01917800488530923694

[polymorphism-for-dummies]: https://www.haskellforall.com/2015/10/polymorphism-for-dummies.html

[luca-cardelli]: http://lucacardelli.name/
[on-understanding-types]: http://lucacardelli.name/Papers/OnUnderstanding.A4.pdf


[system-f]: https://en.wikipedia.org/wiki/System_F


(+) :: Num a => a -> a -> a

This means that (+) is exactly the same kind of thing as anything with
a simpler type like a -> a -> a, except the Num a => is telling us
that we're not free to choose any type a. We can only choose a type
for a when we know that it is a member of the Num type class (another
slightly more precise way of saying "a is a member of Num is "the
constraint Num a holds").

(+) :: forall a. Num a => a -> a -> a

In which case you can read this off moderately easily as an English
sentence once you know what forall a. and Num a => means: "For every
type a, provided Num a holds, plus has the type a -> a -> a".


you need -XExplicitForAll 

I suppose it helps if we add the implied parentheses:

(+) :: ∀ a . ( Num a => (a -> (a -> a)) )
id :: ∀ a . ( a -> a )

The ∀ always goes together with a .. It's basically special syntax
meaning “anything between ∀ and . are type variables that I want to
introduce into the following scope”†





Type class dictionaries on the other hand are genuinely arguments at
runtime; they have to be created and passed around between functions.
The "implicit arguments" (either at type level in GHC's intermediate
output, or at value level at runtime) are the implementation strategy
for parametric polymorphism and type class constraints; not what they
mean. There are alternative possible implementations


## Probably not true
†Actually this is a type-level lambda. The type expression ∀ a . b
behaves analogously to the value level expression \a -> b.


=> denotes what Idris calls an implicit function: Num a is a
dictionary for the instance Num a, and such a dictionary is implicitly
needed whenever you're adding numbers. But whether a is a type
variable here that was previously introduced by some ∀, or a fixed
type, doesn't really matter. You could also have

(+) :: Num Int => Int -> Int -> Int

That's just superfluous, because the compiler knows that Int is a Num
instance and hence automatically (implicitly!) chooses the right
dictionary.



The caller has to "prove" that the chosen a is an instance of the
class by providing its methods as hidden arguments: this is done by
the compiler under the hood. E.g.

```haskell
f :: forall a. Show a => ... 
```

is handled, roughly, as if it were 

```haskell
f :: (a->String) -> .... 
```

In this way, when the code of f uses show, the compiler can simply
invoke it through the implicit a->String argument. On calls to f, the
compiler will add a suitable argument for the programmer, according to
the available instances. –


:set -XUnicodeSyntax

f :: ∀ a . a -> a ; f x = x





As the forall matter appears to be settled, I'll attempt to explain
the => a bit. The things to the left of the => are arguments, much
like ones to the left of a ->. But you don't apply these arguments
manually, and they can only have specific types.

f :: Num a => a -> a

is a function that takes two arguments:

    A Num a dictionary.
    An a.

When you apply f, you just provide the a. GHC has to provide the Num
a. If it's applied to a specific concrete type like Int, GHC knows Num
Int and can supply it at the call site. Otherwise, it checks that Num
a is provided by some outer context and uses that one. The great thing
about Haskell's typeclass system is that it ensures that any two Num a
dictionaries, however they are found, will be identical. So it doesn't
matter where the dictionary comes from—it is sure to be the right one.


Further discussion

A lot of these things we're talking about aren't exactly part of
Haskell so much as they're part of the way GHC interprets Haskell by
translation to GHC core, AKA System FC, an extension of the very
well-studied System F, AKA the Girard-Reynolds calculus. System FC is
an explicitly typed polymorphic lambda calculus with algebraic
datatypes, etc., but no type inference, no instance resolution,
etc. After GHC checks the types in your Haskell code, it translates
that code to System FC by a thoroughly mechanical process. It can do
this confidently because the type checker "decorates" the code with
all the information the desugarer needs to plumb all the dictionaries
around. If you have a Haskell function that looks like

foo :: forall a . Num a => a -> a -> a
foo x y= x + y

then that will translate to something that looks like

foo :: forall a . Num a -> a -> a -> a
foo = /\ (a :: *) -> \ (d :: Num a) -> \ (x :: a) -> \ (y :: a) -> (+) @a d x y

The /\ is a type lambda—it's just line a normal lambda except it takes
a type variable. The @ represents application of a type to a function
that takes one. The + is really just a record selector. It chooses the
right field from the dictionary it's passed.


type applications don't survive compilation


 in Haskell 98, all the free type variables of an explicit
 source-language type signature are universally quantified, except for
 the class type variables in a class declaration.
 
 
 
 
 
 # manual
 7.8.5.3. Implicit quantification

GHC performs implicit quantification as follows. At the top level
(only) of user-written types, if and only if there is no explicit
forall, GHC finds all the type variables mentioned in the type that
are not already in scope, and universally quantifies them. For
example, the following pairs are equivalent:

  f :: a -> a
  f :: forall a. a -> a

  g (x::a) = let
                h :: a -> b -> b
                h x y = y
             in ...
  g (x::a) = let
                h :: forall b. a -> b -> b
                h x y = y
             in ...

 Notice that GHC does not find the innermost possible quantification
 point. For example:

  f :: (a -> a) -> Int
           -- MEANS
  f :: forall a. (a -> a) -> Int
           -- NOT
  f :: (forall a. a -> a) -> Int
  
-- explain here the difference
  


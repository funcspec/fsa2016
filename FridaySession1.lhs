== Friday Session  2015-09-09

> module FridaySession1 where

==

**Play, play, play**

The quickest way to get fluent in Haskell is by using
the interpreter as a tutoring tools.

. . .

Try out things all the time.

E.g. you are investigating `map` and `takeWhile`:

     *FridaySession1> map (*2) [0..10]
     [0,2,4,6,8,10,12,14,16,18,20]
     *FridaySession1> takeWhile (\n -> even n || n < 10) [0..]
     [0,1,2,3,4,5,6,7,8,9,10]
     *FridaySession1> takeWhile (\n -> even n && n < 10) [0..]
     [0]
     *FridaySession1> takeWhile (\n -> even n && n < 10) (map (*2) [0..]

     <interactive>:18:51:
         parse error (possibly incorrect indentation or mismatched brackets)
     *FridaySession1> takeWhile (\n -> even n && n < 10) (map (*2) [0..])
     [0,2,4,6,8]

And so on

==

Using **github** and **git**

You can use the *github* interface and paste your homework
direcly into your repository.

But it is much more convenient to use `git`.

The most important git commands are

     git clone <directory>

     git pull

     git add <file>

     git commit -m "first homework"

     git push

     man gittutorial

Here is a [guide to the tutorials](https://help.github.com/articles/good-resources-for-learning-git-and-github/)


==

About **stack**

The simplest way to use *stack* is this:

     stack exec ghci FridaySession1.lhs

For linux users it is convenient to add an alias to `.bashrc`.

     alias sghci="stack exec ghci"

==

**Useful commands in GHCi**

`:q` exits.

`:r` reloads the input file.

`:t` gives you the type of something.

`:set +s` turns on timing and memory measurements.

With `let x = 5` you can store something in memory.

This also works for functions:

    Prelude> let x = 10
    Prelude> x + 5
    15
    Prelude> let f = (\n -> n + 7)
    Prelude> f 10
    17
    Prelude> f x
    17

==

**Using QuickCheck for system investigation**

See this
[introduction to QuickCheck testing](https://www.schoolofhaskell.com/user/pbv/an-introduction-to-quickcheck-testing).

     import Test.QuickCheck

Example: find out the difference between `mod` and `rem`

     quickCheckResult (\ m n -> rem m n == mod m n)

This gives exceptions, because of a division by 0 error. Change this to:

     quickCheckResult (\ m n -> n /= 0 ==> rem m n == mod m n)

This gives:

     *** Failed! Falsifiable (after 9 tests and 5 shrinks):
     -1
     2

Now we see the difference:

     *FridaySession1> rem (-1) 2
     -1
     *FridaySession1> mod (-1) 2
     1

==

**Prefix vs. Infix**

The + function is infix, i.e. we write `5 + 7` and not `+ 5 7`.
To make an infix function a prefix function, we bracket it:

    Prelude> (+) 5 7
    12
    Prelude> (*) 5 7
    35

To turn a prefix function to an infix function, use ` `:

    Prelude> 4 `elem` [1..]
    True

==

Infix operators have to be written with non-alphabet symbols.
To define them we actually define their prefix equivalent:

> (<-->) :: Int -> Int -> Int
> (<-->) m n = m + n + 7

    FridaySession1> 4 <--> 10
    21

==

**Types and Type Classes**

The function `map` is fully polymorphic, here `a` and `b` can really
be anything:

     map :: (a -> b) -> [a] -> [b]

Similarly, addition can work on many things: Integers, Reals, Fraction, ...
But there are some things which it will not work on: Strings, Boys, ...
To distinguish these, Haskell uses type classes. In this case, the
class is called Num:

    (+) :: Num a => a -> a -> a

==

Even `5` and `23984724` are not of a fixed type but can be instantiated in
every type that is in the type class Num.

    5 :: Num a => a
    23984724 :: Num a => a
    (+23984724) :: Num a => a -> a

Another type class is `Eq`. It contains all types for which we have an
equality test. We need this for example in the definition of `elem`:

    elem :: Eq a => a -> [a] -> Bool


==

**Redundant matching and brackets**

Here are two tools to clean up your code:

* **hlint** - complains about redundancy and ugly things
  It can be installed with "cabal install hlint"

* **the "-Wall" option for ghc and ghci**
  This will complain about many things, for example
  missing type definitions and unused matches.

Example:

    bad_left_even :: (Int,Int) -> Bool
    bad_left_even (n,m) = (even n)

*hlint* says:

     FridaySession1.lhs:144:3: Suggestion: Use camelCase
     Found:
       bad_left_even (n, m) = ...
     Why not:
       badLeftEven (n, m) = ...

     FridaySession1.lhs:144:25: Suggestion: Redundant bracket
     Found:
       (even n)
     Why not:
       even n

     2 hints


**ghci -Wall** says:

    FridaySession1.lhs:143:20: Warning: Defined but not used: ‘m’

A better version is thus:

> leftEven :: (Int,Int) -> Bool
> leftEven (n,_) = even n

==

Shiny and new: Foldable

In recent versions of GHC many functions which used to work
only on lists are now more generic. This includes `map` and `foldl`.
If map in your GHCI has this type, then you have the new stuff:

    map :: (Foldable t) => (a -> b) -> t a -> t b

Think of this `t` as something like [ ]. For now and probably the
rest of this course you can replace `t a` with `[a]` in your mind
and in your own type definitions.

[More information about this here on the Haskell wiki](https://wiki.haskell.org/Foldable_Traversable_In_Prelude)


==

**Other Useful Links**

[GHCi on Acid](https://wiki.haskell.org/GHC/GHCi#GHCi_on_Acid)

[Hoogle](https://www.haskell.org/hoogle/)

[Hoogle lookup of `reverse`](https://www.haskell.org/hoogle/?hoogle=reverse)

[Hoogle lookup of the type `(a -> b) -> [a] -> [b]`](https://www.haskell.org/hoogle/?hoogle=%28a-%3Eb%29+-%3E+[a]+-%3E+[b])

[GHC is full of lies - here is the blog post I mentioned.](https://www.fpcomplete.com/blog/2015/02/primitive-haskell)

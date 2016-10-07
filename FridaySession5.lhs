Notes from the session on Friday, 2016-10-07
============================================

> module FridaySession5 where

Import Suggestions
-----------------

If you only use a few of the functions from an imported module, it is
good practice to list what you are using. This also allows you to reuse
all not imported variable names here without ambiguity or overshadowing.

> import Data.Maybe (fromJust,fromMaybe)

If you want almost everything from a module, you can use `hiding`
as follows. For example, if `seq` is your favorite variable name, do this:

> import Prelude hiding (seq)

The Prelude library is always imported by GHC and GHCi.
Explicitly mentioning it allows us to hide some of its parts.


Foldable surprises: "length" works on many things
-------------------------------------------------

Supppose we have the following list of tuples and want to get the length of
the list in the tuple with 2 in the first place:

> mymap :: [(Int,[Int])]
> mymap = [(1, [3,4,2,5]), (2,[6,5]), (3,[2,4,5,1,6]) ]

> wrong :: Int
> wrong = length $ lookup 2 mymap

This compiles, but it does not do what we want:

    > wrong
    1

Let us look at the type of `length` again:

    > :t length
    length :: Foldable t => t a -> Int

We can also see which things are in the type class `Foldable`:

    > length (Just 1)
    > :info Foldable
    ...
    instance Foldable [] -- Defined in ‘Data.Foldable’
    instance Foldable Maybe -- Defined in ‘Data.Foldable’

So anything of type `Maybe` is foldable and `length` is defined on anything of type `Maybe a`:

    > length (Just 4)
    1
    > length (Just "Hello")
    1
    > length (Just (print [1,2,3]))
    1

The correct way to get what we wanted above is thus to unwrap
the Just result from lookup first and then apply length.

> correct :: Int
> correct = length list where (Just list) = lookup 2 mymap

Or, using `fromMaybe` or `fromJust` from `Data.Maybe`:

> correct2 :: Int
> correct2 = length (fromMaybe (error "Not found.") $ lookup 2 mymap)
>
> correct3 :: Int
> correct3 = length (fromJust $ lookup 2 mymap)


Can we have unordered pairs?
----------------------------

Yes, and this is a nice example how to define our own instances of the `Show` and `Eq` type classes:

> data UnOrdPair a = Pair (a,a)
>
> instance (Show a, Ord a) => Show (UnOrdPair a) where
>   show (Pair (x,y)) | x <= y    = "Pair (" ++ show x ++ "," ++ show y ++ ")"
>                     | otherwise = "Pair (" ++ show y ++ "," ++ show x ++ ")"
>
> instance Ord a => Eq (UnOrdPair a) where
>   (==) (Pair (x1,y1)) (Pair (x2,y2)) = (x1==x2 && y1 == y2) || (x1==y2 && y1 == x2)


Taking apart non-empty lists
----------------------------

We have `head`, `tail`, `init` and `last`. The most beautiful explanation of
what they do is from [Learn You a Haskell, 2.3](http://learnyouahaskell.com/starting-out#an-intro-to-lists):

![](https://s3.amazonaws.com/lyah/listmonster.png)

Note that all four functions fail on the empty list with an exception.


Report Example
--------------

Please have a look at [report-example](https://github.com/funcspec/report-example)
and use it for your final projects if you want.


Limit the memory used by GHCi
-----------------------------

To prevent GHCi from eating all the memory and crashing your computer, use this:

    stack exec ghci -- mynicefilename.hs -Wall +RTS -M256m

The 256m is the memory limit. If things do not work you can increase it, but
don't put more than your computer actually has. If you then do something that
uses up all memory, like computing the sum of all numbers, it will crash only
GHCi and not your whole system:

    Prelude> sum [(1::Int)..]
    <interactive>: Heap exhausted;
    <interactive>: Current maximum heap size is 268435456 bytes (256 MB).
    <interactive>: Use `+RTS -M<size>' to increase it.

Additionally, under Unix systems you can prefix any command with "nice" to
indicate that it should be considered less important than other programs
running on your computer.

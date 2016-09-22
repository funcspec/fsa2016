FSA Lab, Week 1
===============

> module FSAlab1 where


Exercises with foldr and foldl
------------------------------

The foldr function is defined by

  foldr :: (a -> b -> b) -> b -> [a] -> b
  foldr f b [] = b
  foldr f b (x:xs) = f x (foldr f b xs)

This gives the abstract pattern of recursion over lists, and it can
be used to give abstract definitions of many functions that operate
on lists. Example:

  and = foldr (&&) True.

Define length in terms of foldr.

Define elem x in terms of foldr.

Define or in terms of foldr.

Define map f in terms of foldr.

Define filter p in terms of foldr.

Define (++) in terms of foldr.

Define reverse in terms of foldr.

The function foldr folds to the right. It is also possible to fold
to the left, by means of:

  foldl :: (a -> b -> a) -> a -> [b] -> a
  foldl f z [] = z
  foldl f z (x:xs) = foldl f (f z x) xs

Give an alternative definition of reverse, in terms of foldl.

Which of the two versions of reverse is more efficient? Why?

One of foldr, foldl may work on infinite lists. Which one? Why?

Look up some explanations at https://wiki.haskell.org/Fold


Implement and use the Luhn Algorithm
------------------------------------

https://en.wikipedia.org/wiki/Luhn_algorithm

The Luhn algorithm is a formula for validating credit card numbers.

Give an implementation in Haskell. The type declaration should run:

luhn :: Integer -> Bool

This function should check whether an input number satisfies the
Luhn formula.

Next, use luhn to write functions

isAmericanExpress, isMaster, isVisa :: Integer -> Bool

for checking whether an input number is a valid American Express
Card, Master Card, or Visa Card number. Consult Wikipedia for the
relevant properties.


Crime Scene Investigation
-------------------------

A group of five school children is caught in a crime. One of them
has stolen something from some kid they all dislike. The
headmistress has to find out who did it. She questions the
children, and this is what they say:

Matthew: Carl didn’t do it, and neither did I.

Peter: It was Matthew or it was Jack.

Jack: Matthew and Peter are both lying.

Arnold: Matthew or Peter is speaking the truth, but not both.

Carl: What Arnold says is not true.

Their class teacher now comes in. She says: three of these boys
always tell the truth, and two always lie. You can assume that
what the class teacher says is true. Use Haskell to write a
function that computes who was the thief, and a function that
computes which boys made honest declarations. Here are some
definitions to get you started.

> data Boy = Matthew | Peter | Jack | Arnold | Carl
>            deriving (Eq, Show)

> boys :: [Boy]
> boys = [Matthew, Peter, Jack, Arnold, Carl]

You should first define a function

accuses :: Boy -> Boy -> Bool

for encoding whether a boy accuses another boy.

Next, define

accusers :: Boy -> [Boy]

giving the list of accusers of each boy.

Finally, define

guilty, honest :: [Boy]

to give the list of guilty boys, plus the list of boys who made
honest (true) statements. If the puzzle is well-designed, then
guilty should give a singleton list.
Hint: the puzzle is well-designed.


Know Thyself
------------

If this was all difficult for you, go back to the exercises in the
first two chapters of The Haskell Road (http://www.cwi.nl/~jve/HR/)
and do the implementation exercises.


Bonus
-----

If this was all easy for you, you should next try some of the
problems of Project Euler (https://projecteuler.net/). Try
problems 9, 10 and 49.


Submission deadline
-------------------

Sunday, Sept 11, at midnight. But: make sure that you have some
work to discuss for the Friday code review session.

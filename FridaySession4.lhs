Notes from the session on Friday, 2016-09-30
============================================

> module FridaySession4 where


Practical Hints
---------------

- To measure how long it takes GHCi to evaluate something, use `:set +s`

- The `main` function in Haskell should alway be of type `IO ()`.

- Instead of duplicating a complete file and then making changes or
  additions, you should use `import` and then put additional new
  functions in the second file.
  See [here](https://github.com/funcspec/fsa2016/blob/master/FSA3.hs#L4)
  and [here](https://github.com/m4lvin/mchlpe/blob/master/QALogic.lhs#L12) for examples
  and [here for a whole chapter on modules](http://learnyouahaskell.com/modules).


Comments on Lab 3
-----------------

- The average number of hints in a Sudoku is decreasing in this order:
  Classic, Crossed, NRC, NRCX.

- Counting Sudokus: We discussed different ideas from Stefania and Eric
  but it does not seem straightforward.

Stable Roommates Problem on Wikipedia
-------------------------------------

The example looked wrong in some browsers because a strikedthrough
four did look exactly the same as a normal four.
The numbers are grayed out now as well:
<https://en.wikipedia.org/wiki/Stable_roommates_problem#Example>


Epistemic Model Checking
------------------------

- to be added

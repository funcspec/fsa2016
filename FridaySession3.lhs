Notes from the session on Friday, 2016-09-16
============================================

> module FridaySession3 where


How to install and use hlint
----------------------------

If you use stack:

 -  Do `stack install hlint` in a terminal (not in GHCi).
 -  Make sure that the folder `~/.local/bin` which stack mentions at
    the end is in your `PATH` variable. How to do that depends on your
    operating system. Under Unix, edit the file ~/.bash_profile
    or ~/.bashrc and add a line like this:

        PATH="$HOME/.local/bin:$PATH"

    Here `$HOME` is the same as `~`, namely your home directory,
    something like `/home/alice` or `/Users/bob`.

If you use the Haskell platform, do `cabal install hlint` and
add "~/.cabal/bin" to your PATH variable.

After this, you can do `hlint filename.hs` to get a list of useful complaints.


Speed up GHCi with Object Code
------------------------------

To get faster results in GHCi, use the `-fobject-code` flag.
Using stack, and together with the recommended -Wall warning
you would do this for a *single file*:

    stack exec ghci -- -Wall -fobject-code filename.hs

The `--` is to separate arguments for stack from those for ghci.

Bonus exercise: Learn how to configure an abbreviation for this
on your operating system. (Hint: `~/.bashrc` and `alias`).

If you use a `stack.yaml` and a `.cabal` file, then you can
add both options to `ghc-options` in the .cabal file like
[here](https://github.com/funcspec/fsa2016/blob/master/fsa2016.cabal#L7)
and `stack ghci` will use them.


Mastermind
----------

To optimize the strategies, hard code the first guess.

Avoid redundancy in your code: Functions like the loop which
calls the guess and feedback functions should only be written
once, with a parameter for which strategy should be used.
See [this code by Rahiel](https://github.com/funcspec/Rahiel/blob/master/FSAlab2.hs#L58) for an example.

So, what's the best strategy now?
See [this code by Stefania](https://github.com/funcspec/Stefania/blob/master/FSAlab2.hs#L207)
for a simple comparison of the different strategies.
Note this is for only four colours.


Blocks of let, let in, do, etc.
-------------------------------

We discussed the following examples with let ... in ..., do, and let.

> mynumber :: Int
> mynumber =
>   let x = 5
>   in
>     x +10

> saymynumber :: IO ()
> saymynumber = do
>   let
>     x = 5
>   print $ (x::Integer) + 10

General advice: Use `let ... in ...` for pure functions and only
use do for IO (or other monadic) functions. Though surprisingly
`do` can also be used in pure functions:

> puredo :: Int
> puredo = do
>   let x = 4
>   x + 10


Editors
-------

By now you should be using an editor that

- has syntax highlighing
- replaces all tabs with spaces
- eliminates trailing whitespace

and maybe even

- offer completion of function names
- shows you the type of expressions
- integrates ghc(i) with warnings and hlint

The one which Malvin used today is called *Atom* and made by people at github.
It runs on Linux, OS X and Windows. See [atom.io](https://www.atom.io)
and install the following packages to get integrated ghc, hlint and more
goodies: `autocomplete-haskell`, `haskell-ghc-mod`, `ide-haskell`,
`ide-haskell-repl`, `language-haskell` and  `trailing-space`.

If you use *Notepad++*, please tell it to use spaces instead of tabs in
the language-specific preferences. Also, have a look at the NppExec
plugin to get integrated GHCi like this.


Git and github
--------------

Use git, not the browser interface, whenever you want to do more than
uploading a single file. Please update your email in the settings, using
the same address as on github:

    git config user.name "John Doe"
    git config --global user.email "john@doe.nl"


Hoare Logic vs. Testing in Haskell
----------------------------------

We noted again that Hoare triples can not express arbitrary relations
on input-output pairs. For example, this means that we can not say in
one Hoare triple that a Sudoko `P :: Sudoku -> Sudoku` should return
a solution to the initial problem (and not overwrite given entries).

Still, in Haskell it is no problem to have more general tests like this:

> test :: (a->b) -> ( (a,b) -> Bool) -> [a] -> Bool
> test f prop xs = all prop [(x,f x) | x <- xs]

By thew way, `all prop` is the same as `(and . map prop)`.

> myf :: Int -> Int
> myf x | odd  x    = 10
>       | otherwise = x

> myprop :: (Int,Int) -> Bool
> myprop (x,fx) = odd x || x == fx

This gives:

    test myf myprop [1..10]
    True


Ideas for Final Projects
------------------------

See also [this list](http://homepages.cwi.nl/~jve/courses/16/fsa/lab/ReportTopics.html).

 -  Continue the homeowkr an do a detailed comparison of mastermind strategies.
 -  Binary Decision Diagrams (BDDs):
    - [Donald Knuth's 17th annual Christmas Tree Lecture](https://www.youtube.com/watch?v=axUgEAgrSB8)
      is probably the best introduction, next to the chapter in his
      [The Art of Computer Programming](https://cs.stanford.edu/~uno/taocp.html):
      Combinatorial Algorithms, Part 1, vol. 4A.* (2011), pp. 202--280.

    - [NooBDD](https://github.com/m4lvin/NooBDD), a totally naive and not-reducing BDD package from Malvin
 -  Epistemic Model Checking using BDDs:
     -  Malvin is working on this.
        See https://github.com/jrclogic/SMCDEL
     -  Possible project: Use the parallel BDD package [Sylvan](http://fmt.ewi.utwente.nl/tools/sylvan/) via [these bindings](https://github.com/adamwalker/sylvan-haskell).
 -  The certified programming language [idris](http://www.idris-lang.org/).
 -  The proof assistant [Agda](http://wiki.portal.chalmers.se/agda/pmwiki.php), programmed in Haskell.

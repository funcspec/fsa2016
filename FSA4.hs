
module FSA4 where

import Control.Monad.State
import System.Random

table :: [(Int, Int)]
table = map (\ x -> (x,x^(3::Integer))) [1..100]

process :: Int -> Int -> Maybe Int
process m n = lookup m table >>= \ v ->
              lookup n table >>= \ w -> return (v+w)

greetings :: IO ()
greetings = do putStrLn "First name?"
               x <- getLine
               putStrLn "Second name?"
               y <- getLine
               putStrLn  ("hello "++x++" "++y)

greetz :: IO ()
greetz = putStrLn "First name?" >>
         getLine >>= \ x ->
         putStrLn "Second name?" >>
         getLine >>= \ y ->
         putStrLn  ("hello "++x++" "++y)

action :: Maybe Integer
action = do Just x <- Nothing
            return (x^(2::Integer))

action' :: Maybe a
action' = Nothing >>= f where
  f (Just x) = return x
  f Nothing  = fail ""

data Tree a = Leaf a | Branch (Tree a) (Tree a)
     deriving (Eq,Ord,Show)

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Branch left right) = Branch (fmap f left)
                                      (fmap f right)

instance Applicative Tree where
-- pure ::  a -> Tree a
   pure = Leaf
-- (<*>) :: Tree (a -> b) -> Tree a -> Tree b
   (<*>) (Leaf f) atree = fmap f atree
   (<*>) (Branch fl fr) atree = Branch (fl <*> atree) (fr <*> atree)

instance Monad Tree where
-- return :: a -> Tree a
   return = Leaf
-- (>>=) :: Tree a -> (a -> Tree b) -> Tree b
   (Leaf x) >>= f = f x
   (Branch left right) >>= f =
         Branch (left >>= f) (right >>= f)

hello :: IO ()
hello = do
  putStrLn "What is your name?"
  n <- getLine
  putStrLn ("Hello " ++ n)

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

randomFlip :: Int -> IO Int
randomFlip x = do
  b <- getRandomInt 1
  if b==0 then return x else return (-x)

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do
    x <-  getRandomInt k
    y <- randomFlip x
    xs <- getIntL k (n-1)
    return (y:xs)

getIntL' :: Int -> Int -> IO [Int]
getIntL' _ 0 = return []
getIntL' k n = getRandomInt k   >>= \ x  ->
               randomFlip x     >>= \ y  ->
               getIntL k (n-1)  >>= \ xs ->
               return (y:xs)

genIntList :: IO [Int]
genIntList = do
  k <- getRandomInt 20
  n <- getRandomInt 10
  getIntL k n

spuriousGetline :: IO String
spuriousGetline = do x <- getLine
                     return x

process' :: Int -> Int -> Maybe Int
process' m n = liftM2 (+)
                (lookup m table) (lookup n table)

newtype StateMonad s a = SM (s -> (a,s))

instance Functor (StateMonad s) where
-- fmap :: (a -> b) -> StateMonad s a -> StateMonad s b
   fmap f (SM x) = SM (\ s -> let (a,t) = x s in (f a, t))

instance Applicative (StateMonad s) where
-- pure ::  a -> StateMonad s a
   pure a = SM (\ s -> (a,s))
-- (<*>) :: StateMonad s (a -> b) -> StateMonad s a -> StateMonad s b
   (SM xf) <*> (SM xa) = SM (\ s -> let (f  ,s' ) = xf s
                                        (a  ,s'') = xa s'
                                    in  (f a,s'') )

instance Monad (StateMonad s) where
  return a = SM (\ s -> (a,s))
  x >>= f = SM (\s -> let SM x' = x
                          (a,s') = x' s
                          SM f' = f a
                          (b,s'') = f' s'
                      in (b,s''))

fetch :: StateMonad s s
fetch = SM (\ s -> (s,s))

store :: s -> StateMonad s ()
store x =  SM (const ((),x))

tck :: StateMonad Int Int
tck = do n <- fetch
         store (n+1)
         return n

tick :: State Int Int
tick = do n <- get
          put (n+1)
          return n

newtype Parser a = Parser { parse :: String -> [(a,String)] }

instance Functor Parser where
  fmap f (Parser p) = Parser (\cs -> [(f a, b) | (a, b) <- p cs])

instance Applicative Parser where
    pure x = Parser (\ cs -> [(x,cs)])
    (Parser p1) <*> (Parser p2) =
        Parser (\cs -> [(f a, cs2) | (f, cs1) <- p1 cs,
                                     (a, cs2) <- p2 cs1])

instance Monad Parser where
    return = pure
    p >>= f = Parser (\cs ->
              concat [parse (f a) cs' | (a,cs') <- parse p cs ])

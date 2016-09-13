
module FSAlab2
where
import Data.List

data Colour   = Red | Yellow | Blue | Green | Orange
                deriving (Eq,Show,Bounded,Enum)

data Answer   = Black | White deriving (Eq,Show)

type Pattern  = [Colour]
type Feedback = [Answer]

samepos :: Pattern -> Pattern -> Int
samepos _      []                 = 0
samepos []     _                  = 0
samepos (x:xs) (y:ys) | x == y    = samepos xs ys + 1
                      | otherwise = samepos xs ys

occurscount ::  Pattern -> Pattern -> Int
occurscount _  []       = 0
occurscount xs (y:ys)
          | y `elem` xs = occurscount (delete y xs) ys + 1
          | otherwise   = occurscount xs ys

reaction :: Pattern -> Pattern -> [Answer]
reaction secret guess = replicate n Black
                     ++ replicate m White
   where n = samepos secret guess
         m = occurscount secret guess - n

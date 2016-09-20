module FSA2

where 

import System.Random
import Test.QuickCheck

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

randomFlip :: Int -> IO Int
randomFlip x = do 
   b <- getRandomInt 1
   if b==0 then return x else return (-x)

genIntList :: IO [Int]
genIntList = do 
  k <- getRandomInt 20
  n <- getRandomInt 10
  getIntL k n

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  getRandomInt k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs)

quicksort :: Ord a => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) = 
   quicksort [ a | a <- xs, a <= x ]  
   ++ [x]
   ++ quicksort [ a | a <- xs, a > x ]

isTrue :: a -> Bool
isTrue _ = True

prop_ordered :: Ord a => [a] -> Bool
prop_ordered [] = True
prop_ordered (x:xs) = all (>= x) xs && prop_ordered xs

testR :: Int -> Int -> ([Int] -> [Int])
                    -> ([Int] -> [Int] -> Bool) -> IO ()
testR k n f r = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- genIntList
                  if r xs (f xs)
                    then do
                      print ("pass on: " ++ show xs)
                      testR (k+1) n f r
                    else
                      error ("failed test on: " ++ show xs)

testPost :: ([Int] -> [Int]) -> ([Int] -> Bool) -> IO ()
testPost f p = testR 1 100 f (\_ -> p)

quicksrt :: Ord a => [a] -> [a]  
quicksrt [] = []  
quicksrt (x:xs) = 
   quicksrt [ a | a <- xs, a < x ]  
   ++ [x]
   ++ quicksrt [ a | a <- xs, a > x ]

samelength :: [Int] -> [Int] -> Bool
samelength xs ys = length xs == length ys

testRel :: ([Int] -> [Int]) -> ([Int] -> [Int] -> Bool) -> IO ()
testRel f r = testR 1 100 f r 

forall = flip all

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

stronger, weaker :: [a] -> 
       (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

neg :: (a -> Bool) -> a -> Bool
neg p = \ x -> not (p x)

infixl 2 .&&. 
infixl 2 .||.

(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p .&&. q = \ x -> p x && q x 

(.||.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p .||. q = \ x -> p x || q x

compar :: [a] -> (a -> Bool) -> (a -> Bool) -> String
compar xs p q = let pq = stronger xs p q 
                    qp = stronger xs q p 
                in 
                  if pq && qp then "equivalent"
                  else if pq  then "stronger"
                  else if qp  then "weaker"
                  else             "incomparable"

infixl 2 #

(#) :: (a -> b) -> (b -> c) -> (a -> c)
(#) = flip (.)

infixl 1 $$

($$) :: a -> (a -> b) -> b
($$) = flip ($)

update :: Eq a => (a -> b) -> (a,b) -> a -> b
update f (x,y) = \ z -> if x == z then y else f z 

updates :: Eq a => (a -> b) -> [(a,b)] -> a -> b
updates = foldl update 

type Var = String
type Env = Var -> Integer

data Expr = I Integer | V Var
          | Add Expr Expr 
          | Subtr Expr Expr 
          | Mult Expr Expr 
          deriving (Eq,Show)

eval :: Expr -> Env -> Integer 
eval (I i) _ = i 
eval (V name) env = env name
eval (Add e1 e2) env = (eval e1 env) + (eval e2 env)
eval (Subtr e1 e2) env = (eval e1 env) - (eval e2 env)
eval (Mult e1 e2) env = (eval e1 env) * (eval e2 env)

assign :: Var -> Expr -> Env -> Env 
assign var expr env =  update env (var, eval expr env)

initEnv :: Env 
initEnv = \ _ -> undefined

example = initEnv $$ 
          assign "x" (I 3) # 
          assign "y" (I 5) # 
          assign "z" (Mult (V "x") (V "y")) #
          eval (V "z")

data Condition = Eq Expr Expr 
               | Lt Expr Expr 
               | Gt Expr Expr 
               | Ng Condition 
               | Cj [Condition] 
               | Dj [Condition]
               deriving (Eq,Show)

data Statement = Ass Var Expr
               | Cond Condition Statement Statement
               | Seq [Statement]
               | While Condition Statement
               deriving (Eq,Show)

evalc :: Condition -> Env -> Bool
evalc (Eq e1 e2) env = eval e1 env == eval e2 env
evalc (Lt e1 e2) env = eval e1 env <  eval e2 env
evalc (Gt e1 e2) env = eval e1 env >  eval e2 env 
evalc (Ng c) env = not (evalc c env)
evalc (Cj cs) env = and (map (\ c -> evalc c env) cs)
evalc (Dj cs) env = or  (map (\ c -> evalc c env) cs)

exec :: Statement -> Env -> Env 
exec (Ass v e) env = assign v e env
exec (Cond c s1 s2) env = 
 if evalc c env then exec s1 env else exec s2 env 
exec (Seq ss) env = foldl (flip exec) env ss 
exec w@(While c s) env = 
 if not (evalc c env) then env 
 else exec w (exec s env) 

fib :: Statement
fib = Seq [Ass "x" (I 0), Ass "y" (I 1), 
           While (Gt (V "n") (I 0))
             (Seq [Ass "z" (V "x"), 
                   Ass "x" (V "y"),
                   Ass "y" (Add (V "z") (V "y")), 
                   Ass "n" (Subtr (V "n") (I 1))])]

run :: [(Var,Integer)] -> Statement -> [Var] -> [Integer]
run xs program vars = 
  exec program (updates initEnv xs) $$ 
    \ env -> map (\ c -> eval c env) (map V vars)

while :: (a -> Bool) -> (a -> a) -> a -> a
while = until . (not.)

euclid m n = (m,n) $$
   while (\ (x,y) -> x /= y) 
         (\ (x,y) -> if x > y then (x-y,y) 
                              else (x,y-x)) #
         fst

whiler :: (a -> Bool) -> (a -> a) -> (a -> b) -> a -> b
whiler p f r = while p f # r

euclid2 m n = (m,n) $$
          whiler (\ (x,y) -> x /= y) 
                 (\ (x,y) -> if x > y then (x-y,y) 
                                      else (x,y-x))
                 fst

fibonacci :: Integer -> Integer
fibonacci n = fibon (0,1,n)

fibon = whiler 
         (\ (_,_,n) -> n > 0)
         (\ (x,y,n) -> (y,x+y,n-1))
         (\ (x,_,_) -> x)

fb :: Integer -> Integer
fb n = fb' 0 1 n where 
   fb' x y 0 = x 
   fb' x y n = fb' y (x+y) (n-1)

data Coin = C Int

w :: Coin -> Float 
w (C n) = if n == lighter then 1 - 0.01
          else if n == heavier then 1 + 0.01
          else 1

weight :: [Coin] -> Float
weight = sum . (map w)

balance :: [Coin] -> [Coin] -> Ordering 
balance xs ys = 
  if weight xs < weight ys then LT
  else if weight xs > weight ys then GT
  else EQ

lighter, heavier :: Int
lighter = 3
heavier = 0

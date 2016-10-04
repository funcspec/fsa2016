{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module FSA5 where

import System.Random

factorsNaive :: Integer -> [Integer]
factorsNaive n0 = factors' n0 2 where
  factors' 1 _ = []
  factors' n m
    | n `mod` m == 0 = m : factors' (n `div` m) m
    | otherwise      =     factors' n (m+1)

factors :: Integer -> [Integer]
factors n0 = let
   ps = takeWhile (\m -> m^(2::Integer) <= n0) primes
 in factors' n0 ps where
   factors' 1 _  = []
   factors' n [] = [n]
   factors' n (p:ps)
    | n `mod` p == 0 = p: factors' (n `div` p) (p:ps)
    | otherwise      =    factors' n ps

prime :: Integer -> Bool
prime n = factors n == [n]

primes :: [Integer]
primes = 2 : filter prime [3..]

mers :: Integer -> Integer
mers 1  = 2^2-1;    mers 2  = 2^3-1;     mers 3  = 2^5-1
mers 4  = 2^7-1;    mers 5  = 2^13-1;    mers 6  = 2^17-1
mers 7  = 2^19-1;   mers 8  = 2^31-1;    mers 9  = 2^61-1
mers 10 = 2^89-1;   mers 11 = 2^107-1;   mers 12 = 2^127-1
mers 13 = 2^521-1;  mers 14 = 2^607-1;   mers 15 = 2^1279-1
mers 16 = 2^2203-1; mers 17 = 2^2281-1;  mers 18 = 2^3217-1
mers 19 = 2^4253-1; mers 20 = 2^4423-1;  mers 21 = 2^9689-1
mers 22 = 2^9941-1; mers 23 = 2^11213-1; mers 24 = 2^19937-1
mers 25 = 2^21701-1;
mers _  = undefined

addM :: Integer -> Integer -> Integer -> Integer
addM x y = rem (x+y)

multM :: Integer -> Integer -> Integer -> Integer
multM x y = rem (x*y)

invM :: Integer -> Integer -> Integer
invM x n = let
   (u,v) = fctGcd x n
   copr  = x*u + v*n == 1
   i     = if signum u == 1 then u else u + n
 in
   if copr then i else error "no inverse"

fctGcd :: Integer -> Integer -> (Integer,Integer)
fctGcd a b =
  if b == 0
  then (1,0)
  else
     let
       (q,r) = quotRem a b
       (s,t) = fctGcd b r
     in (t, s - q*t)

coprime :: Integer -> Integer -> Bool
coprime n m = gcd n m == 1

coprime' :: Integer -> Integer -> Bool
coprime' n m = let (x,y) = fctGcd n m
               in x*n + y*m == 1

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

grow :: (node -> [node]) -> node -> Tree node

grow step seed = T seed (map (grow step) (step seed))

takeT :: Int -> Tree a -> Tree a

takeT 0 (T x _) = T x []
takeT n (T x ts) = T x (map (takeT (n-1)) ts)

coprimeT :: Tree (Integer,Integer)
coprimeT = grow f (1,1)

f :: (Integer,Integer) -> [(Integer,Integer)]
f (n,m) = [(n+m,m),(n,n+m)]

pairs :: [(Integer,Integer)]
pairs = concatMap (\ n -> zip [1..n] (repeat n)) [1..]

coprimes :: [(Integer,Integer)]
coprimes = filter (uncurry coprime) pairs

expM ::  Integer -> Integer -> Integer -> Integer
expM x y = rem (x^y)

exM :: Integer -> Integer -> Integer -> Integer
exM = expM -- to be replaced by a fast version

primeTestF :: Integer -> IO Bool
primeTestF n = do
   a <- randomRIO (1, n-1) :: IO Integer
   return (exM a (n-1) n == 1)

primeTestsF :: Int -> Integer -> IO Bool
primeTestsF k n = do
 as <- sequence $ fmap (\_-> randomRIO (1,n-1)) [1..k]
 return (all (\ a -> exM a (n-1) n == 1) as)

decomp :: Integer -> (Integer,Integer)
decomp n0 = decomp' (0,n0) where
  decomp' = until (odd.snd) (\ (m,n) -> (m+1,div n 2))

primeMR :: Int -> Integer -> IO Bool
primeMR _ 2 = return True
primeMR 0 _ = return True
primeMR k n = error "not yet implemented"

composites :: [Integer]
composites = error "not yet implemented"

encodeDH :: Integer -> Integer -> Integer -> Integer
encodeDH p k m = error "not yet implemented"

decodeDH :: Integer -> Integer -> Integer
         -> Integer -> Integer -> Integer
decodeDH p k ga b c = error "not yet implemented"

encode :: Integer -> Integer -> Integer -> Integer
encode p k m = let
   p' = p-1
   e  = head [ x | x <- [k..], gcd x p' == 1 ]
 in
   exM m e p

decode :: Integer -> Integer -> Integer -> Integer
decode p k m = let
   p' = p-1
   e  = head [ x | x <- [k..], gcd x p' == 1 ]
   d  = invM e p'
 in
   exM m d p

cipher :: Integer -> Integer
cipher = encode secret bound

decipher :: Integer -> Integer
decipher = decode secret bound

totient :: Integer -> Integer
totient n = toInteger $ length [ k | k <- [1..n], gcd k n == 1 ]

phi :: Integer -> Integer -> Integer
phi p q = (p - 1) * (q - 1)

select :: Integer -> Integer -> Integer
select p q = let
   t = phi p q
 in
   head [ x | x <- [3..], gcd x t == 1 ]

rsaPublic :: Integer -> Integer -> (Integer,Integer)
rsaPublic p q = error "not yet implemented"

rsaPrivate ::  Integer -> Integer -> (Integer,Integer)
rsaPrivate p q = error "not yet implemented"

rsaEncode, rsaDecode :: (Integer,Integer) -> Integer -> Integer
rsaEncode (e,n) m =  error "not yet implemented"

rsaDecode = rsaEncode

trapdoor :: (Integer,Integer) -> Integer -> Integer
trapdoor = rsaEncode

secret, bound :: Integer
secret = mers 18
bound  = 131

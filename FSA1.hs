module FSA1 where

sentence,onAndOn :: String
sentence = "Sentences can go " ++ onAndOn
onAndOn  = "on and " ++ onAndOn

sentences :: [String]
sentences = "Sentences can go on":
               map (++ " and on") sentences

threefold :: Integer -> Bool
threefold n = rem n 3 == 0

threefolds :: [Integer]
threefolds = filter threefold [0..]

nats :: [Integer]
nats = [0..]

query1, query2 :: Bool
query1 = all (\ n -> any (\ m -> n < m) nats) nats
query2 = any (\ n -> all (\ m -> n <= m) nats) nats

forall, exist :: [a] -> (a -> Bool) -> Bool
forall = flip all
exist  = flip any

query1', query2' :: Bool
query1' = forall nats (\ n -> exist nats (\ m -> n < m))
query2' = exist nats (\ n -> forall nats (\ m -> n <= m))

myall, myany :: Foldable t => (a -> Bool) -> t a -> Bool
myall p = foldr (\ x b -> p x && b) True
myany p = foldr (\ x b -> p x || b) False

divide :: Integer -> Integer -> Bool
divide n m = rem m n == 0

isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\ d -> not (divide d n)) [2..n-1]

isPrime' :: Integer -> Bool
isPrime' n = all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^(2::Integer) <= n) [2..]

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^(2::Integer) <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

sieve :: [Integer] -> [Integer]
sieve (n:ns) = n : sieve (filter (\m -> rem m n /= 0) ns)
sieve [] = error "sieve needs a beginning"

eprimes :: [Integer]
eprimes = sieve [2..]

reversal :: Integer -> Integer
reversal = read . reverse . show

solution :: [Integer]
solution = takeWhile (< 10000) (filter (prime.reversal) primes)

least :: (Integer -> Bool) -> Integer

least p = head (filter p nats)

least1 :: Num t => (t -> Bool) -> t
least1 p0 = lst p0 0 where
  lst p n = if p n then n else lst p (n+1)

dif2 :: [Integer] -> [(Integer,Integer)]
dif2 (p:q:rs) = if p + 2 == q then (p,q) : dif2 (q:rs)
                else dif2 (q:rs)
dif2 _ = error "dif2 needs a list of length >= 2"

primePairs :: [(Integer,Integer)]
primePairs = dif2 primes

factors :: Integer -> [Integer]
factors k = factors' k (takeWhile (\m -> m^(2::Integer) <= k) primes) where
  factors' 1 _ = []
  factors' n [] = [n]
  factors' n (p:ps)
    | n `mod` p == 0 = p : factors' (n `div` p) (p:ps)
    | otherwise      = factors' n ps

ppairs :: [[Integer]]
ppairs = map (\ (p,_) -> factors (p+1))  primePairs

dif6 :: [Integer] -> [(Integer,Integer,Integer)]
dif6 (p:q:r:ss) = if p + 6 == r then (p,q,r) : dif6 (q:r:ss)
                  else dif6 (q:r:ss)
dif6 _ = error "dif6 needs a list of length >= 3"

primeTriples :: [(Integer, Integer, Integer)]
primeTriples = dif6 primes

sol2 :: [(Integer, Integer, Integer)]
sol2 = take 100 primeTriples

nextPrime :: Integer -> Integer
nextPrime n = if prime n then n else nextPrime (n+1)

mersenne :: [(Integer,Integer)]
mersenne = [ (p,2^p -1) | p <- primes, prime (2^p - 1) ]

counterexamples :: [([Integer],Integer)]
counterexamples = [ (ps,product ps + 1) |
                         ps <- [ take n primes | n <- [2..] ],
                         not $ prime (product ps + 1) ]

pythTriples :: [(Integer,Integer,Integer)]
pythTriples = filter (\ (x,y,z) -> x^(2::Integer) + y^(2::Integer) == z^(2::Integer))
   [ (x,y,z) | z <- [1..], x <- [1..z], y <- [1..z], x < y ]
-- [ (x,y,z) | z <- [1..], x <- [1..z], y <- [x+1..z] ]

pythQuadruples :: [(Integer,Integer,Integer,Integer)]
pythQuadruples = filter (\ (x,y,z,u) -> x^(2::Integer) + y^(2::Integer) + z^(2::Integer) == u^(2::Integer))
   [ (x,y,z,u) | u <- [1..], x <- [1..u], y <- [x..u], z <- [y..u] ]

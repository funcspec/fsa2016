
module FSA3 where 

import FSA2 (while,whiler)

fp :: Eq a => (a -> a) -> a -> a 
fp f = until (\ x -> x == f x) f

fbo n = fibo (0,1,n) where 
     fibo = fp (\ (x,y,k) -> if k == 0 then (x,y,k)
                             else (y,x+y,k-1))

bab a = \ x -> ((x + a/x)/2)

sr a = fp (bab a) a

iterateFix :: Eq a => (a -> a) -> a -> [a]
iterateFix f = apprx . iterate f where
  apprx (x:y:zs) = if x == y then [x] else x: apprx (y:zs)

fix :: (a -> a) -> a
fix f = f (fix f)

fbx n = fibo (0,1,n) where
    fibo = fix (\ f (x,y,k) -> if k == 0 then (x,y,k)
                               else f (y,x+y,k-1))

fbb n = fbbb (0,1,n) where 
  fbbb (x,y,n) = if n == 0 then x else fbbb (y,x+y,n-1)

fbc n = fbbc 0 1 n where 
  fbbc x y n = if n == 0 then x else fbbc y (x+y) (n-1)

fp' :: Eq a => (a -> a) -> a -> a
fp' f = fix (\ g x -> if x == f x then x else g (f x))

until' :: (a -> Bool) -> (a -> a) -> a -> a
until' p f = fix (\ g x -> if p x then x else g (f x))

while' :: (a -> Bool) -> (a -> a) -> a -> a
while' p f = fix (\ g x -> if not (p x) then x else g (f x))

apprFact :: (Integer -> Integer) -> Integer -> Integer
apprFact = \ f n -> if n == 0 then 1 else n * f (n-1)

fact = fix apprFact

pre :: (a -> Bool) -> (a -> b) -> a -> b 
pre p f x = if p x then f x else error "pre"

post :: (b -> Bool) -> (a -> b) -> a -> b 
post p f x = if p (f x) then f x else error "post"

decomp :: Integer -> (Integer,Integer)
decomp n = decmp (0,n) where
  decmp = until (odd.snd) (\ (m,k) -> (m+1,div k 2)) 

decompPost :: Integer -> (Integer,Integer)
decompPost = \n -> post (\ (m,k) -> 2^m * k == n) decomp n

assert :: (a -> b -> Bool) -> (a -> b) -> a -> b 
assert p f x = if p x (f x) then f x else error "assert"

decompA :: Integer -> (Integer,Integer)
decompA = assert (\ n (m,k) -> 2^m * k == n) decomp

stepA :: (Integer, Integer) ->  (Integer, Integer)
stepA = assert (\ (m,k) (m',k') -> 2^m*k == 2^m'*k')
                (\ (m,k) -> (m+1,div k 2))

invar :: (a -> Bool) -> (a -> a) -> a -> a
invar p f x = 
  let 
    x' = f x 
  in
   if p x && not (p x') then error "invar" else x'

succI = invar (>0) succ

predI = invar (<0) pred

largestOddFactor =  while even (invar (>0) (`div` 2))

predI' = invar (>0) pred

ext_gcd :: Integer -> Integer -> (Integer,Integer) 
ext_gcd a b = ext_gcd' (a,b,0,1,1,0) where
    ext_gcd' = whiler 
                 (\ (_,b,_,_,_,_) ->  b /= 0) 
                 (\ (a,b,x,y,lastx,lasty) -> let 
                    (q,r)   = quotRem a b 
                    (x',lastx') = (lastx-q*x,x)
                    (y',lasty') = (lasty-q*y,y)
                 in (b,r,x',y',lastx',lasty'))
                 (\ (_,_,_,_,lx,ly) -> (lx,ly))

bezout :: Integer -> Integer -> (Integer,Integer) -> Bool
bezout m n (x,y) = x*m + y*n == euclid m n 

euclid m n = fst $ eucl (m,n) where
     eucl = until (uncurry  (==))
         (\ (x,y) -> if x > y then (x-y,x) else (x,y-x))

ext_gcdA = assert2 bezout ext_gcd

assert2 ::  (a -> b -> c -> Bool) 
             -> (a -> b -> c) -> a -> b -> c
assert2 p f x y = 
  if p x y (f x y) then f x y
  else error "assert2"

fct_gcd :: Integer -> Integer -> (Integer,Integer) 
fct_gcd a b = 
  if b == 0 
  then (1,0) 
  else 
     let 
       (q,r) = quotRem a b
       (s,t) = fct_gcd b r 
     in (t, s - q*t)

fct_gcdA = assert2 bezout fct_gcd


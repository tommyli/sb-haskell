-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?

module Main where

factorsOf :: Integer -> [Integer]
factorsOf 0 = []
factorsOf 1 = [1]
factorsOf n = factorsOf' n (upToRoot n)

factorsOf' :: Integer -> [Integer] -> [Integer]
factorsOf' x [] = [1, x]
factorsOf' x (y:ys) =
  if (isFactor x y) then
    y:(x `div` y):(factorsOf' x ys)
  else
    factorsOf' x ys

isFactor :: Integer -> Integer -> Bool
isFactor number testFactor = number `mod` testFactor == 0

upToRoot :: Integer -> [Integer]
upToRoot n = [2..(round (sqrt $ fromInteger n+1))]
-- upToRoot n = [x | x <- [2..n-1], x*x <= n]  -- has performance issues but don't know why

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = length (factorsOf n) == 2

maxPrimeFactor :: Integer -> Integer
maxPrimeFactor n = maximum (filter isPrime (factorsOf n))

primesUpTo :: Integer -> [Integer]
primesUpTo n = filter isPrime [2..n]

main :: IO ()
--main = putStrLn $ show (maxPrimeFactor 600851475143)
main = putStrLn $ show (primesUpTo 1000000)

-- Very neat way of testing if a number is prime, found on Internet
-- http://www.haskell.org/pipermail/haskell-cafe/2005-December/013130.html
-- and performs very well too.
--prime 1 = False
--prime 2 = True
--prime n = filter (divides n) primes == [] where 
--  numbers = [x | x <- [2..n-1], x*x <= n]
--  primes = filter prime numbers
--divides a b = (mod a b == 0)

-- Another neat way dealing with primes, from lecturer
-- returns the (infinite) list of all primes
-- The sieve of Eratosthenes
all_primes :: [Integer]
all_primes = prime_filter ([2..])

prime_filter :: [Integer] -> [Integer]
prime_filter [] = []
prime_filter (x:xs) =
  x:(prime_filter (filter_multiples x xs))
  
-- filter n xs: filter out multiples of n from xs
filter_multiples :: Integer -> [Integer] -> [Integer]
filter_multiples _ [] = []
filter_multiples n (x:xs)
  | (mod x n) == 0 =
    filter_multiples n xs
  | otherwise =
    x:(filter_multiples n xs)

primes_upto :: Integer -> [Integer]
primes_upto n = primes_upto' n all_primes

primes_upto' :: Integer -> [Integer] -> [Integer]
primes_upto' n (p:ps)
  | p > n = []
  | otherwise = p:(primes_upto' n ps)

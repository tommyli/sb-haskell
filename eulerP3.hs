-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?

import Data.Set (Set)
import qualified Data.Set as Set

factorsOf :: Integer -> Set Integer
factorsOf 0 = Set.empty
factorsOf 1 = Set.singleton 1
factorsOf n =
  let
    sqrtList = [2..(round (sqrt $ fromInteger n+1))]
    factorList = map (factorPair n) sqrtList
  in
    Set.fromList (foldr (\p l -> (fst p):(snd p):l) [] factorList)

factorPair :: Integer -> Integer -> (Integer, Integer)
factorPair number testFactor =
  if isFactor number testFactor == True then
    (testFactor, number `div` testFactor)
  else
    (0,0)

isFactor :: Integer -> Integer -> Bool
isFactor number testFactor =
  if number `mod` testFactor == 0 then True else False

prime 1 = False
prime 2 = True
prime n = filter (isFactor n) primes == [] where 
  numbers = [x | x <- [2..n-1], x*x <= n]
  primes = filter prime numbers

divides a b = (mod a b == 0)

pf :: Integer -> Integer

pf n = Set.findMax (Set.filter prime (factorsOf n))

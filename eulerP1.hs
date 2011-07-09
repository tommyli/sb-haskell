-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.

f3 n = (n `mod` 3) == 0
f5 n = (n `mod` 5) == 0

listMultiple :: [Int] -> [Int]
listMultiple [] = []
listMultiple (x:xs) =
  if (f3 x || f5 x) then
    (x:listMultiple xs)
  else
    listMultiple xs

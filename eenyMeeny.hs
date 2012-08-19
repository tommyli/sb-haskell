eenyMeeny :: Int -> Int -> ([Int], Int)
eenyMeeny 1 _ = ([], 1)
eenyMeeny n k = eenyMeeny' [0..(n-1)] 0 k []

eenyMeeny' :: [Int] -> Int -> Int -> [Int] -> ([Int], Int)
eenyMeeny' (n:[]) _ _ sequence = (sequence, n)
eenyMeeny' ns currPos k sequence =
  eenyMeeny' (removeFromList ns indexToRemove) indexToRemove k $ sequence ++ [ns !! indexToRemove]
  where indexToRemove = (currPos + k - 1) `mod` length ns

removeFromList :: [Int] -> Int -> [Int]
removeFromList [] _ = []
removeFromList xs index = (take (index) xs) ++ (drop (index + 1) xs)

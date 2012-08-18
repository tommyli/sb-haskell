eenyMeeny :: Int -> Int -> ([Int], Int)
eenyMeeny 1 _ = ([], 1)
eenyMeeny n k = eenyMeeny' [0..(n-1)] k k []

eenyMeeny' :: [Int] -> Int -> Int -> [Int] -> ([Int], Int)
eenyMeeny' (n:[]) _ _ winners = (winners, n)
eenyMeeny' (n:ns) currk k winners =
  if currk == 1 then
    eenyMeeny' ns k k (winners ++ [n])
  else
    eenyMeeny' (ns ++ [n]) (currk - 1) k winners

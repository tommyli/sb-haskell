eenyMeeny :: Int -> Int -> ([Int], Int)
eenyMeeny 1 _ = ([], 1)
eenyMeeny n k = eenyMeeny' [0..(n-1)] k k []

eenyMeeny' :: [Int] -> Int -> Int -> [Int] -> ([Int], Int)
eenyMeeny' (n:[]) _ _ sequence = (sequence, n)
eenyMeeny' (n:ns) currk k sequence =
  if currk == 1 then
    eenyMeeny' ns k k (sequence ++ [n])
  else
    eenyMeeny' (ns ++ [n]) (currk - 1) k sequence

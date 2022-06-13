merger :: [String] -> [String] -> [String]
merger xs ys = merger2 [] xs ys

merger2 :: [String] -> [String] -> [String] -> [String]
merger2 acc [] [] = acc
merger2 acc x [] = acc ++ x
merger2 acc [] y = acc ++ y

merger2 acc (x:xs) (y:ys) =
  if (x <= y) then
    merger2 (acc ++ [x]) xs (y:ys)
  else
    merger2 (acc ++ [y]) (x:xs) ys

-- merger3 :: [String] -> [String] -> [String]
-- merger3 [] [] = []
-- merger3 (x:[]) (ys) =
-- numbers1 = 1:map(+1) numbers1
-- numbers2 = 1:map(+2) numbers2
-- items1 = map(show) numbers1
-- items2 = map(show) numbers2
-- items1Take5 = take 5 (map(show) numbers1)
-- items2Take5 = take 5 (map(show) numbers2)
-- take 10 (merger items1 items2)  -- does not work
-- take 10 (merger items1Take5 items2Take5)  -- works

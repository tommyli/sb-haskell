-- Using Nilakantha series to generate sequence of numbers that get closer to PI
-- Reference: http://www.wikihow.com/Calculate-Pi

-- A very hacky way to inverse addition and subtraction, there's got to be a better way!
inverse :: (Num a) => (a -> a -> a) -> (a -> a -> a)
inverse f = if (f 1 1) == 2 then (-) else (+)

-- Applies alternating add or subtract function to nilakSeq to produce PI sequence
piSeq :: (Fractional a) => [a]
piSeq = piSeq' (+) nilakSeq

piSeq' :: (Fractional a) => (a -> a -> a) -> [a] -> [a]
piSeq' f (x:x1:xs) = x:(f x x1):(piSeq' (inverse f) ((f x x1):xs))

-- The actual sequence of fractions to apply
nilakSeq :: (Fractional a) => [a]
nilakSeq = 3:map (4 /) (denomSeq 2)

denomSeq :: (Num a) => a -> [a]
denomSeq n = fst (denomFunc n):denomSeq (snd (denomFunc n))

-- Return denominator and next sequence for Nilakantha series
denomFunc :: (Num a) => a -> (a, a)
denomFunc a = (a * (a + 1) * (a + 2), (a + 2))

import Data.Sequence (Seq, fromFunction)

-- | combinations of k elements among a list
combinationsOf :: Int -> [a] -> [[a]]
combinationsOf 1 as        = map pure as
combinationsOf k as@(x:xs) = 
  run (l-1) (k-1) as $ combinationsOf (k-1) xs
  where
    l = length as
    run :: Int -> Int -> [a] -> [[a]] -> [[a]]
    run n k ys cs 
      | n == k    = map (ys ++) cs
      | otherwise = map (q:) cs ++ run (n-1) k qs (drop dc cs)
      where
        (q:qs) = take (n-k+1) ys
        dc     = product [(n-k+1)..(n-1)] `div` product [1..(k-1)]

-- | generate all permutations of a binary sequence
permutationsBinarySequence :: Int -> Int -> [Seq Int]
permutationsBinarySequence nzeros nones = 
  let n = nzeros + nones in 
    map (binarySequence n) (combinationsOf nones [0 .. n-1])
  where
    binarySequence :: Int -> [Int] -> Seq Int
    binarySequence n combo = fromFunction n f 
      where
        f :: Int -> Int
        f i = fromEnum (i `elem` combo)


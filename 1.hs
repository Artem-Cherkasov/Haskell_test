partitionN :: [a] -> Int -> [[a]]
partitionN [] _ = []
partitionN xs n = if null xs || (length xs /= n && length xs < n * 2 - 1) || n < 0 then [] else partitionN' xs n (length xs)
    where
        partitionN' [] _ _ = []
        partitionN' xs n startLength
            | startLength `mod` n == 0 = take (startLength `div` n) xs : partitionN' (drop (startLength `div` n) xs) n startLength
            | otherwise = take (startLength `div` n + 1) xs : partitionN' (drop (startLength `div` n + 1) xs) n startLength
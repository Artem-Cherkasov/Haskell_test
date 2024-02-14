partitionN :: [a] -> Int -> [[a]]
partitionN [] _ = []
partitionN xs n = take n xs : partitionN (drop n xs) n

main = do
    print(partitionN "Hello world!" 2)
degree :: Int -> Int -> [Int]
degree n start = [start^k | k <- [1..n]]

degreeList :: Int -> Int -> [[Int]]
degreeList n end = [degree n start | start <- [1..end]]

main = do
    print(degreeList 2 4)

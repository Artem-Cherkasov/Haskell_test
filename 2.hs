elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices _ [] = []
elemIndices x ys = findIndices x ys 0
    where 
        findIndices _ [] _ = []
        findIndices x (y:ys) i
            | x == y = i : findIndices x ys (i+1)
            | otherwise = findIndices x ys (i+1)
        
square n = n*n
squares cardinality = [square n | n <- [1..cardinality]]
oddSquares cardinality = [n | n <- squares cardinality, n `mod` 2 /= 0]
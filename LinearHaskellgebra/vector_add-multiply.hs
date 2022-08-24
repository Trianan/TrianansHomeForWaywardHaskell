{- Matrix addition and multiplication -}

addVectors::[Int]->[Int]->[Int] -- Adds two vectors that are not necessarily the same size.
addVectors [] [] = []
addVectors (a:_) [] = [a]
addVectors [] (a:_) = [a]
addVectors (x:xs) (y:ys) = [x + y] ++ addVectors xs ys

scaleVector::Int->[Int]->[Int] -- Multiplies a vector by a scalar.
scaleVector n v = [ x*n | x <- v ]

i_hat scalar = scaleVector scalar [1, 0] -- Identity vectors
j_hat scalar = scaleVector scalar [0, 1]

get_ijScalar (x,y) = -- Numbers in vector as scalars of identity vectors.
    "(" ++ show x ++ "," ++ show y ++ ") = " ++
    "(" ++ show x ++ ")i + " ++ "(" ++ show y ++ ")j = " ++
    show [i_hat x, j_hat y]


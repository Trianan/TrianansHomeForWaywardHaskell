length_recursive ::(Num b) => [a] -> b -- Takes any type of list, returns its length.
length_recursive [] = 0 -- Handles empty lists (to allow recursive definition)
length_recursive (_:xs) = 1 + length_recursive xs


collatz :: Integer -> Integer -- Defines the core operation of the algorithm uses.
collatz x = if (x `mod` 2 == 0) 
    then x `div` 2 -- `div` is integer-supporting division.
    else 3*x + 1 

collatzSeries :: Integer -> [Integer]
collatzSeries 0 = []
collatzSeries 1 = [1,4,2,1] --The function won't terminate if this isn't included.
collatzSeries 4 = [4,2,1]
collatzSeries x = [x] ++ collatzSeries (collatz x)

collatzRange :: [Integer] -> [Int] {- Returns list of numbers of terms obtained through passing each Integer in
 a given range, to the algorithm that produces the types of series made famous by the Collatz Conjecture. -} 
collatzRange [] = [] --Pattern match for edge case of empty list.
collatzRange [x] = [length_recursive (collatzSeries x)] -- ...edge case for singleton.
collatzRange (x:xs) = [length_recursive (collatzSeries x)] ++ collatzRange xs -- This combines [Int]-lists through recursion.

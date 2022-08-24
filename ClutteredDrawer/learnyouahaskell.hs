onlyVowels :: [Char] -> [Char] -- Takes a string and returns one.
onlyVowels str = [ c | c <- str, c `elem` ['a','e','i','o','u','y']]
{- 
    From "str", take each character "c" and compose a string using each "c", 
    if and only if "c" is an element in the set of vowels (y-inclusive).
    Return the modified string which now only includes vowels. 
    -}

expSum :: Int -> Int -> Int -> Int -- Takes 3 ints and returns an int.
expSum a b exp = (a + b)^exp

factorial :: Integer -> Integer -- Takes unbounded integer as argument, and returns one.
factorial n = product [1..n]
factorial' :: Integer -> Integer -- This demonstrates pattern-matching with functions.
factorial' 0 = 1 -- This is a case of specific pattern matching.
factorial' n = n * factorial' (n-1) -- f(0)=1 defined, we now can use recursive definition for rest of cases.

circArea :: Float -> Float -- Floating-point decimal number
circArea r = pi * r^2
circArea' :: Double -> Double -- Floating-point decimanl number (2x precision)
circArea' r = pi * r^2

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a) -- a is part of Num typeclass (all number types)
addVectors a b = (fst a + fst b, snd a + snd b) -- Definition without pattern matching.
addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a) 
addVectors' (x1, y1) (x2, y2) = (x1+x2, y1+y2) -- Pattern matching to define a function.

{-
    This demonstrates expanding the operations available for pairs, to triples.
    "_" is used to ignore values in that position while pattern matching.
-}
first :: (a, b, c) -> a -- Returns first element from a triple
first (x, _, _) = x -- Returns first element, ignores others (doesn't store them)

second :: (a, b, c) -> b -- Returns second...
second (_, y, _) = y

third :: (a, b, c) -> c -- Returns third...
third (_, _, z) = z

hypotenuse :: (Floating a) => [(a, a)] -> [a] -- Pattern matching with lists
hypotenuse sides = [sqrt(a^2 + b^2) | (a,b) <- sides] -- Returns hypotenuse list from pairs of sides list.

head' :: [a] -> a
head' [] = error "Called head on empty list."
head' (x:_) = x -- Returns first element in list

tell :: (Show a) => [a] -> String -- More pattern matching with lists
tell [] = "Empty" --Handles empty lists.
tell (x:[]) = "One: " ++ show x --Matches singleton lists.
tell (x:y:[]) = "Two: " ++ show x ++ " and " ++ show y --Matches with two-element lists.
tell (x:y:_) = "More than two elements." --Matches any list with greater than two elements.

length_recursive ::(Num b) => [a] -> b -- Takes any type of list, returns its length.
length_recursive [] = 0 -- Handles empty lists (to allow recursive definition)
length_recursive (_:xs) = 1 + length_recursive xs 
{- Takes and discards first element, increments 1, 
    then calls itself on remaining list until empty. -}

{- This takes a list of number types, and returns their sum.
    The edge-case of an empty-list is defined as returning the sum 0, through pattern matching.
    Anything that is not an empty list is handled by the next pattern.
    It is defined as the first element, plus the sum of the rest of the list, through recursion. -}
seriesSum :: (Num n) => [n] -> n 
seriesSum [] = 0
seriesSum (n:ns) = n + seriesSum ns

{- This demonstrates "as-patterns", which tie a name to a pattern used for matching,
    to allow reference to the entire object of reference instead of strictly its matched elements. -}
firstLetter :: String -> String
firstLetter "" = "This is an empty string, jerk."
firstLetter str@(c:cs) = "The first letter in " ++ str ++ " is " ++ [c] ++ ". "

{- This demonstrates "guards"; a useful alternative to if-else trees.
   Alse demonstrates "where"-bindings in functions, 
   which define local names/patterns/functions/etc.
   "Where"-bindings can also be nested (for example when defining helper functions.) -}
gleEquals :: (Ord a, Eq a, Show a) => a -> a -> String -- "Greater-than, less-than, equals" operator.
a `gleEquals` b -- Defining function using infixed-function backticks.
    | (a > b) = show a ++ gt ++ show b -- "show" turns Int into String.
    | (a < b) = show a ++ lt ++ show b
    | (a == b) = show a ++ eq ++ show b
    | otherwise = "What?!" -- Catches any other case.
    where gt = " is greater than "
          lt = " is less than "
          eq = " is equal to "

{- Demonstration of "let-be" expressions, "let" defines local names/functions/etc,
    "in" defines the expression in which the names are used. Inline they are seperated
    by colons. -}
squareTriple :: (Floating f) => [(f,f,f)] -> [(f,f,f)]
squareTriple ((a, b, c):_) = [let sqr n = n*n in (sqr a, sqr b, sqr c)]


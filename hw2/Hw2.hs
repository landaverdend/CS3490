module Hw2 where 

--Count N function
countInt :: Integer -> [Integer] -> Integer
countInt n [] = 0
countInt n (x:xs)
        | (n == x) = 1 + countInt n xs
        | otherwise = 0 + countInt n xs

--Count Zero function
countZeros :: [Integer] -> Integer 
countZeros xs  = countInt 0 xs

--Append function
append :: [a] -> [a] -> [a]
append [] x = x
append (x:xs) y = x : append xs y

--CountEq function
countEq :: Eq a => a -> [a] -> Integer
countEq x [] = 0
countEq x (y:ys)
        | (x == y) = 1 + countEq x ys
        | otherwise = 0 + countEq x ys

--Non-negatives function
nonnegatives :: [Integer] -> [Integer]
nonnegatives [] = []
nonnegatives (x:xs)
        | x >= 0    = x : nonnegatives xs
        | otherwise = nonnegatives xs

--odds only function
oddsOnly :: [Integer] -> [Integer]
oddsOnly [] = []
oddsOnly (x:xs)
        | (x `mod` 2) == 1 = x : oddsOnly xs
        | otherwise        = oddsOnly xs

--Double all function
doubleAll :: [Integer] -> [Integer]
doubleAll [] = []
doubleAll (x:xs) = (2 * x) : doubleAll xs

--Commafy function.
commafy :: [Integer] -> String
commafy [] = "" 
commafy [x] = show x
commafy (x:xs) 
        | (tail xs) == [] = "" ++ (show x) ++ "," ++ (show (last xs)) --there has to be a better way.
        | otherwise       = (show x) ++ "," ++ commafy xs

--Skip even function.
skipEven :: [a] -> [a]
skipEven [] = []
skipEven [x] = []
skipEven (x1:x2:xs) = x2 : skipEven xs

--Skip odd function.
skipOdd :: [a] -> [a]
skipOdd [] = []
skipOdd [x] = [x]
skipOdd (x1:x2:xs) = x1 : skipOdd xs

--Flatten function.
flatten' :: [[a]] -> [a]
flatten' [] = []
flatten' [[]] = []
flatten' (a:as) =  a ++ flatten' as



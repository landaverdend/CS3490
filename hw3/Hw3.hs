module Hw3 where 

--Count Eq
countEq :: Eq a => a -> [a] -> Integer
countEq x l = foldl (\i elem -> if elem == x then i+1 else i) 0 l 

--duplicate 
dup' :: [a] -> [a]
dup' xs = foldr f [] xs
    where f x y = x:x:y

--all
all' :: (a -> Bool) -> [a] -> Bool
all' p xs = foldr (\x y -> p(x) && y) True xs

--nonnegatives
nonnegatives :: [Integer] -> [Integer]
nonnegatives xs = foldr f [] xs
    where f x y = if x < 0 then y else x:y

--MatchInt 
matchInt :: [(Integer, String)] -> Integer
matchInt xs = foldr f 0 xs 
    where f elem acc = if checkMatch elem then acc + 1 else acc

--helper for matchInt 
checkMatch :: (Integer, String) -> Bool
checkMatch xs = show(fst(xs)) == snd xs
module Hw4 where 
data LTree a = LLeaf a | LNode (LTree a) a (LTree a) deriving Show


myTree :: LTree Integer
myTree = (LNode (LNode (LLeaf 2) 2 (LLeaf 1)) 4 (LLeaf 0))


--countLeaves recursive
countLeaves :: LTree a -> Integer
countLeaves (LLeaf a) = 1 
countLeaves (LNode l a r) = countLeaves(l) + countLeaves(r)

--countEvenNodes recursive
countEvenNodes :: LTree Integer -> Integer
countEvenNodes (LLeaf a) = 0
countEvenNodes (LNode l a r)
                | (a `mod` 2) == 0 = 1 + countEvenNodes(l) + countEvenNodes(r)
                | otherwise    = countEvenNodes(l) + countEvenNodes(r)

--minTree recursive
minTree :: LTree Integer -> Integer
minTree (LLeaf a) = a
minTree (LNode l a r) 
                | a < min (minTree(l)) (minTree(r)) = a
                | otherwise                         = min (minTree(l)) (minTree(r))
 
--occursInLeaves recursive
occursInLeaves :: (Eq a) => a -> LTree a -> Bool
occursInLeaves findMe (LLeaf a) = (a == findMe)
occursInLeaves findMe (LNode l a r) = (occursInLeaves(findMe) (l)) || (occursInLeaves(findMe) (r))

--checkNoCover recursive
checkNoCover :: (Eq a) => a -> LTree a -> Bool
checkNoCover findMe (LLeaf a) = (a == findMe)
checkNoCover findMe (LNode l a r) 
                | a == findMe = False
                | otherwise   = (checkNoCover(findMe) (l)) || (checkNoCover(findMe)(r))



--FOLD BELOW
             --b = Tree type, a = data shtuff
foldTree :: (b -> a -> b -> b) -> (a -> b) -> LTree a -> b
foldTree comb base (LLeaf x) = base x
foldTree comb base (LNode t1 y t2) = comb (foldTree comb base t1) y (foldTree comb base t2)

--minTree2 fold 
minTree :: LTree Integer -> Integer
minTree = foldTree (\l a r -> min(a) (min l r))  (\x -> x)
                        --this is for nodes        this is for leaves
--CountLeaves2 fold
countLeaves :: LTree a -> Integer
countLeaves = foldTree (\l a r -> l + r) (\a -> 1)

--Count Even Nodes fold
countEvenNodes :: LTree Integer -> Integer
countEvenNodes = foldTree (\l a r -> if (a `mod` 2 == 0)
                                      then 1 + l + r
                                      else l + r) (\x -> 0) 

--Occurs in Leaves fold
occursInLeaves :: (Eq a) => a -> LTree a -> Bool
occursInLeaves (findMe) = foldTree (\l a r -> l || r) (\x -> if (x == findMe) then True else False)  

--checkNoCover fold
checkNoCover :: (Eq a) => a -> LTree a -> Bool
checkNoCover (findMe) = foldTree (\ l a r -> if (a == findMe)
                                             then False
                                             else l || r) (\x -> x == findMe) 

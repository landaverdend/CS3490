-- To test your solutions, run: 
-- ghci> :l hw4grader
-- ghci> runTestTT testAll
--
-- This assumes that the file hw4.hs exists in the same directory,
-- and that the file begins with the line "module Hw4 where"

module HW4Grader where

import Test.HUnit
import Hw4

intEmpty :: [Integer]
intEmpty = []

tree1 :: LTree Integer
tree1 = LNode (LNode (LLeaf 3) 2 (LLeaf 4))
              1
              (LNode (LLeaf 6) 5 (LNode (LLeaf 9) 7 (LLeaf 10)))
tree2 :: LTree Integer
tree2 = LNode (LLeaf 0) 1 (
          LNode (LLeaf 0) 2 (
            LNode (LLeaf 0) 3 (
              LNode (LLeaf 0) 4 (LLeaf 0) )))
tree3 :: LTree Integer
tree3 = LNode (LNode (LLeaf 1) 2 (LNode (LLeaf 2) 3 (LLeaf 1)))
              1
              (LNode (LLeaf 2) 4 (LLeaf (-1)))
tree4 :: LTree Char
tree4 = LNode (LLeaf 'a') 'b' (LLeaf 'c')
tree5 :: LTree String
tree5 = LLeaf "hello"

minTreeTest = TestList [ TestCase (assertEqual "minTree1" 0 (minTree tree2))
                       , TestCase (assertEqual "minTree2" 1 (minTree tree1))
                       , TestCase (assertEqual "minTree3" (-1) (minTree tree3))
                       , TestCase (assertEqual "minTree4" 100 (minTree (LLeaf 100)))
                       ]
countLeavesTest = TestList [ TestCase (assertEqual "countLeaves1" 5 (countLeaves tree1))
                           , TestCase (assertEqual "countLeaves2" 5 (countLeaves tree2))
                           , TestCase (assertEqual "countLeaves3" 5 (countLeaves tree3))
                           , TestCase (assertEqual "countLeaves4" 1 (countLeaves tree5))
                           ]
countEvenNodesTest = TestList [ TestCase (assertEqual "countEvenNodes1" 1 (countEvenNodes tree1))
                              , TestCase (assertEqual "countEvenNodes2" 2 (countEvenNodes tree2))
                              , TestCase (assertEqual "countEvenNodes3" 2 (countEvenNodes tree3))
                              , TestCase (assertEqual "countEvenNodes4" 0 (countEvenNodes (LLeaf 0)))
                              ]
occursInLeavesTest = TestList [ TestCase (assertEqual "occursInLeaves1" False (occursInLeaves 1 tree1))
                              , TestCase (assertEqual "occursInLeaves2" True  (occursInLeaves 3 tree1))
                              , TestCase (assertEqual "occursInLeaves3" True  (occursInLeaves 1 tree3))
                              , TestCase (assertEqual "occursInLeaves4" True  (occursInLeaves 'c' tree4))
                              ]
checkNoCoverTest = TestList [ TestCase (assertEqual "checkNoCover1" True (checkNoCover 3 tree1))
                            , TestCase (assertEqual "checkNoCover2" False (checkNoCover 1 tree3))
                            , TestCase (assertEqual "checkNoCover3" True (checkNoCover 2 tree3))
                            , TestCase (assertEqual "checkNoCover4" True (checkNoCover "hello" tree5))
                            ]
testAll = TestList [minTreeTest, countLeavesTest, countEvenNodesTest, occursInLeavesTest, checkNoCoverTest]

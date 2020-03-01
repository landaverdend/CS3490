-- To use: define functions in Hw3.hs, which must begin with "module Hw3 where"
-- Then load this file in ghci and run:
-- ghci> runTestTT testAll

module Hw3grader where

import Test.HUnit
import Hw3

intEmpty :: [Integer]
intEmpty = []

testDup1 = TestCase (assertEqual "Dup1" intEmpty (dup' []))
testDup2 = TestCase (assertEqual "Dup2" [1,1] (dup' [1]))
testDup3 = TestCase (assertEqual "Dup3" "aabb" (dup' "ab"))
testDup4 = TestCase (assertEqual "Dup4" [[1],[1],[2],[2]] (dup' [[1],[2]]))

testDups = TestList [testDup1, testDup2, testDup3, testDup4 ]

testCountEq1 = TestCase (assertEqual "countEq1" 0 (countEq 0 [1..10]))
testCountEq2 = TestCase (assertEqual "countEq2" 1 (countEq 0 [-10..10]))
testCountEq3 = TestCase (assertEqual "countEq3" 5 (countEq 5 [5,5,5,5,5]))
testCountEq4 = TestCase (assertEqual "countEq4" 2 (countEq 3 [1,2,3,4,3,2,1]))

testCountEqs = TestList [testCountEq1, testCountEq2, testCountEq3, testCountEq4]

testAll1 = TestCase (assertEqual "all1" True (all' even intEmpty))
testAll2 = TestCase (assertEqual "all2" True (all' even [2,4,6]))
testAll3 = TestCase (assertEqual "all3" False (all' even [1..10]))
testAll4 = TestCase (assertEqual "all4" True (all' (== 'a') "aaaaa"))

testAlls = TestList [testAll1, testAll2, testAll3, testAll4]

testNonneg1 = TestCase (assertEqual "Nonneg1" [] (nonnegatives [-3,-2,-1]))
testNonneg2 = TestCase (assertEqual "Nonneg2" [0,1,2,3] (nonnegatives [-3,-2,-1,0,1,2,3]))
testNonneg3 = TestCase (assertEqual "Nonneg3" [0,1,177] (nonnegatives [-100,-10,-1,0,1,177]))
testNonneg4 = TestCase (assertEqual "Nonneg4" [0..10] (nonnegatives [-100..10]))

testNonnegs = TestList [testNonneg1,testNonneg2,testNonneg3,testNonneg4]

testMatch1 = TestCase (assertEqual "match1" 0   (matchInt [(0,"zero")]))
testMatch2 = TestCase (assertEqual "match2" 2   (matchInt [(0,"0"),(10,"10")]))
testMatch3 = TestCase (assertEqual "match3" 100 (matchInt (map (\x -> (x, show x)) [1..100])))
testMatch4 = TestCase (assertEqual "match4" 3   (matchInt [(0,"badstring"),(100,"100"),(3,"3"),(10,"ten"),(-1,"-1")]))

testMatchs = TestList [testMatch1, testMatch2, testMatch3, testMatch4]

testAll = TestList [testDups, testCountEqs, testAlls, testNonnegs, testMatchs]

-- To use: define functions in Hw2.hs, which must begin with "module Hw2 where"
-- Then load this file in ghci and run:
-- ghci> runTestTT testAll

module Hw2grader where

import Test.HUnit
import Hw2

countIntTest = TestList [
  TestCase (assertEqual "countInt1" 0 (countInt 0 [])),
  TestCase (assertEqual "countInt2" 1 (countInt 0 [0])),
  TestCase (assertEqual "countInt3" 1 (countInt 1 [1..100])),
  TestCase (assertEqual "countInt4" 3 (countInt 2 [1,2,3,2,1,2,3]))]

countEqTest = TestList [
  TestCase (assertEqual "countEq1" 0 (countEq 0 [1..10])),
  TestCase (assertEqual "countEq2" 1 (countEq 1 [1..10])),
  TestCase (assertEqual "countEq3" 2 (countEq 'l' "Hello")),
  TestCase (assertEqual "countEq4" 3 (countEq 'o' "Morocco"))]

appendTest = TestList [
  TestCase (assertEqual "append1" [1] (append [1] [])),
  TestCase (assertEqual "append2" [1..6] (append [1..3] [4..6])),
  TestCase (assertEqual "append3" "Helloworld" (append "Hello" "world")),
  TestCase (assertEqual "append4" "Noee" (append "" "Noee"))]

skipEvenTest = TestList [
  TestCase (assertEqual "skipEven1" [] (skipEven [1])),
  TestCase (assertEqual "skipEven2" [2] (skipEven [1,2])),
  TestCase (assertEqual "skipEven3" [2,4..10] (skipEven [1..10])),
  TestCase (assertEqual "skipEven4" "el,wrd" (skipEven "Hello, world"))]

skipOddTest = TestList [
  TestCase (assertEqual "skipOdd1" [1] (skipOdd [1])),
  TestCase (assertEqual "skipOdd2" [1] (skipOdd [1,2])),
  TestCase (assertEqual "skipOdd3" [1,3..10] (skipOdd [1..10])),
  TestCase (assertEqual "skipOdd4" "Hlo ol" (skipOdd "Hello, world"))]

flattenTest = TestList [
  TestCase (assertEqual "flatten1" [1] (flatten' [[1]])),
  TestCase (assertEqual "flatten2" [1,2,3,4] (flatten' [[1],[2],[3,4]])),
  TestCase (assertEqual "flatten3" "c" (flatten' [[],[],"c",[]])),
  TestCase (assertEqual "flatten4" "Hello world" (flatten' ["Hello"," ","world"]))]

nonnegativesTest = TestList [
  TestCase (assertEqual "nonneg1" [1,2,3] (nonnegatives [1,2,3])),
  TestCase (assertEqual "nonneg2" [0,1,2,3] (nonnegatives [-3..3])),
  TestCase (assertEqual "nonneg3" [2,1,0] (nonnegatives [2,1,0,-1,-2])),
  TestCase (assertEqual "nonneg4" [1,1,1] (nonnegatives [1,-1,1,-1,1])) ]

oddsOnlyTest = TestList [
  TestCase (assertEqual "oddsOnly1" [] (oddsOnly [2,4..20])),
  TestCase (assertEqual "oddsOnly2" [-9,-7..9] (oddsOnly [-10..10])),
  TestCase (assertEqual "oddsOnly3" [1,-1,1,-1,1] (oddsOnly [1,0,-1,0,1,0,-1,0,1])),
  TestCase (assertEqual "oddsOnly4" (filter odd [123..1234]) (oddsOnly [123..1234])) ]

doubleAllTest = TestList [
  TestCase (assertEqual "doubleAll1" [] (doubleAll [])),
  TestCase (assertEqual "doubleAll2" [2,4,6] (doubleAll [1,2,3])),
  TestCase (assertEqual "doubleAll3" [-20,-18..20] (doubleAll [-10..10])),
  TestCase (assertEqual "doubleAll4" [0,0,2] (doubleAll [0,0,1])) ]

commafyTest = TestList [
  TestCase (assertEqual "commafy1" "12" (commafy [12])),
  TestCase (assertEqual "commafy2" "1,2" (commafy [1,2])),
  TestCase (assertEqual "commafy3" "" (commafy [])),
  TestCase (assertEqual "commafy4" "1,2,3,4,5" (commafy [1..5])) ]

testAll = TestList [ countIntTest, countEqTest, appendTest, skipEvenTest, skipOddTest,
            flattenTest, nonnegativesTest, oddsOnlyTest, doubleAllTest, commafyTest ]

module Main where

import Test.Tasty
import Test.Tasty.HUnit

import HW01

main :: IO ()
main = do
  defaultMain (testGroup "HW1 Tests" [toDigitsTest1, toDigitsTest2, toDigitsTest3, toDigitsRevTest,
      doubleEveryOtherTest1, doubleEveryOtherTest2, sumDigitsTest, validateTest1, validateTest2, hanoiTest])

toDigitsTest1 :: TestTree
toDigitsTest1 = testCase "Testing toDigits"
  (assertEqual "toDigits 1234 == [1,2,3,4]" [1,2,3,4] (toDigits 1234))

toDigitsTest2 :: TestTree
toDigitsTest2 = testCase "Testing toDigits"
  (assertEqual "toDigits 0 == []" [] (toDigits 0))

toDigitsTest3 :: TestTree
toDigitsTest3 = testCase "Testing toDigits"
  (assertEqual "toDigits (-17) == []" [] (toDigits (-17)))

toDigitsRevTest :: TestTree
toDigitsRevTest = testCase "Testing toDigitsRev"
  (assertEqual "toDigits 1234 == [4,3,2,1]" [4,3,2,1] (toDigitsRev 1234))

doubleEveryOtherTest1 :: TestTree
doubleEveryOtherTest1 = testCase "Testing doubleEveryOther"
  (assertEqual "doubleEveryOther [8,7,6,5] == [16,7,12,5]" [16,7,12,5] (doubleEveryOther [8,7,6,5]))

doubleEveryOtherTest2 :: TestTree
doubleEveryOtherTest2 = testCase "testing doubleEveryOther"
  (assertEqual "doubleEveryOther [1,2,3] == [1,4,3]" [1,4,3] (doubleEveryOther [1,2,3]))

sumDigitsTest :: TestTree
sumDigitsTest = testCase "Testing sumDigitsTest" 
  (assertEqual "sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22" 22 (sumDigits [16,7,12,5]))

validateTest1 :: TestTree
validateTest1 = testCase "Testing validate"
  (assertEqual "validate 4012888888881881 = True" True (validate 4012888888881881))

validateTest2 :: TestTree
validateTest2 = testCase "Testing validate"
  (assertEqual "validate 4012888888881882 = False" False (validate 4012888888881882))

hanoiTest :: TestTree
hanoiTest = testCase "Testing hanoi"
  (assertEqual "hanoi 2 \"a\" \"b\" \"c\"" [("a","c"), ("a","b"), ("c","b")] (hanoi 2  "a" "b" "c"))

module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Golf

main :: IO ()
main = do
  defaultMain (testGroup "HW3 Test" [skipsTest1, skipsTest2, skipsTest3, skipsTest4, localMaximaTest1, localMaximaTest2, localMaximaTest3])

skipsTest1 :: TestTree
skipsTest1 = testCase "Testing skipsTest"
  (assertEqual "skips \"ABCD\"" ["ABCD", "BD", "C", "D"] (skips "ABCD"))

skipsTest2 :: TestTree
skipsTest2 = testCase "Testing skipsTest"
  (assertEqual "skips \"hello!\"" ["hello!", "el!", "l!", "l", "o", "!"] (skips "hello!"))

skipsTest3 :: TestTree
skipsTest3 = testCase "Testing skipsTest"
  (assertEqual "skips [1]" [[1]] (skips [1])) 
   
skipsTest4 :: TestTree
skipsTest4 = testCase "Testing skipsTest"
  (assertEqual "skips [True, False]" [[True,False], [False]] (skips [True,False]))

{-skipsTest5 :: TestTree
skipsTest5 = testCase "Testing skipsTest"
  (assertEqual "skips []" [] (skips []))-}

localMaximaTest1 :: TestTree
localMaximaTest1 = testCase "Testing localMaxima"
  (assertEqual "localMaxima [2,9,5,6,1]" [9,6] (localMaxima [2,9,5,6,1]))

localMaximaTest2 :: TestTree
localMaximaTest2 = testCase "Testing localMaxima"
  (assertEqual "localMaxima [2,3,4,1,5]" [4] (localMaxima [2,3,4,1,5]))

localMaximaTest3 :: TestTree
localMaximaTest3 = testCase "Testing localMaxima"
  (assertEqual "localMaxima [1,2,3,4.5]" [] (localMaxima [1,2,3,4,5]))


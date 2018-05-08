module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Golf

main :: IO ()
main = do
  defaultMain (testGroup "HW3 Test" [skipsTest1, skipsTest2, skipsTest3, skipsTest4])

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

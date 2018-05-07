module Main where

import Test.Tasty
import Test.Tasty.HUnit

import LogAnalysis
import Log

main :: IO ()
main = do
  defaultMain (testGroup "HW2 Tests" [parseTest1, parseTest2, parseTest3])

parseTest1 :: TestTree
parseTest1 = testCase "Testing parseMessage"
  (assertEqual "parseMessage \"E 2 562 help help\"" (LogMessage (Error 2) 562 "help help") (parseMessage "E 2 562 help help"))

parseTest2 :: TestTree
parseTest2 = testCase "Testing parseMessage"
  (assertEqual "parseMessage \"I 29 la la la\"" (LogMessage Info 29 "la la la") (parseMessage "I 29 la la la"))

parseTest3 :: TestTree
parseTest3 = testCase "Testing parseMessage"
  (assertEqual "parseMessage \"This is not in the right format\"" (Unknown "This is not in the right format") (parseMessage "This is not in the right format"))

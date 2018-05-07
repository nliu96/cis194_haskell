{-# OPTIONS_GHC -Wall #-}
module LogAnalysis 
(
  parseMessage
) where

import Log

parseMessage :: String -> LogMessage
parseMessage x = case (words x) of
  ("I":time:message) -> LogMessage Info (read time) (unwords message)
  ("W":time:message) -> LogMessage Warning (read time) (unwords message)
  ("E":sev:time:message) -> LogMessage (Error (read sev)) (read time) (unwords message)
  _ -> Unknown x

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown  _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ time _) tree@(Node left nodeMsg@(LogMessage _ nodeTime _) right)
  | time > nodeTime = Node left nodeMsg (insert msg right)
  | time < nodeTime = Node (insert msg left) nodeMsg right
  | otherwise = tree

build :: [LogMessage] -> MessageTree
build [] = Leaf
build [x] = Node Leaf x Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ((LogMessage (Error sev) _ msg):logMsgs)
  | sev > 50 = [msg] ++ (whatWentWrong logMsgs)
  | otherwise = whatWentWrong logMsgs

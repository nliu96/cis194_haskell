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

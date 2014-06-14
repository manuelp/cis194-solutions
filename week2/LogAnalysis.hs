{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage s = case (words s) of
  ("I":ts:msg) -> LogMessage Info (read ts) (unwords msg)
  ("W":ts:msg) -> LogMessage Warning (read ts) (unwords msg)
  ("E":code:ts:msg) -> LogMessage (Error (read code)) (read ts) (unwords msg)
  _ -> Unknown s

parse :: String -> [LogMessage]
parse s = map (parseMessage) (lines s)

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ ts _) (Node left lg@(LogMessage _ t _) right)
  | ts <= t = (Node (insert m left) lg right)
  | otherwise = (Node left lg (insert m right))
insert m (Node _ (Unknown _) right) = insert m right

-- Exercise 3
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (m:ms) = insert m (build ms)

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) = (inOrder left) ++ [m] ++ (inOrder right)

-- Exercise 5
interestingMessages :: LogMessage -> [String]
interestingMessages (LogMessage (Error s) _ msg)
  | s >= 50 = [msg]
  | otherwise = []
interestingMessages _ = []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong (m:ms) = interestingMessages m ++ whatWentWrong ms

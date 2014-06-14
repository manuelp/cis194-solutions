{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1
type Token = String

parseMessage :: [Token] -> LogMessage
parseMessage ("I":ts:msg) = LogMessage Info (read ts) (unwords msg)
parseMessage ("W":ts:msg) = LogMessage Warning (read ts) (unwords msg)
parseMessage ("E":code:ts:msg) = LogMessage (Error (read code)) (read ts) (unwords msg)
parseMessage l = Unknown (unwords l)

parse :: String -> [LogMessage]
parse s = map (parseMessage . words) (lines s)

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
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ((LogMessage (Error s) _ msg):ms)
  | s >= 50 = msg : whatWentWrong ms
  | otherwise = whatWentWrong ms
whatWentWrong (_:ms) = whatWentWrong ms

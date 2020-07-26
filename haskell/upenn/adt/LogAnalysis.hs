{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import qualified Data.List as List

parseMessage :: String -> LogMessage
parseMessage [] = Unknown ""
parseMessage (x:xs) = case x of 'I' -> LogMessage Info (time wlist) (str wlist)
                                'W' -> LogMessage Warning (time wlist) (str wlist)
                                'E' -> LogMessage (Error (time wlist)) (time (tail wlist)) (str $ tail wlist)
                                (_)  -> Unknown (x:xs)
   where wlist = List.words xs
         time l = (read (head l) :: Int)
         str l = List.unwords $ tail l

parse :: String -> [LogMessage]
parse x = map parseMessage $ List.lines x

insert :: LogMessage -> MessageTree -> MessageTree
insert m Leaf = Node Leaf m Leaf
insert (Unknown _) tree = tree
insert _ t@(Node _ (Unknown _) _) = t -- will never happen, to make compiler happy
insert m@(LogMessage _ time _) (Node lhs r@(LogMessage _ rt _) rhs)
   | time > rt = Node lhs r (insert m rhs)
   | otherwise = Node (insert m lhs) r rhs

build :: [LogMessage] -> MessageTree
build msgs = foldr insert Leaf msgs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf m Leaf) = [m]
inOrder (Node lhs m rhs) = (inOrder lhs) ++ [m] ++ (inOrder rhs)

filterErrors :: [LogMessage] -> [LogMessage]
filterErrors [] = []
filterErrors (l@(LogMessage (Error e) _ _):xs) = if (e >= 50) then ([l] ++ res) else res
   where res = filterErrors xs
filterErrors (_:xs) = filterErrors xs

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = map (\(LogMessage (Error _) _ s) -> s)  fes
   where smsgs = inOrder $ build msgs
         fes = filterErrors $ smsgs

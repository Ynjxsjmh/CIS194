{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1 -----------------------------------------

-- Other functions which may (or may not) be useful to you include lines, words, unwords, take, drop, and (.).

parseMessage :: String -> LogMessage
parseMessage sentence = case words sentence of
                          (x:y:wordList) -> case x of
                                              "E" -> LogMessage (Error (read y :: Int)) (read (head wordList) :: Int) (unwords (drop 1 wordList))
                                              "I" -> LogMessage Info (read y :: Int) (unwords wordList)
                                              "W" -> LogMessage Warning (read y :: Int) (unwords wordList)
                                              _  -> Unknown sentence
                          _ -> Unknown sentence




parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)


-- Exercise 2 -----------------------------------------
module HsBlog.Markup (Document, Structure (..), parse) where

import Data.Maybe

type Document = [Structure]

type Lines = [String]

data Structure
  = Heading Int String
  | Paragraph String
  | OrderedList [String]
  | UnorderedList [String]
  | CodeBlock [String]
  deriving (Show, Eq)

parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> Lines -> Document
parseLines ast [] = maybeToList ast
-- Any varying level
--  Note Html only supports up to 6
parseLines ast (('*' : line) : rest) = concatNode ast $ heading : continue
  where
    heading = Heading lvl content
    lvl = (+ 1) . length . takeWhile (== '*') $ line
    content = trim . dropWhile (== '*') $ line
    continue = parseLines Nothing rest
-- Unordered List Case
parseLines ast (('-' : ' ' : line) : rest) = case ast of
  Just (UnorderedList list) -> parseLines (Just . UnorderedList $ list <> [line]) rest
  _anyOtherFailure -> concatNode ast $ parseLines (Just . UnorderedList $ [line]) rest
-- Ordered List Case
parseLines ast (('#' : ' ' : line) : rest) = case ast of
  Just (OrderedList list) -> parseLines (Just . OrderedList $ list <> [line]) rest
  _anyOtherFailure -> concatNode ast $ parseLines (Just . OrderedList $ [line]) rest
-- Code Block Case
parseLines ast (('>' : ' ' : line) : rest) = case ast of
  Just (CodeBlock code) -> parseLines (Just . CodeBlock $ code <> [line]) rest
  _anyOtherFailure -> concatNode ast $ parseLines (Just . CodeBlock $ [line]) rest
-- Paragraph Case
parseLines ast (curLine : rest)
  | line == "" = concatNode ast $ parseLines Nothing rest
  | otherwise = case ast of
    Just (Paragraph par) -> parseLines (Just . Paragraph . unwords $ [par, line]) rest
    _anyOtherFailure -> concatNode ast $ parseLines (Just . Paragraph $ line) rest
  where
    line = trim curLine

trim :: String -> String
trim = unwords . words

concatNode :: Maybe Structure -> [Structure] -> [Structure]
concatNode = maybe id (:)
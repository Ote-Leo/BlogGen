module HsBlog.Html.Internal where

newtype Html = Html String

newtype Structure = Structure String

instance Semigroup Structure where
  (<>) (Structure x) (Structure y) = Structure (x <> y)

instance Monoid Structure where
  mempty = empty_

type Title = String

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

html_ :: Title -> Structure -> Html
html_ title content = Html . docComp $ docHead <> docBody
  where
    docComp = el "html" . getContent
    docHead = head_ . getContent . title_ . escape $ title
    docBody = body_ . getContent $ content

structConst :: String -> String -> Structure
structConst name = Structure . el name

body_ :: String -> Structure
body_ = structConst "body"

head_ :: String -> Structure
head_ = structConst "head"

title_ :: String -> Structure
title_ = structConst "title"

p_ :: String -> Structure
p_ = structConst "p"

h_ :: Int -> String -> Structure
h_ = structConst . (<>) "h" . show

render :: Html -> String
render (Html x) = x

getContent :: Structure -> String
getContent (Structure x) = x

escape :: String -> String
escape =
  let escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c]
   in concatMap escapeChar

list_ :: String -> [Structure] -> Structure
list_ typ = Structure . el typ . concatMap (el "li" . getContent)

ol_ :: [Structure] -> Structure
ol_ = list_ "ol"

ul_ :: [Structure] -> Structure
ul_ = list_ "ul"

b_ :: String -> Structure
b_ = structConst "b"

code_ :: String -> Structure
code_ = structConst "pre"

empty_ :: Structure
empty_ = Structure ""

concatStruct :: [Structure] -> Structure
concatStruct lst = case lst of
  [] -> empty_
  x : xs -> x <> concatStruct xs

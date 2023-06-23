module HsBlog.Html.Internal where

-- * Types

newtype Html = Html String

newtype Structure = Structure String

newtype Content = Content String

type Title = String

-- * EDSL

html_ :: Title -> Structure -> Html
html_ title content = Html . docComp $ docHead <> docBody
  where
    docComp = el "html" . getContent
    docHead = head_ . getContent . title_ . escape $ title
    docBody = body_ . getContent $ content

-- * Structure

body_ :: String -> Structure
body_ = structConst "body"

head_ :: String -> Structure
head_ = structConst "head"

title_ :: String -> Structure
title_ = structConst "title"

p_ :: Content -> Structure
p_ = structConst "p" . getContentString

h_ :: Int -> Content -> Structure
h_ n = Structure . el ("h" <> show n) . getContentString

ol_ :: [Structure] -> Structure
ol_ = list_ "ol"

ul_ :: [Structure] -> Structure
ul_ = list_ "ul"

code_ :: String -> Structure
code_ = structConst "pre" . escape

instance Semigroup Structure where
  (<>) (Structure x) (Structure y) = Structure (x <> y)

instance Monoid Structure where
  mempty = empty_

-- * Content

txt_ :: String -> Content
txt_ = Content . escape

link_ :: FilePath -> Content -> Content
link_ path content = Content $ elAttr "a" ("href=\"" <> escape path <> "\"") (getContentString content)

img_ :: FilePath -> Content
img_ path = Content $ "<img src=\"" <> escape path <> "\">"

b_ :: Content -> Content
b_ content = Content $ el "b" (getContentString content)

i_ :: Content -> Content
i_ content = Content $ el "i" (getContentString content)

instance Semigroup Content where
  (Content c1) <> (Content c2) = Content (c1 <> c2)

instance Monoid Content where
  mempty = Content ""

-- * Render

render :: Html -> String
render (Html x) = x

-- * Utilities

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

elAttr :: String -> String -> String -> String
elAttr = undefined

structConst :: String -> String -> Structure
structConst name = Structure . el name

getContent :: Structure -> String
getContent (Structure x) = x

list_ :: String -> [Structure] -> Structure
list_ typ = Structure . el typ . concatMap (el "li" . getContent)

empty_ :: Structure
empty_ = Structure ""

concatStruct :: [Structure] -> Structure
concatStruct lst = case lst of
  [] -> empty_
  x : xs -> x <> concatStruct xs

getContentString :: Content -> String
getContentString (Content str) = str

escape :: String -> String
escape = concatMap escapeChar
  where
    escapeChar c = case c of
      '<' -> "&lt;"
      '>' -> "&gt;"
      '&' -> "&amp;"
      '"' -> "&quot;"
      '\'' -> "&#39;"
      _ -> [c]
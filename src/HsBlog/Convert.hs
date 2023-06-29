{- | Markup to HTML conversion module.

This module handles converting documents written in our custom
Markup language into HTML pages.
-}
module HsBlog.Convert where

import HsBlog.Env
import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup

convert :: Env -> Html.Title -> Markup.Document -> Html.Html
convert env title doc = Html.html_ headd body
  where
    headd = Html.title_ (eBlogName env <> " - " <> title)
    article = foldMap convertStructure doc
    websiteTitle = Html.h_ 1 (Html.link_ "index.html" $ Html.txt_ $ eBlogName env)
    body = websiteTitle <> article

convertStructure :: Markup.Structure -> Html.Structure
convertStructure struct = case struct of
  Markup.Heading lvl txt -> Html.h_ lvl $ Html.txt_ txt
  Markup.Paragraph txt -> Html.p_ $ Html.txt_ txt
  Markup.OrderedList lst -> Html.ol_ . map (Html.p_ . Html.txt_) $ lst
  Markup.UnorderedList lst -> Html.ul_ . map (Html.p_ . Html.txt_) $ lst
  Markup.CodeBlock lst -> Html.code_ . unlines $ lst

buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex files = Html.html_ headd (title <> listings <> previews)
  where
    headd = Html.title_ "Blog"
    title = Html.h_ 1 . Html.link_ "index.html" . Html.txt_ $ "Blog"
    listings = Html.h_ 2 . Html.txt_ $ "Posts"
    previews = mconcat . preview $ files

preview :: [(FilePath, [Markup.Structure])] -> [Html.Structure]
preview = map mkLink
  where
    mkLink (file, doc) = case doc of
      Markup.Heading 1 heading : article ->
        (Html.h_ 3 . Html.link_ file . Html.txt_ $ heading)
          <> (foldMap convertStructure . take 3 $ article)
          <> (Html.p_ . Html.link_ file . Html.txt_ $ "...")
      _ -> Html.h_ 3 . Html.link_ file . Html.txt_ $ file
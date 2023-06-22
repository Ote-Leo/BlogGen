module HsBlog.Convert where

import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure

convertStructure :: Markup.Structure -> Html.Structure
convertStructure struct = case struct of
  Markup.Heading lvl txt -> Html.h_ lvl $ Html.txt_ txt
  Markup.Paragraph txt -> Html.p_ $ Html.txt_ txt
  Markup.OrderedList lst -> Html.ol_ . map (Html.p_ . Html.txt_) $ lst
  Markup.UnorderedList lst -> Html.ul_ . map (Html.p_ . Html.txt_) $ lst
  Markup.CodeBlock lst -> Html.code_ . unlines $ lst

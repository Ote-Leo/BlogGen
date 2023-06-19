module HsBlog where

import GHC.IO.Handle
import GHC.IO.Handle.Text
import HsBlog.Convert
import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = hGetContents input >>= hPutStrLn output . process title

convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory = undefined

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse
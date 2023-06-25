module HsBlog (convertDirectory, convertSingle, process) where

import GHC.IO.Handle
import GHC.IO.Handle.Text
import HsBlog.Convert
import HsBlog.Directory (convertDirectory)
import HsBlog.Env
import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup

convertSingle :: String -> Handle -> Handle -> IO ()
convertSingle title input output = hGetContents input >>= hPutStrLn output . process title

process :: String -> String -> String
process title = Html.render . convert defaultEnv title . Markup.parse
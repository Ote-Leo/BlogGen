-- | Process multiple files and convert directories
module HsBlog.Directory
  ( convertDirectory,
    buildIndex,
  )
where

import Control.Exception
import Control.Monad
import Data.List
import Data.Traversable
import HsBlog.Convert (convert, convertStructure)
import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup
import System.Directory
import System.Exit
import System.FilePath
import System.IO

-- | Copy files from one directory to another, converting '.norg' files to
--   '.html' files in the process. Recording unsuccessful reads and writes to stderr.
--
-- May throw an exception on output directory creation.
convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory inputDir outputDir = do
  DirContents filesToProcess filesToCopy <- getDirFilesAndContent inputDir
  createOutputDirectoryOrExit outputDir
  let outputHtmls = orgsToRenderedHtml filesToProcess
  copyFiles outputDir filesToCopy
  writeFiles outputDir outputHtmls
  putStrLn "Done."

------------------------------------

-- * Read directory content

-- | Returns the directory content
getDirFilesAndContent :: FilePath -> IO DirContents
getDirFilesAndContent inputDir = do
  files <- map (inputDir </>) <$> listDirectory inputDir
  let (orgFiles, otherFiles) =
        partition ((== ".norg") . takeExtension) files
  orgFilesAndContent <-
    applyIoOnList readFile orgFiles >>= filterAndReportFailures
  pure $
    DirContents
      { dcFilesToProcess = orgFilesAndContent,
        dcFilesToCopy = otherFiles
      }

-- | The relevant directory content for our application
data DirContents = DirContents
  { -- | File paths and their content
    dcFilesToProcess :: [(FilePath, String)],
    -- | Other file paths, to be copied directly
    dcFilesToCopy :: [FilePath]
  }

------------------------------------

-- * Build index page

buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex files =
  let previews =
        map
          ( \(file, doc) ->
              case doc of
                Markup.Heading 1 heading : article ->
                  Html.h_ 3 (Html.link_ file (Html.txt_ heading))
                    <> foldMap convertStructure (take 2 article)
                    <> Html.p_ (Html.link_ file (Html.txt_ "..."))
                _ ->
                  Html.h_ 3 (Html.link_ file (Html.txt_ file))
          )
          files
   in Html.html_
        "Blog"
        ( Html.h_ 1 (Html.link_ "index.html" (Html.txt_ "Blog"))
            <> Html.h_ 2 (Html.txt_ "Posts")
            <> mconcat previews
        )

------------------------------------

-- * Conversion

-- | Convert text files to Markup, build an index, and render as html.
orgsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]
orgsToRenderedHtml orgFiles =
  let orgOutputFiles = map toOutputMarkupFile orgFiles
      index = ("index.html", buildIndex orgOutputFiles)
   in map (fmap Html.render) (index : map convertFile orgOutputFiles)

toOutputMarkupFile :: (FilePath, String) -> (FilePath, Markup.Document)
toOutputMarkupFile (file, content) =
  (takeBaseName file <.> "html", Markup.parse content)

convertFile :: (FilePath, Markup.Document) -> (FilePath, Html.Html)
convertFile (file, doc) = (file, convert file doc)

------------------------------------

-- * Output to directory

-- | Creates an output directory or terminates the program
createOutputDirectoryOrExit :: FilePath -> IO ()
createOutputDirectoryOrExit outputDir =
  whenIO
    (not <$> createOutputDirectory outputDir)
    (hPutStrLn stderr "Cancelled." *> exitFailure)

-- | Creates the output directory.
--   Returns whether the directory was created or not.
createOutputDirectory :: FilePath -> IO Bool
createOutputDirectory dir = do
  dirExists <- doesDirectoryExist dir
  create <-
    if dirExists
      then do
        override <- confirm "Output directory exists. Override?"
        when override (removeDirectoryRecursive dir)
        pure override
      else pure True
  when create (createDirectory dir)
  pure create

-- | Copy files to a directory, recording errors to stderr.
copyFiles :: FilePath -> [FilePath] -> IO ()
copyFiles outputDir files = do
  let copyFromTo file = copyFile file (outputDir </> takeFileName file)
  void $ applyIoOnList copyFromTo files >>= filterAndReportFailures

-- | Write files to a directory, recording errors to stderr.
writeFiles :: FilePath -> [(FilePath, String)] -> IO ()
writeFiles outputDir files = do
  let writeFileContent (file, content) =
        writeFile (outputDir </> file) content
  void $ applyIoOnList writeFileContent files >>= filterAndReportFailures

------------------------------------

-- * IO work and handling errors

-- | Try to apply an IO function on a list of values, document successes and failures
applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
applyIoOnList action inputs = do
  for inputs $ \input -> do
    maybeResult <-
      catch
        (Right <$> action input)
        ( \(SomeException e) -> do
            pure $ Left (displayException e)
        )
    pure (input, maybeResult)

-- | Filter out unsuccessful operations on files and report errors to stderr.
filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
filterAndReportFailures =
  foldMap $ \(file, contentOrErr) ->
    case contentOrErr of
      Left err -> do
        hPutStrLn stderr err
        pure []
      Right content ->
        pure [(file, content)]

------------------------------------

-- * Utilities

confirm :: String -> IO Bool
confirm question = do
  putStrLn (question <> " (y/n)")
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _ -> do
      putStrLn "Invalid response. Use y or n."
      confirm question

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = do
  result <- cond
  if result
    then action
    else pure ()

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import RIO
import Lens.Micro ((?~), at)
import RIO.Text
import Data.Aeson
import Data.Aeson.KeyMap (union)
import Lens.Micro.Aeson (_Object, key, _Array, _String)
import Slick
import Development.Shake
import Development.Shake.FilePath ( (-<.>), (</>), (<.>), dropDirectory1, takeFileName )
-- import Development.Shake.Forward (cacheAction, shakeArgsForward)
import Development.Shake.Classes (Binary)
import Data.UUID (UUID, toString, toText)
import Data.UUID.V4 (nextRandom)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import Prelude(putStrLn)
import RIO.Time (UTCTime, parseTimeOrError, defaultTimeLocale, iso8601DateFormat, formatTime, getCurrentTime)
import qualified RIO.Set as Set
import qualified RIO.List as List


-- -- --Config-----------------------------------------------------------------------

type App = ReaderT FileMeta Action

siteMeta :: SiteMeta
siteMeta
  = SiteMeta
  { siteAuthor = "Denis Angelov"
  , siteTitle = "DA Blog"
  , siteDomain = "https://denisangelov.xyz/"
  } 

outputFolder :: FilePath
outputFolder = "docs"

-- -- --Data models-------------------------------------------------------------------

data SiteMeta
  = SiteMeta
  { siteAuthor :: Text
  , siteTitle :: Text
  , siteDomain :: Text
  } deriving (Generic, Show, ToJSON)

data FileMeta
  = FileMeta
  { version :: UUID
  } deriving (Generic, Show, ToJSON)

withSiteMeta :: FileMeta -> Value -> Value
withSiteMeta fileMeta (Object obj) = Object $ RIO.foldr union obj [siteMetaObj, fileMetaObj]
  where
    Object fileMetaObj = toJSON fileMeta
    Object siteMetaObj = toJSON siteMeta
withSiteMeta _ _ = error "only add site meta to objects"

-- -- | Data for the all posts page
data  BlogInfo
  = BlogInfo
  { posts :: [Post]
  , tags :: [Tag]
  } deriving (Generic, Show, ToJSON)

type Tag = Text

-- -- | Data for a blog post
data Post
  = Post
  { title :: Text
  , tags :: [Tag]
  , tagsAttribute :: Text
  , tagsDisplay :: Text
  , date :: Text
  , url :: Text
  } deriving (Generic, Show, ToJSON, FromJSON, Binary )

data AtomData
  = AtomData
  { title :: Text
  , author :: Text
  , posts :: [Post]
  , currentTime :: Text
  } deriving (Generic, Show, ToJSON )

-- -- | given a list of posts this will build a table of contents

templateFolderPath = "site/templates/"

indexHTML = "index.html"

blogHTML :: FilePath
blogHTML = "blog.html"

postHTML :: FilePath
postHTML = "post.html"

cvHTML :: FilePath
cvHTML = "cv.html"

atomXML :: FilePath
atomXML = "atom.xml"


indexTemplatePath = templateFolderPath <> indexHTML
cvTemplatePath = templateFolderPath <> cvHTML
blogTemplatePath = templateFolderPath <> blogHTML
postTemplatePath = templateFolderPath <> postHTML
atomTemplatePath = templateFolderPath <> atomXML


buildCV :: App ()
buildCV = do
  fileMeta <- ask
  cvTemplate <- lift $ compileTemplate' cvTemplatePath
  let cvPage
        = toJSON siteMeta
        & withSiteMeta fileMeta
        & substitute cvTemplate
        & unpack
  writeFile' (outputFolder </> cvHTML) cvPage

buildIndex :: App ()
buildIndex = do
  fileMeta <- ask
  indexTemplate <- lift $ compileTemplate' indexTemplatePath
  let indexPage
        = toJSON siteMeta
        & withSiteMeta  fileMeta
        & substitute indexTemplate
        & unpack

  writeFile' (outputFolder </> indexHTML ) indexPage



buildAllPosts :: [Post] -> App ()
buildAllPosts posts' = do
  fileMeta <- ask

  liftIO . putStrLn $ "messages: " <> show posts'
  allPostsTemplate <- lift $ compileTemplate' blogTemplatePath


  let
    tags' = posts' & RIO.map (\Post{..} -> tags) & RIO.foldr (<>) [] & Set.fromList & Set.toList    
    blogPage
        = BlogInfo { posts = posts', tags = tags' } 
        & toJSON
        & withSiteMeta fileMeta
        & substitute allPostsTemplate
        & unpack

  writeFile' (outputFolder </> blogHTML ) blogPage


-- -- | Find and build all posts
buildPosts :: App [Post]
buildPosts = do
  fileMeta <- ask
  pPaths <- lift $ getDirectoryFiles "." ["site/posts//*.md"]
  lift $ forP pPaths $ buildPost fileMeta

-- -- | Load a post, process metadata, write it to output, then return the post object
-- -- Detects changes to either post content or template

buildPost :: FileMeta -> FilePath -> Action Post
buildPost fileMeta srcPath = do
  -- Declare dependencies
  need [srcPath, postTemplatePath]

  putNormal $ "Rebuilding post: " <> srcPath

  postContent <- readFile' srcPath
  postData <- markdownToHTML . pack $ postContent

  let
    postUrl =
      srcPath
        & (-<.> "html")
        & dropDirectory1
        & pack

    tagsArray =
      postData ^.. key "tags" . _Array . traverse . _String

    fullPostData =
      postData
        & _Object . at "url" ?~ String postUrl
        & _Object . at "tagsAttribute"
            ?~ String (intercalate "," tagsArray)
        & _Object . at "tagsDisplay"
            ?~ String (intercalate " | " tagsArray)
        & withSiteMeta fileMeta

  postTemplate <- compileTemplate' postTemplatePath

  substitute postTemplate fullPostData
    & unpack
    & writeFile' (outputFolder </> unpack postUrl)

  convert fullPostData


copyStaticFiles :: App ()
copyStaticFiles = do
  fileMeta <- ask
  
  let 
    tmpCssDir = "build/tmp/css"
    tmpJsDir = "build/tmp/js"
    minifiedCssPath = tmpCssDir </> "index.min.css"
    minifiedJsPath = tmpJsDir </> "index.min.js"

  liftIO $ createDirectoryIfMissing True tmpCssDir
  liftIO $ createDirectoryIfMissing True tmpJsDir


  lift $ cmd_ ("/usr/bin/esbuild" :: String) ("site/css/index.css" :: String) ("--minify" :: String) ("--outfile=" <> minifiedCssPath)
  lift $ cmd_ ("/usr/bin/esbuild" :: String) ("site/js/index.js" :: String)  ("--minify" :: String) ("--outfile=" <> minifiedJsPath)

  let finalCssPath = outputFolder </>  "css" </> "index" <.> toString (version fileMeta) <.> "css"
  lift $ copyFileChanged minifiedCssPath finalCssPath

  let finalJsPath = outputFolder </>  "js" </>  "index" <.> toString (version fileMeta) <.> "js"
  lift $ copyFileChanged minifiedJsPath finalJsPath

  liftIO $ removeDirectoryRecursive "build/tmp"


  filepaths <- lift $ getDirectoryFiles "site" ["images//*", "css//*.css", "js//*.js"]
  let filteredFilepaths = List.filter (\fp -> takeFileName fp /= "index.css" && takeFileName fp /= "index.js") filepaths

  lift $ void $ forP filteredFilepaths $ \filepath ->
    copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

  let pathOtherFiles = "site" </> "other"
  otherFiles <- lift $ getDirectoryContents pathOtherFiles

  lift $ void $ forP otherFiles $ \filepath ->
    copyFileChanged (pathOtherFiles </> filepath) (outputFolder </> filepath)

formatDate :: Text -> Text
formatDate humanDate = toIsoDate parsedTime
  where
    parsedTime =
      parseTimeOrError True defaultTimeLocale  "%b %e, %Y" (unpack humanDate) :: UTCTime

toIsoDate :: UTCTime -> Text
toIsoDate = pack . formatTime defaultTimeLocale (iso8601DateFormat rfc3339)
  where
    rfc3339 = Just "%H:%M:%SZ"


buildFeed :: [Post] -> App ()
buildFeed posts' = do
  now <- liftIO getCurrentTime
  let 
    atomData
        = AtomData
        { title = siteTitle siteMeta 
        , author = siteAuthor siteMeta
        , posts = mkAtomPost <$> posts'
        , currentTime = toIsoDate now
        }

  atomTemplate <- lift $ compileTemplate' atomTemplatePath
  writeFile' (outputFolder </>  atomXML ) . unpack $ substitute atomTemplate (toJSON atomData)
    where
      mkAtomPost :: Post -> Post
      mkAtomPost p = p 
        { date = formatDate $ date p
        , url = siteDomain siteMeta <> url p 
        }


sortPostsByDateDesc =
  let
    parsePostDate :: Post -> UTCTime
    parsePostDate Post{..} =
      parseTimeOrError True defaultTimeLocale "%b %e, %Y" (unpack date)
  in
    List.sortOn (Down . parsePostDate)

-- -- | Specific build rules for the Shake system
-- --   defines workflow to build the website
buildRules :: App ()
buildRules = do
  buildIndex
  buildCV
  allPosts <- sortPostsByDateDesc <$> buildPosts
  buildAllPosts allPosts
  buildFeed allPosts
  copyStaticFiles


-- main :: IO ()
-- main = do
--   guid <- liftIO nextRandom

--   let sh0pts = shakeOptions {  shakeVerbosity = Verbose, shakeLintInside = ["\\"] }
--   shakeArgsForward sh0pts $ runReaderT buildRules FileMeta { version = guid }

main :: IO ()
main = do
    -- Generate a new UUID for this build
    guid <- nextRandom
    let fileMeta = FileMeta { version = guid }

    -- Shake options
    let 
      sh0pts = shakeOptions { shakeVerbosity = Verbose, shakeLintInside = ["\\"] }
      rules = action $ runReaderT buildRules fileMeta
    shakeArgs sh0pts rules
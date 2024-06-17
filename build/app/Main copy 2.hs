{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Main where

import RIO
import Data.UUID (UUID, toString, toText)
import Data.UUID.V4 (nextRandom)
import Lens.Micro ((?~), at)
import RIO.Text
import Data.Aeson
import Data.Aeson.KeyMap (union)
import Lens.Micro.Aeson (_Object)
import Slick
import Development.Shake
import Development.Shake.FilePath ((-<.>), (</>), dropDirectory1, takeBaseName, takeExtension, takeFileName, (<.>), takeDirectory)
import Development.Shake.Forward (cacheAction, shakeArgsForward)
import Development.Shake.Classes (Binary)
import Prelude (putStrLn)
import RIO.Time (UTCTime, parseTimeOrError, defaultTimeLocale, iso8601DateFormat, formatTime, getCurrentTime)

-- --Config-----------------------------------------------------------------------

siteMeta :: SiteMeta
siteMeta = SiteMeta
  { siteAuthor = "Denis"
  , title = "Test Site"
  }

outputFolder :: FilePath
outputFolder = "docs"

-- -- | RIO Monad  -------------------------------------------------------------------

class HasSiteVersion env where
  versionL :: Lens' env Text

instance HasSiteVersion SiteContext where
  versionL = lens version (\x y -> x { version = y })

data SiteContext = SiteContext
  { version :: Text }

withSiteContext :: Value -> SiteContext -> Value
withSiteContext (Object obj) siteContext =
  let Object context = toJSON siteContext
  in Object $ union obj context

-- -- | Data models-------------------------------------------------------------------

data SiteMeta = SiteMeta
  { siteAuthor :: Text
  , title :: Text
  } deriving (Generic, Show, ToJSON)

withSiteMeta :: Value -> Value
withSiteMeta (Object obj) = Object $ union obj siteMetaObj
  where
    Object siteMetaObj = toJSON siteMeta
withSiteMeta _ = error "only add site meta to objects"

-- -- | Data for the all posts page
data BlogInfo = BlogInfo
  { posts :: [Post]
  } deriving (Generic, Show, ToJSON)

type Tag = Text

-- -- | Data for a blog post

data Post = Post
  { title :: Text
  , tags :: [Tag]
  , date :: Text
  , url :: Text
  } deriving (Generic, Show, ToJSON, FromJSON, Binary)

data AtomData = AtomData
  { title :: Text
  , author :: Text
  , posts :: [Post]
  } deriving (Generic, Show, ToJSON)

-- -- | given a list of posts this will build a table of contents

templateFolderPath = "site/templates/"

indexHTML = "index.html"
blogHTML = "blog.html"
postHTML = "post.html"
cvHTML = "cv.html"
atomXML = "atom.xml"

indexTemplatePath = templateFolderPath <> indexHTML
cvTemplatePath = templateFolderPath <> cvHTML
blogTemplatePath = templateFolderPath <> blogHTML
postTemplatePath = templateFolderPath <> postHTML
atomTemplatePath = templateFolderPath <> atomXML

buildCV :: (HasSiteVersion env, HasLogFunc env) => RIO env (Action ())
buildCV = do
  context <- ask
  cvTemplate <- liftIO $ compileTemplate' cvTemplatePath
  let cvPage = toJSON siteMeta
                & withSiteContext (view versionL context)
                & substitute cvTemplate
                & unpack
  liftIO $ writeFile' (outputFolder </> cvHTML) cvPage

buildIndex :: (HasSiteVersion env, HasLogFunc env) => RIO env (Action ())
buildIndex = do
  context <- ask
  indexTemplate <- liftIO $ compileTemplate' indexTemplatePath
  let indexPage = toJSON siteMeta
                 & withSiteContext (view versionL context)
                 & substitute indexTemplate
                 & unpack
  liftIO $ writeFile' (outputFolder </> indexHTML) indexPage

buildAllPosts :: (HasSiteVersion env, HasLogFunc env) => [Post] -> RIO env (Action ())
buildAllPosts posts' = do
  context <- ask
  logInfo $ displayShow posts'
  allPostsTemplate <- liftIO $ compileTemplate' blogTemplatePath
  let blogPage = BlogInfo { posts = posts' }
                & toJSON
                & withSiteContext (view versionL context)
                & withSiteMeta
                & substitute allPostsTemplate
                & unpack
  liftIO $ writeFile' (outputFolder </> blogHTML) blogPage

buildPosts :: (HasSiteVersion env, HasLogFunc env) => RIO env [Post]
buildPosts = do
  context <- ask
  pPaths <- liftIO $ getDirectoryFiles "." ["site/posts//*.md"]
  forM pPaths (buildPost context)

buildPost :: (HasSiteVersion env, HasLogFunc env) => SiteContext -> FilePath -> RIO env Post
buildPost context srcPath = do
  cacheAction ("build" :: Text, srcPath -<.> "html") $ do
    logInfo $ "Rebuilding post: " <> fromString srcPath
    postContent <- liftIO $ readFile' srcPath
    postData <- liftIO . markdownToHTML . pack $ postContent
    let postUrl = (srcPath -<.> "html") & dropDirectory1 & pack
        fullPostData = postData
                      & _Object . at "url" ?~ String postUrl
                      & withSiteMeta
                      & withSiteContext (view versionL context)
    postTemplate <- liftIO $ compileTemplate' postTemplatePath
    liftIO $ writeFile' (outputFolder </> unpack postUrl) (substitute postTemplate fullPostData & unpack)
    return $ convert fullPostData

copyStaticFiles :: (HasSiteVersion env, HasLogFunc env) => RIO env (Action ())
copyStaticFiles = do
  context <- ask
  filepaths <- liftIO $ getDirectoryFiles "site" ["images//*", "css//*", "js//*"]
  forM_ filepaths $ \filepath -> do
    let newFilePath = if takeFileName filepath == "index.css"
                      then [takeBaseName filepath, toString (view versionL context), takeExtension filepath]
                           & RIO.foldr (<.>) ("" :: FilePath)
                           & \path -> takeDirectory filepath </> path
                      else filepath
    liftIO $ copyFileChanged ("site" </> filepath) (outputFolder </> newFilePath)
  let pathOtherFiles = "site" </> "other"
  otherFiles <- liftIO $ getDirectoryContents pathOtherFiles
  forM_ otherFiles $ \filepath ->
    liftIO $ copyFileChanged (pathOtherFiles </> filepath) (outputFolder </> filepath)

formatDate :: Text -> Text
formatDate humanDate = toIsoDate parsedTime
  where
    parsedTime = parseTimeOrError True defaultTimeLocale "%b %e, %Y" (unpack humanDate) :: UTCTime

toIsoDate :: UTCTime -> Text
toIsoDate = pack . formatTime defaultTimeLocale (iso8601DateFormat rfc3339)
  where
    rfc3339 = Just "%H:%M:%SZ"

buildFeed :: (HasSiteVersion env, HasLogFunc env) => [Post] -> RIO env ()
buildFeed posts' = do
  now <- liftIO getCurrentTime
  let atomData = AtomData
                { title = "Placeholder Title"
                , author = siteAuthor siteMeta
                , posts = mkAtomPost <$> posts'
                }
  atomTemplate <- liftIO $ compileTemplate' atomTemplatePath
  liftIO $ writeFile' (outputFolder </> atomXML) . unpack $ substitute atomTemplate (toJSON atomData)
  where
    mkAtomPost :: Post -> Post
    mkAtomPost p = p { date = formatDate $ date p }

buildRules :: (HasSiteVersion env, HasLogFunc env) => RIO env ()
buildRules = do
  buildIndex
  buildCV
  allPosts <- buildPosts
  buildAllPosts allPosts
  buildFeed allPosts
  copyStaticFiles

main :: IO ()
main = do
  guid <- nextRandom
  let context = SiteContext { version = toText guid }
  let shOpts = shakeOptions { shakeVerbosity = Verbose, shakeLintInside = ["\\"] }
  runRIO context $ shakeArgsForward shOpts buildRules
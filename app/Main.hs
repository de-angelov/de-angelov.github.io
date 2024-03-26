module Main where

import RIO
import Lens.Micro ((?~), at)
import RIO.Text
import Data.Aeson
import Data.Aeson.KeyMap (union)
import Lens.Micro.Aeson (_Object)
import Slick
import Development.Shake
import Development.Shake.FilePath ( (-<.>), (</>), dropDirectory1 )
import Development.Shake.Forward (cacheAction, shakeArgsForward)
import Development.Shake.Classes (Binary)



import Prelude(putStrLn)
import RIO.Time (UTCTime, parseTimeOrError, defaultTimeLocale, iso8601DateFormat, formatTime, getCurrentTime)
-- --Config-----------------------------------------------------------------------

siteMeta :: SiteMeta
siteMeta
  = SiteMeta
  { siteAuthor = "Denis"
  , title = "Test Site"
  }


outputFolder :: FilePath
outputFolder = "docs/"

-- -- --Data models-------------------------------------------------------------------

data SiteMeta
  = SiteMeta
  { siteAuthor :: Text
  , title :: Text
  } deriving (Generic, Show, ToJSON)


withSiteMeta :: Value -> Value
withSiteMeta (Object obj) = Object $ union obj siteMetaObj
  where
    Object siteMetaObj = toJSON siteMeta
withSiteMeta _ = error "only add site meta to objects"

-- -- | Data for the index page

data IndexInfo
  = IndexInfo
  { posts :: [Post]
  } deriving (Generic, Show, ToJSON)

type Tag = Text

-- -- | Data for a blog post

data Post
  = Post
  { title :: Text
  , tags :: [Tag]
  , date :: Text
  } deriving (Generic, Show, ToJSON, FromJSON, Binary )

data AtomData
  = AtomData
  { title :: Text
  , author :: Text
  , posts :: [Post]
  } deriving (Generic, Show, ToJSON )

-- -- | given a list of posts this will build a table of contents

templateFolderPath = "site/templates/" 

indexHTML = "index.html"

postHTML = "post.html"

atomXML = "atom.xml"


indexTemplatePath = templateFolderPath <> indexHTML
postTemplatePath = templateFolderPath <> postHTML
atomTemplatePath = templateFolderPath <> atomXML

buildIndex :: [Post] -> Action ()
buildIndex posts' = do
  liftIO . putStrLn $ "test" <> show posts'
  indexTemplate <- compileTemplate' indexTemplatePath
  let indexPage
        = IndexInfo { posts = posts' }
        & toJSON
        & withSiteMeta
        & substitute indexTemplate
        & unpack

  writeFile' (outputFolder </> indexHTML ) indexPage

-- -- | Find and build all posts
buildPosts :: Action [Post]
buildPosts = do
  pPaths <- getDirectoryFiles "." ["site/posts//*.md"]
  forP pPaths buildPost

-- -- | Load a post, process metadata, write it to output, then return the post object
-- -- Detects changes to either post content or template


buildPost :: FilePath -> Action Post
buildPost srcPath = do 
    cacheAction ("build" :: Text, srcPath  -<.> "html") $ do
            ("Rebuilding post: " <> srcPath)
              & putStrLn
              & liftIO

            postContent <- readFile' srcPath
            -- load post content and metadata as JSON blob

            postData <- markdownToHTML . pack $ postContent

            let
              postUrl
                = (srcPath -<.> "html")
                & dropDirectory1
                & pack

              fullPostData
                = postData
                & _Object . at "url" ?~ String postUrl
                & withSiteMeta

            postTemplate <- compileTemplate' postTemplatePath

            substitute postTemplate fullPostData
              & unpack 
              & writeFile' (outputFolder </> unpack postUrl)

            convert fullPostData

-- -- -- | Copy all static files from the listed folders to their destination
copyStaticFiles :: Action ()
copyStaticFiles = do 
  filepaths <- getDirectoryFiles "site" ["images//*", "css//*", "js//*"]
  void $ forP filepaths $ \filepath -> 
    copyFileChanged ("site" </> filepath) (outputFolder </> filepath)


formatDate :: Text -> Text
formatDate humanDate = toIsoDate parsedTime 
  where 
    parsedTime = 
      parseTimeOrError True defaultTimeLocale  "%b %e, %Y" (unpack humanDate) :: UTCTime 



toIsoDate :: UTCTime -> Text 
toIsoDate = pack . formatTime defaultTimeLocale (iso8601DateFormat rfc3339)
  where 
    rfc3339 = Just "%H:%M:%SZ"
  

buildFeed :: [Post] -> Action ()
buildFeed posts' = do 
  -- todo
  now <- liftIO getCurrentTime 
  let atomData 
        = AtomData 
        { title = "Placeholder Title"
        , author = siteAuthor siteMeta
        , posts = mkAtomPost <$> posts' 
        }

  atomTemplate <- compileTemplate' atomTemplatePath
  writeFile' (outputFolder </>  atomXML ) . unpack $ substitute atomTemplate (toJSON atomData)
    where 
      mkAtomPost :: Post -> Post
      mkAtomPost p = p { date = formatDate $ date p }

-- -- | Specific build rules for the Shake system
-- --   defines workflow to build the website
buildRules :: Action ()
buildRules = do 
  allPosts <- buildPosts 
  buildIndex allPosts 
  buildFeed allPosts 
  copyStaticFiles 


main :: IO ()
main = do
  let sh0pts = shakeOptions {  shakeVerbosity = Verbose, shakeLintInside = ["\\"] }
  shakeArgsForward sh0pts buildRules 

-- main :: IO ()
-- main = runSimpleApp $ do
--   logInfo "Hello, RIO! 1"
--   let sh0pts = shakeOptions {  shakeVerbosity = Verbose, shakeLintInside = ["\\"] }
--   liftIO $ shakeArgsForward sh0pts buildRules 
--   logInfo "Hello, RIO! 2"

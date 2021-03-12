--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import           Hakyll
import qualified GHC.IO.Encoding               as E
import           Data.Char                      ( isSpace )
import           Data.Function                  ( on )
import           Data.List                      ( dropWhileEnd )
import           Data.List.Split                ( splitOn )
import qualified Data.Text                     as T
import           Data.Maybe                     ( fromMaybe )
import           Data.String                    ( fromString )
import           System.Environment             ( lookupEnv )
import           System.Process                 ( readProcessWithExitCode )
import           System.Exit                    ( ExitCode(..) )
import           System.FilePath.Posix          ( (</>)
                                                , (<.>)
                                                , splitExtension
                                                , splitFileName
                                                , takeDirectory
                                                )

--------------------------------------------------------------------------------

(//) :: Int -> Int -> Float
(//) = (/) `on` fromIntegral

ertField :: String -> Snapshot -> Context String
ertField name snapshot = field name $ \item -> do
  body <- itemBody <$> loadSnapshot (itemIdentifier item) snapshot
  let words = length (T.words . T.pack $ body)
  return $ show $ round $ words // 250

--------------------------------------------------------------------------------

dateFolders :: Routes
dateFolders =
  gsubRoute "/[0-9]{4}-[0-9]{2}-[0-9]{2}-" $ replaceAll "-" (const "/")

appendIndex :: Routes
appendIndex =
  customRoute $ (\(p, e) -> p </> "index" <.> e) . splitExtension . toFilePath

dropPostsPrefix :: Routes
dropPostsPrefix = gsubRoute "posts/" $ const ""

prependCategory :: Routes
prependCategory = metadataRoute $ \md ->
  customRoute
    $ let mbCategory = lookupString "category" md
          category =
            fromMaybe (error "Posts: Post without category") mbCategory
      in  (category </>) . toFilePath

dropIndexHtml :: String -> Context a
dropIndexHtml key = mapContext transform (urlField key) where
  transform url = case splitFileName url of
    (p, "index.html") -> takeDirectory p
    _                 -> url

--------------------------------------------------------------------------------

data GitLog = GitLog { commitHash :: String, commitMsg :: String, commitDate :: String }
  deriving (Show)

getGitLog :: Integer -> IO [GitLog]
getGitLog limit = do
  (status, stdout, _) <- readProcessWithExitCode
    "git"
    [ "log"
    , "--format=" ++ logfmt
    , "--max-count=" ++ show limit
    , "--"
    , "."
    ]
    ""
  return $ case status of
    ExitSuccess -> map parseGitLog $ splitOn "\n" (trim stdout)
    _           -> []
  where trim = dropWhileEnd isSpace
        logfmt = "%h;%s;%ai"

parseGitLog :: String -> GitLog
parseGitLog log = GitLog {..}
  where [commitHash, commitMsg, commitDate] = splitOn ";" log

logListField
  :: String -> Integer -> Context String
logListField pluralName limit =
  listField pluralName ctx $ unsafeCompiler $ do
    logs <- getGitLog limit
    return $ map logItem logs
 where
  ctx = field "commit" (return . show . commitHash . itemBody)
    <> field "message" (return . show . commitMsg . itemBody)
    <> field "date" (return . show . commitDate . itemBody)

  logItem :: GitLog -> Item GitLog
  logItem log = Item (fromString $ "log/" ++ commitHash log) log


--------------------------------------------------------------------------------

type FeedRenderer
  =  FeedConfiguration
  -> Context String
  -> [Item String]
  -> Compiler (Item String)

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle       = "kamelåså"
  , feedDescription = "Special topics in calamity something or other"
  , feedAuthorName  = "Lee Meichin"
  , feedAuthorEmail = "www@mrlee.dev"
  , feedRoot        = "https://www.kamelasa.dev"
  }

feedCtx :: Context String
feedCtx = postCtx <> bodyField "description"

feedCompiler renderer =
  renderer feedConfiguration feedCtx
    =<< fmap (take 10)
    .   recentFirst
    =<< loadAllSnapshots "posts/*" "content"

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
  field "size" (return . show . length . itemBody)
    <> ertField "ert" "prerendered-content"
    <> dateField "date" "%b %d, %Y"
    <> dropIndexHtml "url"
    <> defaultContext

--------------------------------------------------------------------------------

config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "www"
  , inMemoryCache = True
  }

--------------------------------------------------------------------------------

main :: IO ()
main = do
  E.setLocaleEncoding E.utf8

  compilerEnv <- lookupEnv "HAKYLL_ENV"
  let isDevelopment = compilerEnv == Just "development"

  hakyllWith config $ do

    match "images/*" $ do
      route idRoute
      compile copyFileCompiler

    match "fonts/*" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    matchMetadata
        "posts/**"
        (\m -> isDevelopment || lookupString "status" m == Just "published")
      $ do
          route
            $               setExtension "html"
            `composeRoutes` dateFolders
            `composeRoutes` dropPostsPrefix
            `composeRoutes` prependCategory
            `composeRoutes` appendIndex
          compile
            $   pandocCompiler
            >>= saveSnapshot "prerendered-content"
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["atom.xml"] $ do
      route idRoute
      compile $ feedCompiler renderAtom

    create ["rss.xml"] $ do
      route idRoute
      compile $ feedCompiler renderRss

    create ["archive/index.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/**"
        let archiveCtx =
              listField "posts" postCtx (return posts)
                <> constField "title" "Posts"
                <> dropIndexHtml "url"
                <> defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeUrls


    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"

        let indexCtx =
              listField "posts" postCtx (return posts)
                <> logListField "logs" 10
                <> defaultContext

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

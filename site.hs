{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative             (liftA)
import           Data.Char                       (toLower)
import           Data.List                       (intercalate, intersperse, isSuffixOf)
import           Data.List.Split                 (splitOn)
import qualified Data.Map                        as M
import           Data.Monoid                     (mconcat)
import           Data.Ord                        (comparing)
import           Hakyll
import           System.FilePath                 (takeBaseName)
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

mainConfiguration :: Configuration
mainConfiguration = defaultConfiguration
    { deployCommand = "rsync -avze 'ssh' _site/* cardamom:www/blog"
    }


feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Alieniloquent: off on a tangent"
    , feedDescription = "Samuel Tesla's thoughts on things"
    , feedAuthorName  = "Samuel Tesla"
    , feedAuthorEmail = "blog@alieniloquent.com"
    , feedRoot        = "http://blog.alieniloquent.com"
    }

main :: IO ()
main = hakyllWith mainConfiguration $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "templates/*" $ compile templateCompiler

    years <- buildYears "posts/*" (fromCapture "archive/*")

    match "posts/*" $ do
        route $ gsubRoute "posts/" (const "") `composeRoutes`
                directorizeDate               `composeRoutes`
                setExtension "html"
        compile $ pandocCompiler
              >>= saveSnapshot "feedContent"
              >>= loadAndApplyTemplate "templates/post.html" postContext
              >>= saveSnapshot "content"
              >>= loadAndApplyTemplate "templates/post-single.html" postContext
              >>= loadAndApplyTemplate "templates/default.html" (siteContext years)
              >>= normalizeUrls

    create ["index.html"] $ do
        route idRoute
        compile $ makeItem ""
              >>= loadAndApplyTemplate "templates/index.html" indexContext
              >>= loadAndApplyTemplate "templates/default.html" (siteContext years)
              >>= normalizeUrls

    create ["rss.xml"] $ do
        route idRoute
        compile $ loadAllSnapshots "posts/*" "feedContent"
              >>= recentFirst
              >>= return . take 10
              >>= renderRss feedConfiguration feedContext

    tagsRules years $ \year pattern -> do
        route $ gsubRoute "archive/" (const "") `composeRoutes`
                customRoute (\i -> (toFilePath i) ++ "/index.html")
        compile $ do
            posts <- itemBodies =<< recentFirst =<< loadAllSnapshots pattern "content"
            makeItem ""
              >>= loadAndApplyTemplate "templates/index.html" (mconcat
                    [ constField "posts" posts
                    , siteContext years
                    ])
              >>= loadAndApplyTemplate "templates/default.html" (siteContext years)
              >>= normalizeUrls

feedContext :: Context String
feedContext = mconcat
    [ bodyField "description"
    , urlField "url"
    , dateField "date" "%B %e, %Y"
    , defaultContext
    ]

indexContext :: Context String
indexContext = mconcat
    [ field "posts" (\_ -> postList $ fmap (take 5) . recentFirst)
    , defaultContext
    ]

postContext :: Context String
postContext = mconcat
    [ dateField "date" "%B %e, %Y"
    , defaultContext
    ]

siteContext :: Tags -> Context String
siteContext years = mconcat
    [ field "archives" (\_ -> renderYears $ sortTagsBy descendingTags years)
    , defaultContext
    ]

directorizeDate :: Routes
directorizeDate = customRoute (\i -> directorize $ toFilePath i)
  where
    directorize path = concat components
      where
        components = (intersperse "/" date) ++ ["/"] ++ (intersperse "-" rest)
        (date, rest) = splitAt 3 $ splitOn "-" path

postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter =
    loadAllSnapshots "posts/*" "content"
      >>= sortFilter
      >>= itemBodies

itemBodies :: [Item String] -> Compiler String
itemBodies items = return $ concat $ map itemBody items

buildYears :: MonadMetadata m => Pattern -> (String -> Identifier) -> m Tags
buildYears = buildTagsWith getYear

getYear :: MonadMetadata m => Identifier -> m [String]
getYear = return . return . takeYear . takeBaseName . toFilePath
  where
    takeYear path = concat year
      where (year, _) = splitAt 1 $ splitOn "-" path

renderYears :: Tags -> Compiler String
renderYears = renderTags makeLink (intercalate "<br/>\n")
  where
    makeLink tag url _ _ _ = renderHtml $
      H.a ! A.href (toValue url) $ toHtml tag

descendingTags :: (String, [Identifier]) -> (String, [Identifier]) -> Ordering
descendingTags x y = comparing fst y x

normalizeUrls :: Item String -> Compiler (Item String)
normalizeUrls item = return item
                 >>= deIndexUrls
                 >>= relativizeUrls

deIndexUrls :: Item String -> Compiler (Item String)
deIndexUrls item = do
    route <- getRoute $ itemIdentifier item
    return $ case route of
        Nothing -> item
        Just r  -> fmap (withUrls stripIndex) item

stripIndex :: String -> String
stripIndex url = if "index.html" `isSuffixOf` url && elem (head url) "/."
    then take (length url - 10) url else url

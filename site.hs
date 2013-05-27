--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative (liftA)
import           Data.List (intersperse)
import           Data.List.Split (splitOn)
import           Data.Monoid (mappend)
import           Hakyll
--import           System.FilePath

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*" $ do
        route $ gsubRoute "posts/" (const "") `composeRoutes`
                directorizeDate `composeRoutes`
                setExtension "html"
        compile $ do
            compiled <- pandocCompiler
            content <- loadAndApplyTemplate "templates/post.html" postCtx compiled
            saveSnapshot "content" content
            loadAndApplyTemplate "templates/default.html" postCtx content
              >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
indexCtx :: Context String
indexCtx =
    field "posts" (\_ -> postList $ fmap (take 5) . recentFirst) `mappend`
    siteCtx

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    siteCtx

--------------------------------------------------------------------------------
siteCtx :: Context String
siteCtx =
    constField "archives" "TODO" `mappend`
    defaultContext

--------------------------------------------------------------------------------
directorizeDate :: Routes
directorizeDate = customRoute (\i -> directorize $ toFilePath i)
  where
    directorize path = concat components
      where
        components = (intersperse "/" date) ++ ["/"] ++ (intersperse "-" rest)
        (date, rest) = splitAt 3 $ splitOn "-" path

--------------------------------------------------------------------------------
postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter =
    loadAllSnapshots "posts/*" "content"
      >>= sortFilter
      >>= (\x -> return $ concat $ map itemBody x)

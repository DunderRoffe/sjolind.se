{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (pack)
import Data.Text.Lazy (unpack)
import qualified Web.Scotty as S
import Data.Monoid (mconcat)
import Data.Acid
import Blog
import Page
import Control.Monad.IO.Class (liftIO)

import Network.Wai.Middleware.Static
import Network.HTTP.Types

import Text.Blaze
import Text.Blaze.Html.Renderer.Text (renderHtml)

main :: IO()
main = do
  blog <- openLocalStateFrom "db/" (newBlog (pack "My blog"))
  trueSecret <- liftIO $ readFile "secretFile"
  S.scotty 3000 $ do
    S.middleware $ staticPolicy (noDots >-> addBase "static")

    S.get "" $ do
          blog' <- liftIO$ query blog GetBlog
          let blogHTML = formatBlogAsHTML blog'
          S.html $ renderHtml $ htmlPage blogHTML

    S.post "" $ flip S.rescue (\_ -> S.status unauthorized401) $ do
          (Just requestSecret) <- S.header "secret"
          if unpack requestSecret == head (lines trueSecret)
            then do
              bp <- S.jsonData
              liftIO $ update blog (AddBlogPost bp)
            else
              S.raise "unathorized"

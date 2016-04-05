{-# LANGUAGE OverloadedStrings #-}
module Page where

import Prelude hiding (head, div)
import Data.Monoid (mconcat)

import Text.Blaze
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)

htmlPage :: Html -> Html
htmlPage blog =
          docTypeHtml $ do
            head $ do
              title "sjolind.se"
              link ! rel "stylesheet" ! type_ "text/css" ! href "css/blog.css"
            body $ do
              div ! class_ "about" $ "My name is Viktor Sjölind"
              ul $ do
                li $ a ! href "https://github.com/dunderroffe" ! rel "me" $ "GitHub"
                li $ a ! href "https://bitbucket.org/roffe" ! rel "me" $ "BitBucket"
              blog

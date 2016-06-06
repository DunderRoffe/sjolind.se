{-# LANGUAGE OverloadedStrings  #-}
module Project where

import AbsDatabase
import Data.Monoid (mconcat)

import Prelude hiding (div)
import Text.Blaze
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title, form, label)


renderProject :: Project -> Html
renderProject (Project name posts files subprojects) =
  div ! class_ "project" $ do
    h1 $ text $ mconcat ["Project ", name]
    h2 $ text "Posts"
    ul $ do
      mapM_ renderPostThumb posts

renderPostThumb :: Post -> Html
renderPostThumb (Post h d _ _) =
  li $ text $ mconcat [h, " ", d]

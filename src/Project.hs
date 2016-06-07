{-# LANGUAGE OverloadedStrings  #-}
module Project where

import Post
import AbsDatabase

import Data.Monoid (mconcat)
import Data.Text.Lazy (fromStrict)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Data.Default (def)

import Text.Markdown (markdown)
import Text.Blaze
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title, form, label)
import Prelude hiding (div)

renderProject :: Maybe Post -> Project -> Html
renderProject mpost (Project name firstPage posts files subprojects) =
  div ! class_ "project" $ do
    div ! class_ "project-top" $
      h1 $ text $ name
    div ! class_ "project-content" $
      case mpost of
        Nothing   -> markdown def (fromStrict firstPage)
        Just post -> renderPost post
    div ! class_ "project-posts" $ do
      h2 $ text "posts"
      ul $ mapM_ (renderPostThumb name) (Map.elems posts)

renderPostThumb :: Text -> Post -> Html
renderPostThumb projectName (Post h d _ _) =
  li $ a ! href (textValue (mconcat ["/", projectName, "/", h])) $ text $ mconcat [h, " ", d]

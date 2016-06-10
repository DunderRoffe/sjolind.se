{-# LANGUAGE OverloadedStrings  #-}
module Post where

import AbsDatabase

import Prelude hiding (div, span)

import Data.Default (def)
import Data.Text.Lazy (fromStrict)

import Text.Markdown (markdown)
import Text.Blaze
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (span)

import Control.Monad

renderPost :: Post -> Html
renderPost (Post author heading date content comments) =
  div ! class_ "h-entry post" $ do
    h2 $ do
      renderAuthor author
      abbr ! class_ "p-name" $ text heading
      time ! class_ "dt-published" ! datetime (textValue date)$
        text date
    p ! class_ "e-content"$ markdown def (fromStrict content)
    when (not (null comments)) $ do
      div ! class_ "comments" $ mapM_ renderComment comments

renderComment :: Comment -> Html
renderComment (Comment author date comment subcomments) =
  div ! class_ "h-entry comment" $ do
    h3 $ do
      renderAuthor author
      time ! class_ "dt-published" ! datetime (textValue date)$
        text date
    p ! class_ "e-content" $ text comment
    when (not (null subcomments)) $ do
      div ! class_ "comments" $ mapM_ renderComment subcomments

renderAuthor :: Author -> Html
renderAuthor author = do
  h2 ! class_ "h-card" $ do
    img ! class_ "u-photo" ! href (textValue (authorImage author)) ! alt "<author image>"
    span  ! class_ "p-name u-uri"  ! href (textValue (authorUri author)) $ text (authorName author)


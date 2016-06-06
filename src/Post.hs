{-# LANGUAGE OverloadedStrings  #-}
module Post where

import AbsDatabase

import Prelude hiding (div)
import Text.Blaze
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title, form, label)

import Control.Monad

renderPost :: Post -> Html
renderPost (Post heading date content comments) =
  div ! class_ "post" $ do
    h2 $ do
      text heading
      text date
    p $ text content
    when (not (null comments)) $ do
      div ! class_ "comments" $ mapM_ renderComment comments

renderComment :: Comment -> Html
renderComment (Comment author date comment subcomments) =
  div ! class_ "comment" $ do
    h3 $ do
      text author
      text date
    p $ text comment
    when (not (null subcomments)) $ do
      div ! class_ "comments" $ mapM_ renderComment subcomments

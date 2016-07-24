{-# LANGUAGE OverloadedStrings  #-}
module Post where

import AbsDatabase
import Constants

import Prelude hiding (div, span, id)

import Data.Default (def)
import Data.Text.Lazy (fromStrict, toStrict)

import Text.Markdown (markdown)
import Text.Blaze
import Text.Blaze.Html5 hiding (id)
import Text.Blaze.Html5.Attributes hiding (span, form, label)

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
    unless (null comments) $
      div ! class_ "comments" $ mapM_ renderComment comments

renderComment :: Comment -> Html
renderComment (Comment author date comment subcomments) =
  div ! class_ "h-entry comment" $ do
    h3 $ do
      renderAuthor author
      time ! class_ "dt-published" ! datetime (textValue date)$
        text date
    p ! class_ "e-content" $ text comment
    unless (null subcomments) $
      div ! class_ "comments" $ mapM_ renderComment subcomments

renderAuthor :: Author -> Html
renderAuthor author =
  h2 ! class_ "h-card" $ do
    img ! class_ "u-photo" ! src (textValue (authorImage author)) ! alt "<author image>"
    span  ! class_ "p-name u-uri"  ! href (textValue (authorUri author)) $ text (authorName author)

newPostForm :: Post -> Html
newPostForm post =
  form ! action (textValue (toStrict serverUri)) ! method "post" $ do
    let author = postAuthor post
    div $ do
      label ! for "author-name" $ text "Author Name"
      input ! id  "author-name" ! name "author-name" ! value (textValue (authorName author))

    div $ do
      label ! for "author-image" $ text "Author Image"
      input ! id  "author-image" ! name "author-image" ! value (textValue (authorImage author))

    div $ do
      label ! for "author-uri" $ text "Author uri"
      input ! id  "author-uri" ! name "author-uri" ! value (textValue (authorImage author))

    div $ do
      label ! for "heading" $ text "Heading"
      input ! id  "heading " ! name "heading" ! value (textValue (postHeading post))
    div $ do
      label ! for "date" $ text "Date"
      input ! id  "date" ! name "date" ! value (textValue (postDate post)) ! type_ "date"
    div $ do
      pre $ code ! class_ "markdown" $
        textarea ! id "content" ! name "content" $ text $ postContent post

    div $ do
      input ! type_ "submit"

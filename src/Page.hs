{-# LANGUAGE OverloadedStrings #-}
module Page where

import Prelude hiding (head, div, id)
import Data.Monoid (mconcat)
import Control.Monad (when)

import Data.Text.Lazy (Text, toStrict, pack)

import Constants

import Text.Blaze
import Text.Blaze.Internal as B
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title, form, label)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

renderCore :: Html -> Text
renderCore content = pack $ renderHtml $
    docTypeHtml $ do
      head $ do
        title $ text $ toStrict serverUri
        mapM_ (\url -> link ! href (textValue url) ! rel "me") melinks
        link ! rel "stylesheet" ! type_ "text/css" ! href "/css/default.css"
        link ! rel "stylesheet" ! type_ "text/css" ! href "/js/styles/default.css"
        script ! src "/js/highlight.pack.js" $ return ()
        script $ text "hljs.initHighlightingOnLoad();"
      body $ div ! class_ "page-content" $ content

renderAdminBar :: Bool -> Html
renderAdminBar authenticated =
    if authenticated
       then adminBar
       else signInForm

adminBar :: Html
adminBar = do
  form ! action (textValue (toStrict serverUri)) ! method "post" $
    input ! type_ "submit" ! value "New"
  form ! action (textValue (toStrict serverUri)) ! method "post" $
    input ! type_ "submit" ! value "Edit"

newBlogPost :: Html
newBlogPost =
          form ! action (B.lazyTextValue serverUri) ! method "post" $ do
            div $ do
              label ! for "heading" $ text "Heading"
              input ! id "heading " ! name "heading" ! value ""
                    ! placeholder "Awesome thing!"
            div $ do
              label ! for "date" $ text "Date"
              input ! id "date" ! name "date" ! value "" ! type_ "date"
                    ! placeholder "Date"
            div $ do
              label ! for "content" $ text "Content"
              textarea ! id "content" ! name "content"
                    ! placeholder "BlogPost text..." $ return ()

            div $
              input ! type_ "submit"

signInForm :: Html
signInForm =
          form ! action "https://indieauth.com/auth" ! method "get" $ do
            label ! for "indie_auth_url" $ text "Web Address:"
            input ! id "indie_auth_url" ! type_ "text"
                  ! name "me" ! placeholder "yourdomain.com"
            button ! type_ "submit" $ text "Sign In"
            input ! type_ "hidden" ! name "client_id"
                  ! value (B.lazyTextValue serverUri)
            input ! type_ "hidden" ! name "redirect_uri"
                  ! value (B.lazyTextValue (mconcat [serverUri, "/auth"]))

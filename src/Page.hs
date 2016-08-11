{-# LANGUAGE OverloadedStrings #-}
module Page where

import Prelude hiding (head, div, id)
import Data.Monoid (mconcat)
import Control.Monad (when)

import Data.Text.Lazy (Text, toStrict, pack)
import qualified Data.Text as Strict

import Text.Blaze
import Text.Blaze.Internal as B
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title, form, label)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

renderCore :: [Strict.Text] -> Strict.Text -> Html -> Text
renderCore verificationUris serverUri content = pack $ renderHtml $
    docTypeHtml $ do
      head $ do
        title $ text serverUri
        mapM_  linkVerificationUri verificationUris
        link ! rel "stylesheet" ! type_ "text/css" ! href "/css/default.css"
        link ! rel "stylesheet" ! type_ "text/css" ! href "/js/styles/default.css"
        script ! src "/js/highlight.pack.js" $ return ()
        script $ text "hljs.initHighlightingOnLoad();"
      body $ div ! class_ "page-content" $ content

linkVerificationUri :: Strict.Text -> Html
linkVerificationUri uri =
     link ! rel "me" ! href (textValue uri)

renderAdminBar :: Bool -> Strict.Text -> Html
renderAdminBar authenticated serverUri =
    if authenticated
       then adminBar serverUri
       else signInForm serverUri

adminBar :: Strict.Text -> Html
adminBar serverUri = do
  form ! action (textValue serverUri) ! method "post" $
    input ! type_ "submit" ! value "New"
  form ! action (textValue serverUri) ! method "post" $
    input ! type_ "submit" ! value "Edit"

newBlogPost :: Strict.Text -> Html
newBlogPost serverUri =
          form ! action (textValue serverUri) ! method "post" $ do
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

signInForm :: Strict.Text -> Html
signInForm serverUri =
          form ! action "https://indieauth.com/auth" ! method "get" $ do
            label ! for "indie_auth_url" $ text "Web Address:"
            input ! id "indie_auth_url" ! type_ "text"
                  ! name "me" ! placeholder "yourdomain.com"
            button ! type_ "submit" $ text "Sign In"
            input ! type_ "hidden" ! name "client_id"
                  ! value (textValue serverUri)
            input ! type_ "hidden" ! name "redirect_uri"
                  ! value (textValue (mconcat [serverUri, "/auth"]))

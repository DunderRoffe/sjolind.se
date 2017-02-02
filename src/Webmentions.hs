{-# LANGUAGE OverloadedStrings #-}
module Webmentions where

import AbsDatabase
import AcidDatabase
import Endpoints.Common

import Data.Acid (AcidState)
import Data.Text
import Data.ByteString.Lazy (ByteString)
import Data.Maybe

-- Microformat2 parser
import Data.Microformats2.Parser
import Data.Default
import qualified Data.Aeson as Aeson

import Control.Monad (unless)
import Control.Applicative (empty)
import Control.Concurrent (forkIO)

import Network.HTTP.Client hiding (parseUrl)
import Network.HTTP.Client.TLS

type Webmention = (Text, Text)

data Mention = Like Author | Comment [Author] Text | Reference Text

instance FromJSON Author where
    parseJSON (Object o) = do
         properties <- o .: "properties"
         (name:_)  <- properties .:  "name"
         (photo:_) <- properties .:? "name" != ["No_photo"]
         (url:_)   <- properties .:  "name"

         return $ Author name photo url

    parseJSON _ = empty


processWebmentionAsync :: Webmention -> AcidState Database -> IO ()
processWebmentionAsync w db = do
    forkIO (processWebmention w db)
    return ()

processWebmention :: Webmention -> AcidState Database -> IO ()
processWebmention (source, target) db = do
    host <- getServerUri db 
    case stripPrefix host target of
         Nothing  -> return ()
         Just url -> do
             sourceContent <- getSource source
             case parseSource sourceContent target of
                 Nothing -> return ()
                 Just content -> handleMention (parseUrl $ splitOn "/" url) content db

handleMention :: (Text, Text) -> Mention -> AcidState Database -> IO ()
handleMention (_, "") _ _ = return () -- Do not react if someone mentions startpage
handleMention (project, post) mentionContent db =
    -- TODO: Add mention to database
    return ()

parseUrl :: [Text] -> (Text, Text)
parseUrl [] = ("", "")
parseUrl [project]            = (project, "")
parseUrl (project:("post":_)) = (project, "")
parseUrl (project:("file":_)) = (project, "")
parseUrl (project:(post:_))   = (project, post)

parseSource :: ByteString -> Text -> Maybe Mention
parseSource content targetUrl = flip parse root $ do
         rels        <- root .:  "rels"
         in_reply_to <- rels .: "in-reply-to"

         unless (targetUrl `elem` in_reply_to) (return Nothing) $ do
             items    <- root .: "items"
             unless (not null items) (return Nothing) $ do
                 itemName    <- head items .: "name"
                 itemContent <- head items .: "content"
                 unless (not null itemContent) (return Nothing) $ do
                     contentValue <- head itemContent .: "value"
                     authorObj <- head items .: "author"
                     author    <- parseJSON authorObj

                     Just $ Comment author contentValue

    where root = parseMf2 def $ documentRoot $ parseLBS content

getSource :: Text -> IO ByteString
getSource source = do
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest $ unpack source

    let request = initialRequest { method = "GET" }
    response <- httpLbs request manager
    return $ responseBody response

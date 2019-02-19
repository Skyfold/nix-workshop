{-# LANGUAGE OverloadedStrings #-}

module Handler.Download where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Yesod

import Foundation

getDownloadR :: Int -> Handler TypedContent
getDownloadR ident = do
    StoredFile filename contentType bytes <- getById ident
    addHeader "Content-Disposition" $ Text.concat
        [ "attachment; filename=\"", filename, "\""]
    sendResponse (Text.encodeUtf8 contentType, toContent bytes)

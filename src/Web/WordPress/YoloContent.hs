{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Web.WordPress.YoloContent where

import           Data.Aeson         (ToJSON, FromJSON)
import           Network.HTTP.Media ((//))
import           Servant.API        (Accept (..), MimeRender (..), MimeUnrender (..))

data YoloJSON

instance Accept YoloJSON where
  contentType _ = "text" // "html"

instance ToJSON a => MimeRender YoloJSON a where
  mimeRender = mimeRender

instance FromJSON a => MimeUnrender YoloJSON a where
  mimeUnrender = mimeUnrender

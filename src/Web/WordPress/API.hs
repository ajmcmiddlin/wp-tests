{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Web.WordPress.API where

import           Data.Proxy               (Proxy (Proxy))
import           Servant.API              ((:<|>), (:>), Get, JSON, ReqBody)
import           Servant.Client           (client)

import           Web.WordPress.Types.Post (ListPost, Post)

type Posts =
  "posts" :>
  ( ReqBody '[JSON] ListPost :> Get '[JSON] [Post]
  )

postsAPI = Proxy :: Proxy Posts

list = client postsAPI

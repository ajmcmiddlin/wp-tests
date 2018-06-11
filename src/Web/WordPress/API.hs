{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Web.WordPress.API where

import           Data.Proxy               (Proxy (Proxy))
import           Servant.API              ((:<|>), (:>), Get, JSON, ReqBody)
import           Servant.Client           (client, ClientM)

import           Web.WordPress.Types.Post (ListPosts, Post)

type Posts =
  "posts" :>
  ( ReqBody '[JSON] ListPosts :> Get '[JSON] [Post]
  )

postsAPI = Proxy :: Proxy Posts

listPosts :: ListPosts -> ClientM [Post]
listPosts = client postsAPI

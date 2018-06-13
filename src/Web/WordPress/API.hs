{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Web.WordPress.API where

import           Data.Proxy                (Proxy (Proxy))
import           Servant.API               ((:<|>), (:>), Get, JSON, ReqBody)
import           Servant.Client            (ClientM, client)

import           Web.WordPress.Types.Post  (ListPostsMap, PostMap)
import           Web.WordPress.YoloContent (YoloJSON)

type Posts =
  "posts" :>
  ( ReqBody '[JSON] ListPostsMap :> Get '[JSON, YoloJSON] [PostMap]
  )

postsAPI = Proxy :: Proxy Posts

listPosts :: ListPostsMap -> ClientM [PostMap]
listPosts = client postsAPI

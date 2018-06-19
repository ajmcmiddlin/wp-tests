{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Web.WordPress.API where

import           Data.Proxy                (Proxy (Proxy))
import           Servant.API               ((:<|>) ((:<|>)), (:>), Get, JSON, ReqBody, NoContent, Post)
import           Servant.Client            (ClientM, client)

import           Web.WordPress.Types.Post  (ListPostsMap, PostMap)
import           Web.WordPress.YoloContent (YoloJSON)

type Posts =
  "posts" :>
  (    ReqBody '[JSON] ListPostsMap :> Get '[JSON, YoloJSON] [PostMap]
  :<|> ReqBody '[JSON] PostMap :> Post '[JSON] NoContent
  )

postsAPI = Proxy :: Proxy Posts

listPosts :: ListPostsMap -> ClientM [PostMap]
createPost :: PostMap -> ClientM NoContent

(listPosts :<|> createPost) = client postsAPI


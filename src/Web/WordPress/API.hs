{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Web.WordPress.API where

import           Data.Proxy                (Proxy (Proxy))
import           Servant.API               ((:<|>) ((:<|>)), (:>), BasicAuth,
                                            Get, JSON, NoContent, Post, ReqBody, BasicAuthData)
import           Servant.Client            (ClientM, client)

import           Web.WordPress.Types.Post  (ListPostsMap, PostMap, CreatePostMap)
import           Web.WordPress.YoloContent (YoloJSON)

type Posts =
  "posts" :>
  (    ReqBody '[JSON] ListPostsMap :> Get '[JSON, YoloJSON] [PostMap]
  :<|> BasicAuth "wordpress" Int :> ReqBody '[JSON] CreatePostMap :> Post '[JSON] NoContent
  )

postsAPI = Proxy :: Proxy Posts

listPosts :: ListPostsMap -> ClientM [PostMap]
createPost :: BasicAuthData -> CreatePostMap -> ClientM NoContent

(listPosts :<|> createPost) = client postsAPI


{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Web.WordPress.API where

import           Data.Functor.Identity         (Identity)
import           Data.Proxy                    (Proxy (Proxy))
import           Servant.API                   ((:<|>) ((:<|>)), (:>),
                                                BasicAuth, BasicAuthData,
                                                Capture, Get, JSON, Post,
                                                QueryParams, ReqBody, Delete)
import           Servant.Client                (ClientM, client)

import           Servant.QueryParamMap         (QueryParamMap)
import           Web.WordPress.Types.ListPosts (ListPostsKey, ListPostsMap)
import           Web.WordPress.Types.Post      (PostMap, Status)
import           Web.WordPress.YoloContent     (YoloJSON)

type Posts =
  "posts" :>
  (    QueryParamMap ListPostsKey Identity :> Get '[JSON, YoloJSON] [PostMap]
  :<|> BasicAuth "wordpress" () :> ReqBody '[JSON] PostMap :> Post '[JSON] PostMap
  :<|> BasicAuth "wordpress" () :> Capture "id" Int :> Get '[JSON] PostMap
  :<|> BasicAuth "wordpress" () :> Capture "id" Int :> Delete '[JSON] PostMap
  )

postsAPI :: Proxy Posts
postsAPI = Proxy

listPosts :: ListPostsMap -> ClientM [PostMap]
createPost :: BasicAuthData -> PostMap -> ClientM PostMap
getPost :: BasicAuthData -> Int -> ClientM PostMap
deletePost :: BasicAuthData -> Int -> ClientM PostMap

(listPosts :<|> createPost :<|> getPost :<|> deletePost) =
  client postsAPI

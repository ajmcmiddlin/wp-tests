{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Web.WordPress.API where

import           Data.Functor.Identity         (Identity)
import           Data.Proxy                    (Proxy (Proxy))
import           Servant.API                   ((:<|>) ((:<|>)), (:>),
                                                BasicAuth, BasicAuthData,
                                                Capture, Delete, Get, JSON,
                                                Post, QueryParam, ReqBody)
import           Servant.Client                (ClientM, client)

import           Servant.QueryParamMap         (QueryParamMap)
import           Servant.RequiredQueryParam    (QueryParam')
import           Web.WordPress.Types.ListPosts (ListPostsKey, ListPostsMap)
import           Web.WordPress.Types.Post      (DeletedPost, ForceDelete,
                                                NoForceDelete, PostMap)
import           Web.WordPress.YoloContent     (YoloJSON)

type Posts =
  "posts" :> (
  List
  :<|> BasicAuth "wordpress" () :> List

  :<|> BasicAuth "wordpress" () :> Capture "id" Int :> Get '[JSON] PostMap

  :<|> BasicAuth "wordpress" () :> ReqBody '[JSON] PostMap :> Post '[JSON] PostMap

  :<|> BasicAuth "wordpress" () :> Capture "id" Int :>
       QueryParam "force" NoForceDelete :> Delete '[JSON] DeletedPost

  :<|> BasicAuth "wordpress" () :> Capture "id" Int :>
       QueryParam' "force" ForceDelete :> Delete '[JSON] DeletedPost
  )

type List = QueryParamMap ListPostsKey Identity :> Get '[JSON, YoloJSON] [PostMap]

postsAPI :: Proxy Posts
postsAPI = Proxy

listPosts :: ListPostsMap -> ClientM [PostMap]
listPostsAuth :: BasicAuthData -> ListPostsMap -> ClientM [PostMap]
getPost :: BasicAuthData -> Int -> ClientM PostMap
createPost :: BasicAuthData -> PostMap -> ClientM PostMap
deletePost :: BasicAuthData -> Int -> Maybe NoForceDelete -> ClientM DeletedPost
deletePostForce :: BasicAuthData -> Int -> ForceDelete -> ClientM DeletedPost

(     listPosts :<|> listPostsAuth :<|> getPost :<|> createPost
 :<|> deletePost :<|> deletePostForce ) = client postsAPI

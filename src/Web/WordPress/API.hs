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
  -- List
  List
  :<|> Auth :> List

  -- Get
  :<|> Auth :> Id :> Get '[JSON] PostMap

  -- Create
  :<|> Auth :> ReqBody '[JSON] PostMap :> Post '[JSON] PostMap

  -- Update
  :<|> Auth :> Id :> ReqBody '[JSON] PostMap :> Post '[JSON] PostMap

  -- Delete
  :<|> Auth :> Id :> QueryParam "force" NoForceDelete :> Delete '[JSON] DeletedPost
  :<|> Auth :> Id :> QueryParam' "force" ForceDelete :> Delete '[JSON] DeletedPost
  )

type List = QueryParamMap ListPostsKey Identity :> Get '[JSON, YoloJSON] [PostMap]
type Auth = BasicAuth "wordpress" ()
type Id = Capture "id" Int

postsAPI :: Proxy Posts
postsAPI = Proxy

listPosts :: ListPostsMap -> ClientM [PostMap]
listPostsAuth :: BasicAuthData -> ListPostsMap -> ClientM [PostMap]
getPost :: BasicAuthData -> Int -> ClientM PostMap
createPost :: BasicAuthData -> PostMap -> ClientM PostMap
updatePost :: BasicAuthData -> Int -> PostMap -> ClientM PostMap
deletePost :: BasicAuthData -> Int -> Maybe NoForceDelete -> ClientM DeletedPost
deletePostForce :: BasicAuthData -> Int -> ForceDelete -> ClientM DeletedPost

(     listPosts :<|> listPostsAuth
 :<|> getPost
 :<|> createPost
 :<|> updatePost
 :<|> deletePost :<|> deletePostForce ) =
   client postsAPI

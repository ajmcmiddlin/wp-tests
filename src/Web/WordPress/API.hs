{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Web.WordPress.API where

import           Servant.API ((:<|>), (:>), Get, JSON, ReqBody)

import Web.WordPress.Types.Post (ListPost, Post)

type Posts =
  "posts" :>
  ( ReqBody '[JSON] ListPost :> Get '[JSON] [Post]
  )

{-# LANGUAGE GADTs      #-}
{-# LANGUAGE MultiWayIf #-}

module Web.WordPress.Test.Expected where

import qualified Data.Dependent.Map       as DM
import           Data.Functor.Compose     (Compose (Compose, getCompose))
import           Data.Functor.Identity    (Identity (runIdentity))
import           Data.Maybe               (fromMaybe)
import           Data.Time                (LocalTime)
import           Data.Time.Extras         (nominalDiff, nominalSecond)

import           Web.WordPress.Test.Types (StatePost (StatePost, getStatePost))
import           Web.WordPress.Types.Post (PostKey (..),
                                           Status (Future, Publish), postDate)

expected ::
  LocalTime
  -> PostKey a
  -> StatePost
  -> [Identity a]
expected now k s =
  let
    lup = maybe [] pure . DM.lookup k . getStatePost $ s
  in
    case k of
      PostStatus        -> expectedStatus now s
      PostDate          -> lup
      PostDateGmt       -> lup
      PostGuid          -> lup
      PostId            -> lup
      PostLink          -> lup
      PostModified      -> lup
      PostModifiedGmt   -> lup
      PostSlug          -> lup
      PostType          -> lup
      PostPassword      -> lup
      PostTitle         -> lup
      PostContent       -> lup
      PostAuthor        -> lup
      PostExcerpt       -> lup
      PostFeaturedMedia -> lup
      PostCommentStatus -> lup
      PostPingStatus    -> lup
      PostFormat        -> lup
      PostMeta          -> lup
      PostSticky        -> lup
      PostTemplate      -> lup
      PostCategories    -> lup
      PostTags          -> lup

expectedStatus ::
  LocalTime
  -> StatePost
  -> [Identity Status]
expectedStatus now (StatePost m) =
  fromMaybe [] $ do
    s <- runIdentity <$> DM.lookup PostStatus m
    dt <- nominalDiff <$> postDate m <*> pure now
    let
      isCloseToCutover = abs dt < (nominalSecond * 10)
    fmap getCompose . getCompose $
      if | elem s [Future, Publish] && isCloseToCutover ->
             Compose . pure . Compose $ fmap pure [Future, Publish]
         | s == Future && dt < 0 ->
             pure Publish
         | s == Publish && dt > 0 ->
             pure Future
         | otherwise ->
             pure s

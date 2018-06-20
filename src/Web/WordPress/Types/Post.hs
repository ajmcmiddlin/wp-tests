{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Web.WordPress.Types.Post where

import           Control.Applicative   (liftA2, liftA3)
import           Data.Aeson            (FromJSON (..), Object, ToJSON (..),
                                        Value (Bool, Object), object,
                                        withObject, (.:), (.:?), (.=))
import           Data.Aeson.Types      (FromJSON1, Pair, Parser, ToJSON1,
                                        parseJSON1, toJSON1)
import           Data.Dependent.Map    (DMap, DSum (..), GCompare (..), empty,
                                        foldrWithKey, fromList, insert, lookup,
                                        toList)
import           Data.Dependent.Sum    (EqTag (..), ShowTag (..), (==>))
import           Data.Functor.Classes  (Eq1, Show1, eq1, showsPrec1)
import           Data.Functor.Const    (Const (..))
import           Data.Functor.Identity (Identity (Identity))
import           Data.GADT.Compare     ((:~:) (Refl), GCompare (..), GEq (..),
                                        GOrdering (..))
import           Data.GADT.Compare.TH  (deriveGCompare, deriveGEq)
import           Data.GADT.Show.TH     (deriveGShow)
import qualified Data.HashMap.Strict   as HM
import           Data.Maybe            (maybe)
import           Data.Proxy            (Proxy (..))
import           Data.Semigroup        ((<>))
import           Data.Set              (Set)
import           Data.Some             (Some (This))
import           Data.Text             (Text, pack)
import           Data.Time             (LocalTime, UTCTime)
import           GHC.Generics          (Generic)
import           GHC.Prim              (Proxy#, proxy#)
import           GHC.TypeLits          (KnownSymbol, Symbol, symbolVal')

import           Data.GADT.Aeson       (FromJSONViaKey (..), GKey (..),
                                        ToJSONViaKey (..), mkParseJSON, symName)

-- TODO ajmccluskey: maybe we can/should hide all of the JSON names in the types to keep everything
-- together and simplify To/FromJSON instances.
data PostKey a where
  PostDate          :: PostKey LocalTime
  PostDateGmt       :: PostKey UTCTime
  PostGuid          :: PostKey (Rendered "guid")
  PostId            :: PostKey Int
  PostLink          :: PostKey Text
  PostModified      :: PostKey LocalTime
  PostModifiedGmt   :: PostKey UTCTime
  PostSlug          :: PostKey Text
  PostStatus        :: PostKey Status
  PostType          :: PostKey Text
  PostPassword      :: PostKey Text
  PostTitle         :: PostKey (Rendered "title")
  PostContent       :: PostKey (RP "content")
  PostAuthor        :: PostKey Int
  PostExcerpt       :: PostKey (RP "excerpt")
  PostFeaturedMedia :: PostKey Int
  PostCommentStatus :: PostKey CommentStatus
  PostPingStatus    :: PostKey PingStatus
  PostFormat        :: PostKey Format
  PostMeta          :: PostKey [Text]
  PostSticky        :: PostKey Bool
  PostTemplate      :: PostKey Text
  PostCategories    :: PostKey [Int]
  PostTags          :: PostKey [Int]

deriving instance Show (PostKey a)
deriving instance Eq (PostKey a)
deriving instance Ord (PostKey a)

-- TODO: use TH to get rid of this
instance FromJSON1 f => FromJSONViaKey PostKey f where
  parseJSONViaKey PostDate          = parseJSON1
  parseJSONViaKey PostDateGmt       = parseJSON1
  parseJSONViaKey PostGuid          = parseJSON1
  parseJSONViaKey PostId            = parseJSON1
  parseJSONViaKey PostLink          = parseJSON1
  parseJSONViaKey PostModified      = parseJSON1
  parseJSONViaKey PostModifiedGmt   = parseJSON1
  parseJSONViaKey PostSlug          = parseJSON1
  parseJSONViaKey PostStatus        = parseJSON1
  parseJSONViaKey PostType          = parseJSON1
  parseJSONViaKey PostPassword      = parseJSON1
  parseJSONViaKey PostTitle         = parseJSON1
  parseJSONViaKey PostContent       = parseJSON1
  parseJSONViaKey PostAuthor        = parseJSON1
  parseJSONViaKey PostExcerpt       = parseJSON1
  parseJSONViaKey PostFeaturedMedia = parseJSON1
  parseJSONViaKey PostCommentStatus = parseJSON1
  parseJSONViaKey PostPingStatus    = parseJSON1
  parseJSONViaKey PostFormat        = parseJSON1
  parseJSONViaKey PostMeta          = parseJSON1
  parseJSONViaKey PostSticky        = parseJSON1
  parseJSONViaKey PostTemplate      = parseJSON1
  parseJSONViaKey PostCategories    = parseJSON1
  parseJSONViaKey PostTags          = parseJSON1

instance ToJSON1 f => ToJSONViaKey PostKey f where
  toJSONViaKey PostDate          = toJSON1
  toJSONViaKey PostDateGmt       = toJSON1
  toJSONViaKey PostGuid          = toJSON1
  toJSONViaKey PostId            = toJSON1
  toJSONViaKey PostLink          = toJSON1
  toJSONViaKey PostModified      = toJSON1
  toJSONViaKey PostModifiedGmt   = toJSON1
  toJSONViaKey PostSlug          = toJSON1
  toJSONViaKey PostStatus        = toJSON1
  toJSONViaKey PostType          = toJSON1
  toJSONViaKey PostPassword      = toJSON1
  toJSONViaKey PostTitle         = toJSON1
  toJSONViaKey PostContent       = toJSON1
  toJSONViaKey PostAuthor        = toJSON1
  toJSONViaKey PostExcerpt       = toJSON1
  toJSONViaKey PostFeaturedMedia = toJSON1
  toJSONViaKey PostCommentStatus = toJSON1
  toJSONViaKey PostPingStatus    = toJSON1
  toJSONViaKey PostFormat        = toJSON1
  toJSONViaKey PostMeta          = toJSON1
  toJSONViaKey PostSticky        = toJSON1
  toJSONViaKey PostTemplate      = toJSON1
  toJSONViaKey PostCategories    = toJSON1
  toJSONViaKey PostTags          = toJSON1

instance (Applicative f, FromJSON1 f) => FromJSON (DMap PostKey f) where
  parseJSON = mkParseJSON "Post"

-- TODO: use TH to get rid of this
instance Show1 f => ShowTag PostKey f where
  showTaggedPrec PostDate          = showsPrec1
  showTaggedPrec PostDateGmt       = showsPrec1
  showTaggedPrec PostGuid          = showsPrec1
  showTaggedPrec PostId            = showsPrec1
  showTaggedPrec PostLink          = showsPrec1
  showTaggedPrec PostModified      = showsPrec1
  showTaggedPrec PostModifiedGmt   = showsPrec1
  showTaggedPrec PostSlug          = showsPrec1
  showTaggedPrec PostStatus        = showsPrec1
  showTaggedPrec PostType          = showsPrec1
  showTaggedPrec PostPassword      = showsPrec1
  showTaggedPrec PostTitle         = showsPrec1
  showTaggedPrec PostContent       = showsPrec1
  showTaggedPrec PostAuthor        = showsPrec1
  showTaggedPrec PostExcerpt       = showsPrec1
  showTaggedPrec PostFeaturedMedia = showsPrec1
  showTaggedPrec PostCommentStatus = showsPrec1
  showTaggedPrec PostPingStatus    = showsPrec1
  showTaggedPrec PostFormat        = showsPrec1
  showTaggedPrec PostMeta          = showsPrec1
  showTaggedPrec PostSticky        = showsPrec1
  showTaggedPrec PostTemplate      = showsPrec1
  showTaggedPrec PostCategories    = showsPrec1
  showTaggedPrec PostTags          = showsPrec1

-- TODO: use TH and Symbol to get rid of this
instance GKey PostKey where
  toFieldName = \case
    PostDate -> "date"
    PostDateGmt -> "date_gmt"
    PostGuid -> "guid"
    PostId -> "id"
    PostLink -> "link"
    PostModified -> "modified"
    PostModifiedGmt -> "modifiedGmt"
    PostSlug -> "slug"
    PostStatus -> "status"
    PostType -> "type"
    PostPassword -> "password"
    PostTitle -> "title"
    PostContent -> "content"
    PostAuthor -> "author"
    PostExcerpt -> "excerpt"
    PostFeaturedMedia -> "featured_media"
    PostCommentStatus -> "comment_status"
    PostPingStatus -> "ping_status"
    PostFormat -> "format"
    PostMeta -> "meta"
    PostSticky -> "sticky"
    PostTemplate -> "template"
    PostCategories -> "categories"
    PostTags -> "tags"

  fromFieldName = \case
    "date" -> Just (This PostDate)
    "date_gmt" -> Just (This PostDateGmt)
    "guid" -> Just (This PostGuid)
    "id" -> Just (This PostId)
    "link" -> Just (This PostLink)
    "modified" -> Just (This PostModified)
    "modifiedGmt" -> Just (This PostModifiedGmt)
    "slug" -> Just (This PostSlug)
    "status" -> Just (This PostStatus)
    "type" -> Just (This PostType)
    "password" -> Just (This PostPassword)
    "title" -> Just (This PostTitle)
    "content" -> Just (This PostContent)
    "author" -> Just (This PostAuthor)
    "excerpt" -> Just (This PostExcerpt)
    "featured_media" -> Just (This PostFeaturedMedia)
    "comment_status" -> Just (This PostCommentStatus)
    "ping_status" -> Just (This PostPingStatus)
    "format" -> Just (This PostFormat)
    "meta" -> Just (This PostMeta)
    "sticky" -> Just (This PostSticky)
    "template" -> Just (This PostTemplate)
    "categories" -> Just (This PostCategories)
    "tags" -> Just (This PostTags)
    _ -> Nothing

  keys = [
      This PostDate
    , This PostDateGmt
    , This PostGuid
    , This PostId
    , This PostLink
    , This PostModified
    , This PostModifiedGmt
    , This PostSlug
    , This PostStatus
    , This PostType
    , This PostPassword
    , This PostTitle
    , This PostContent
    , This PostAuthor
    , This PostExcerpt
    , This PostFeaturedMedia
    , This PostCommentStatus
    , This PostPingStatus
    , This PostFormat
    , This PostMeta
    , This PostSticky
    , This PostTemplate
    , This PostCategories
    , This PostTags
    ]

type PostMap = DMap PostKey Identity

type ListPostsMap = DMap ListPostsKey Identity

data ListPostsKey a where
  ListPostsContext           :: ListPostsKey Context
  ListPostsPage              :: ListPostsKey Int
  ListPostsPerPage           :: ListPostsKey Int
  ListPostsSearch            :: ListPostsKey Text
  ListPostsAfter             :: ListPostsKey LocalTime
  ListPostsAuthor            :: ListPostsKey Int
  ListPostsAuthorExclude     :: ListPostsKey (Set Int)
  ListPostsBefore            :: ListPostsKey LocalTime
  ListPostsExclude           :: ListPostsKey (Set Int)
  ListPostsInclude           :: ListPostsKey (Set Int)
  ListPostsOffset            :: ListPostsKey Int
  ListPostsOrder             :: ListPostsKey Order
  ListPostsSlug              :: ListPostsKey (Set Text)
  ListPostsStatus            :: ListPostsKey Status
  ListPostsCategories        :: ListPostsKey (Set Text)
  ListPostsCategoriesExclude :: ListPostsKey (Set Text)
  ListPostsTags              :: ListPostsKey (Set Text)
  ListPostsTagsExclude       :: ListPostsKey (Set Text)
  ListPostsSticky            :: ListPostsKey Sticky

deriving instance Eq (ListPostsKey a)

instance GKey ListPostsKey where
  toFieldName = \case
    ListPostsContext -> "context"
    ListPostsPage -> "page"
    ListPostsPerPage -> "per_page"
    ListPostsSearch -> "search"
    ListPostsAfter -> "after"
    ListPostsAuthor -> "author"
    ListPostsAuthorExclude -> "author_exclude"
    ListPostsBefore -> "before"
    ListPostsExclude -> "exclude"
    ListPostsInclude -> "include"
    ListPostsOffset -> "offset"
    ListPostsOrder -> "order"
    ListPostsSlug -> "slug"
    ListPostsStatus -> "status"
    ListPostsCategories -> "categories"
    ListPostsCategoriesExclude -> "categories_exclude"
    ListPostsTags -> "tags"
    ListPostsTagsExclude -> "tags_exclude"
    ListPostsSticky -> "sticky"

  fromFieldName = \case
    "context" -> Just (This ListPostsContext)
    "page" -> Just (This ListPostsPage)
    "per_page" -> Just (This ListPostsPerPage)
    "search" -> Just (This ListPostsSearch)
    "after" -> Just (This ListPostsAfter)
    "author" -> Just (This ListPostsAuthor)
    "author_exclude" -> Just (This ListPostsAuthorExclude)
    "before" -> Just (This ListPostsBefore)
    "exclude" -> Just (This ListPostsExclude)
    "include" -> Just (This ListPostsInclude)
    "offset" -> Just (This ListPostsOffset)
    "order" -> Just (This ListPostsOrder)
    "slug" -> Just (This ListPostsSlug)
    "status" -> Just (This ListPostsStatus)
    "categories" -> Just (This ListPostsCategories)
    "categories_exclude" -> Just (This ListPostsCategoriesExclude)
    "tags" -> Just (This ListPostsTags)
    "tags_exclude" -> Just (This ListPostsTagsExclude)
    "sticky" -> Just (This ListPostsSticky)
    _ -> Nothing

  keys =
    [ This ListPostsContext
    , This ListPostsPage
    , This ListPostsPerPage
    , This ListPostsSearch
    , This ListPostsAfter
    , This ListPostsAuthor
    , This ListPostsAuthorExclude
    , This ListPostsBefore
    , This ListPostsExclude
    , This ListPostsInclude
    , This ListPostsOffset
    , This ListPostsOrder
    , This ListPostsSlug
    , This ListPostsStatus
    , This ListPostsCategories
    , This ListPostsCategoriesExclude
    , This ListPostsTags
    , This ListPostsTagsExclude
    , This ListPostsSticky
    ]

instance Show1 f => ShowTag ListPostsKey f where
  showTaggedPrec ListPostsContext           = showsPrec1
  showTaggedPrec ListPostsPage              = showsPrec1
  showTaggedPrec ListPostsPerPage           = showsPrec1
  showTaggedPrec ListPostsSearch            = showsPrec1
  showTaggedPrec ListPostsAfter             = showsPrec1
  showTaggedPrec ListPostsAuthor            = showsPrec1
  showTaggedPrec ListPostsAuthorExclude     = showsPrec1
  showTaggedPrec ListPostsBefore            = showsPrec1
  showTaggedPrec ListPostsExclude           = showsPrec1
  showTaggedPrec ListPostsInclude           = showsPrec1
  showTaggedPrec ListPostsOffset            = showsPrec1
  showTaggedPrec ListPostsOrder             = showsPrec1
  showTaggedPrec ListPostsSlug              = showsPrec1
  showTaggedPrec ListPostsStatus            = showsPrec1
  showTaggedPrec ListPostsCategories        = showsPrec1
  showTaggedPrec ListPostsCategoriesExclude = showsPrec1
  showTaggedPrec ListPostsTags              = showsPrec1
  showTaggedPrec ListPostsTagsExclude       = showsPrec1
  showTaggedPrec ListPostsSticky            = showsPrec1

instance ToJSON1 f => ToJSONViaKey ListPostsKey f where
  toJSONViaKey ListPostsContext           = toJSON1
  toJSONViaKey ListPostsPage              = toJSON1
  toJSONViaKey ListPostsPerPage           = toJSON1
  toJSONViaKey ListPostsSearch            = toJSON1
  toJSONViaKey ListPostsAfter             = toJSON1
  toJSONViaKey ListPostsAuthor            = toJSON1
  toJSONViaKey ListPostsAuthorExclude     = toJSON1
  toJSONViaKey ListPostsBefore            = toJSON1
  toJSONViaKey ListPostsExclude           = toJSON1
  toJSONViaKey ListPostsInclude           = toJSON1
  toJSONViaKey ListPostsOffset            = toJSON1
  toJSONViaKey ListPostsOrder             = toJSON1
  toJSONViaKey ListPostsSlug              = toJSON1
  toJSONViaKey ListPostsStatus            = toJSON1
  toJSONViaKey ListPostsCategories        = toJSON1
  toJSONViaKey ListPostsCategoriesExclude = toJSON1
  toJSONViaKey ListPostsTags              = toJSON1
  toJSONViaKey ListPostsTagsExclude       = toJSON1
  toJSONViaKey ListPostsSticky            = toJSON1

instance Eq1 f => EqTag ListPostsKey f where
  eqTagged ListPostsContext ListPostsContext = eq1


--------------------------------------------------------------------------------
-- CREATE
--------------------------------------------------------------------------------

-- Create inputs are a strict subset of those in posts, so reuse them.
createKeys
  :: [Some PostKey]
createKeys = [
    This PostDate
  , This PostDateGmt
  , This PostSlug
  , This PostStatus
  , This PostPassword
  , This PostTitle
  , This PostContent
  , This PostAuthor
  , This PostExcerpt
  , This PostFeaturedMedia
  , This PostCommentStatus
  , This PostPingStatus
  , This PostFormat
  , This PostMeta
  , This PostSticky
  , This PostTemplate
  , This PostCategories
  , This PostTags
  ]

data Status =
    Publish
  | Future
  | Draft
  | Pending
  | Private
  deriving (Enum, Bounded, Show)

instance ToJSON Status where
  toJSON = \case
    Publish -> "publish"
    Future -> "future"
    Draft -> "draft"
    Pending -> "pending"
    Private -> "private"

instance FromJSON Status where
  parseJSON v = case v of
    "publish" -> pure Publish
    "future"  -> pure Future
    "draft"   -> pure Draft
    "pending" -> pure Pending
    "private" -> pure Private
    _         -> fail $ "Unknown status " <> show v

data CommentStatus =
    CommentsOpen
  | CommentsClosed
  deriving Show

instance ToJSON CommentStatus where
  toJSON = \case
    CommentsOpen -> "open"
    CommentsClosed -> "closed"

instance FromJSON CommentStatus where
  parseJSON v = case v of
    "open"   -> pure CommentsOpen
    "closed" -> pure CommentsClosed
    _        -> fail $ "Unknown comment status " <> show v

data PingStatus =
    PingsOpen
  | PingsClosed
  deriving Show

instance ToJSON PingStatus where
  toJSON = \case
    PingsOpen -> "open"
    PingsClosed -> "closed"

instance FromJSON PingStatus where
  parseJSON v = case v of
    "open"   -> pure PingsOpen
    "closed" -> pure PingsClosed
    _        -> fail $ "Unknown ping status " <> show v

data Format =
    Standard
  | Aside
  | Chat
  | Gallery
  | Link
  | Image
  | Quote
  | Status
  | Video
  | Audio
  deriving (Eq, Show)

instance FromJSON Format where
  parseJSON v = case v of
    "standard" -> pure Standard
    "aside"    -> pure Aside
    "chat"     -> pure Chat
    "gallery"  -> pure Gallery
    "link"     -> pure Link
    "image"    -> pure Image
    "quote"    -> pure Quote
    "status"   -> pure Status
    "video"    -> pure Video
    "audio"    -> pure Audio
    _          -> fail $ "Unknown format " <> show v

instance ToJSON Format where
  toJSON = \case
    Standard -> "standard"
    Aside -> "aside"
    Chat -> "chat"
    Gallery -> "gallery"
    Link -> "link"
    Image -> "image"
    Quote -> "quote"
    Status -> "status"
    Video -> "video"
    Audio -> "audio"

data Context =
    View
  | Edit
  | Embed
  deriving (Eq, Show)

instance ToJSON Context where
  toJSON = \case
    View -> "view"
    Edit -> "edit"
    Embed -> "embed"

data Order =
    Asc
  | Desc
  deriving (Eq, Show)

instance ToJSON Order where
  toJSON = \case
    Asc -> "asc"
    Desc -> "desc"

data OrderBy =
    Author
  | Date
  | Id
  | Include
  | Modified
  | Parent
  | Relevance
  | Slug
  | Title
  deriving (Eq, Show)

data Sticky =
    Sticky
  | NotSticky
  deriving (Eq, Show)

instance ToJSON Sticky where
  toJSON = \case
    Sticky -> Bool True
    NotSticky -> Bool False

newtype Rendered (name :: Symbol) =
  Rendered
  { rendered :: Text }
  deriving (Show)

instance KnownSymbol name => FromJSON (Rendered name) where
  parseJSON =
      withObject (symName @name) $ \v ->
        Rendered <$> v .: "rendered"

instance ToJSON (Rendered name) where
  toJSON (Rendered name) =
    object [("rendered", toJSON name)]

data RP (name :: Symbol) =
  RP
  { rpRendered  :: Text
  , rpProtected :: Bool
  }
  deriving (Eq, Show)

instance KnownSymbol name => FromJSON (RP name) where
  parseJSON =
    withObject (symName @name) $ \v ->
      RP <$> v .: "rendered" <*> v .: "protected"

instance ToJSON (RP name) where
  toJSON RP{..} =
    object [
      ("rendered", toJSON rpRendered)
    , ("protected", toJSON rpProtected)
    ]

deriveGEq ''PostKey
deriveGCompare ''PostKey
deriveGShow ''PostKey

deriveGEq ''ListPostsKey
deriveGCompare ''ListPostsKey
deriveGShow ''ListPostsKey

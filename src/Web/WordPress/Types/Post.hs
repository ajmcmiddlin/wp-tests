{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
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
import           Data.Aeson.Types      (Pair, Parser)
import           Data.Dependent.Map    (DMap, DSum (..), GCompare (..), empty, toList,
                                        foldrWithKey, fromList, insert, lookup)
import           Data.Dependent.Sum    (ShowTag (..), (==>))
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
import           Prelude               hiding (lookup)

import Data.GADT.Aeson (GKey (..), symName, FromJSONKey (..), ToJSONKey (..))


-- TODO ajmccluskey: maybe we can/should hide all of the JSON names in the types to keep everything
-- together and simplify To/FromJSON instances.
data PostKey a where
  PostDate          :: PostKey LocalTime
  PostDateGmt       :: PostKey LocalTime
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

instance FromJSONKey PostKey f where
  parseJSONKey _ = parseJSON

instance ShowTag PostKey Identity where
  showTaggedPrec PostDate          = showsPrec
  showTaggedPrec PostDateGmt       = showsPrec
  showTaggedPrec PostGuid          = showsPrec
  showTaggedPrec PostId            = showsPrec
  showTaggedPrec PostLink          = showsPrec
  showTaggedPrec PostModified      = showsPrec
  showTaggedPrec PostModifiedGmt   = showsPrec
  showTaggedPrec PostSlug          = showsPrec
  showTaggedPrec PostStatus        = showsPrec
  showTaggedPrec PostType          = showsPrec
  showTaggedPrec PostPassword      = showsPrec
  showTaggedPrec PostTitle         = showsPrec
  showTaggedPrec PostContent       = showsPrec
  showTaggedPrec PostAuthor        = showsPrec
  showTaggedPrec PostExcerpt       = showsPrec
  showTaggedPrec PostFeaturedMedia = showsPrec
  showTaggedPrec PostCommentStatus = showsPrec
  showTaggedPrec PostPingStatus    = showsPrec
  showTaggedPrec PostFormat        = showsPrec
  showTaggedPrec PostMeta          = showsPrec
  showTaggedPrec PostSticky        = showsPrec
  showTaggedPrec PostTemplate      = showsPrec
  showTaggedPrec PostCategories    = showsPrec
  showTaggedPrec PostTags          = showsPrec

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

addToMapIfPresent
  :: (Applicative f, FromJSON v, GCompare k)
  => Object
  -> Text
  -> k v
  -> Parser (DMap k f)
  -> Parser (DMap k f)
addToMapIfPresent o name key pDm =
  maybe id (insert key . pure) <$> (o .:? name) <*> pDm

instance Applicative f => FromJSON (DMap PostKey f) where
  parseJSON = withObject "Post" $ \o ->
    let add = addToMapIfPresent o
    in
      add "date" PostDate
    . addToMapIfPresent o "date_gmt" PostDateGmt
    . addToMapIfPresent o "guid" PostGuid
    . addToMapIfPresent o "id" PostId
    . addToMapIfPresent o "link" PostLink
    . addToMapIfPresent o "modified" PostModified
    . addToMapIfPresent o "modifiedGmt" PostModifiedGmt
    . addToMapIfPresent o "slug" PostSlug
    . addToMapIfPresent o "status" PostStatus
    . addToMapIfPresent o "type" PostType
    . addToMapIfPresent o "password" PostPassword
    . addToMapIfPresent o "title" PostTitle
    . addToMapIfPresent o "content" PostContent
    . addToMapIfPresent o "author" PostAuthor
    . addToMapIfPresent o "excerpt" PostExcerpt
    . addToMapIfPresent o "featured_media" PostFeaturedMedia
    . addToMapIfPresent o "comment_status" PostCommentStatus
    . addToMapIfPresent o "ping_status" PostPingStatus
    . addToMapIfPresent o "format" PostFormat
    . addToMapIfPresent o "meta" PostMeta
    . addToMapIfPresent o "sticky" PostSticky
    . addToMapIfPresent o "template" PostTemplate
    . addToMapIfPresent o "categories" PostCategories
    . addToMapIfPresent o "tags" PostTags
    $ pure empty

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

instance GToFieldName ListPostsKey where
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

addToObjectIfPresent
  :: ( ToJSON v
     , GCompare k
     )
  => DMap k Identity
  -> k v
  -> Text
  -> Object
  -> Object
addToObjectIfPresent dm kv label o =
  case lookup kv dm of
    Just (Identity v) -> HM.singleton label (toJSON v) <> o
    Nothing           -> o

instance GCompare ListPostsKey => ToJSON (DMap ListPostsKey Identity) where
  toJSON dm =
    let
      add :: ToJSON v => ListPostsKey v -> Text -> Object -> Object
      add = addToObjectIfPresent dm
    in
        Object
      . add ListPostsContext "context"
      . add ListPostsPage "page"
      . add ListPostsPerPage "per_page"
      . add ListPostsSearch "search"
      . add ListPostsAfter "after"
      . add ListPostsAuthor "author"
      . add ListPostsAuthorExclude "author_exclude"
      . add ListPostsBefore "before"
      . add ListPostsExclude "exclude"
      . add ListPostsInclude "include"
      . add ListPostsOffset "offset"
      . add ListPostsOrder "order"
      . add ListPostsSlug "slug"
      . add ListPostsStatus "status"
      . add ListPostsCategories "categories"
      . add ListPostsCategoriesExclude "categories_exclude"
      . add ListPostsTags "tags"
      . add ListPostsTagsExclude "tags_exclude"
      . add ListPostsSticky "sticky"
      $ HM.empty

data Status =
    Publish
  | Future
  | Draft
  | Pending
  | Private
  deriving Show

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
  deriving Show

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

data Context =
    View
  | Edit
  | Embed
  deriving Show

instance ToJSON Context where
  toJSON = \case
    View -> "view"
    Edit -> "edit"
    Embed -> "embed"

data Order =
    Asc
  | Desc
  deriving Show

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
  deriving Show

data Sticky =
    Sticky
  | NotSticky
  deriving Show
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

data RP (name :: Symbol) =
  RP
  { rpRendered  :: Text
  , rpProtected :: Bool
  }
  deriving Show

instance KnownSymbol name => FromJSON (RP name) where
  parseJSON =
    withObject (symName @name) $ \v ->
      RP <$> v .: "rendered" <*> v .: "protected"

deriveGEq ''PostKey
deriveGCompare ''PostKey
deriveGShow ''PostKey

deriveGEq ''ListPostsKey
deriveGCompare ''ListPostsKey

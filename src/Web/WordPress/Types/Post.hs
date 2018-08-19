{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wno-unused-matches#-}

module Web.WordPress.Types.Post
  (
  -- * Key type
    PostKey (..)

  -- * Map type synonym
  , PostMap

  -- * Field types
  , Status (..)
  , CommentStatus (..)
  -- Don't export a Slug constructor -- force use of `mkSlug`
  , Slug
  , mkSlug
  , trashSlug
  , Author (..)
  , Context (..)
  , Sticky (..)
  , mkCreatePR
  , mkCreateR

  -- * Deleting posts
  , DeletedPost (..)
  , ForceDelete (..)
  , NoForceDelete (..)
  ) where

import           Control.Applicative   ((<|>))
import           Control.Lens          (Getter, to, (^.))
import           Data.Aeson            (FromJSON (..), ToJSON (..),
                                        Value (Bool, String), object,
                                        withObject, withText, (.:))
import           Data.Aeson.Types      (FromJSON1, Result (..), ToJSON1 (..),
                                        parse)
import           Data.Dependent.Map    (DMap)
import           Data.Functor.Identity (Identity)
import           Data.GADT.Compare.TH  (deriveGCompare, deriveGEq)
import           Data.GADT.Show.TH     (deriveGShow)
import           Data.Semigroup        ((<>))
import           Data.Some             (Some (This))
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Time             (LocalTime)
import           GHC.Generics          (Generic)
import           GHC.TypeLits          (KnownSymbol, Symbol)
import           Web.HttpApiData       (ToHttpApiData (toQueryParam))

import           Data.GADT.Aeson       (JSONKey (..), mkParseJSON, symName,
                                        toJSONDMap)
import           Data.GADT.Aeson.TH    (deriveEqTag, deriveFromJSONViaKey,
                                        deriveShowTag, deriveToJSONViaKey)

-- TODO ajmccluskey: maybe we can/should hide all of the JSON names in the types to keep everything
-- together and simplify To/FromJSON instances.
data PostKey a where
  PostDate          :: PostKey LocalTime
  PostDateGmt       :: PostKey LocalTime
  PostGuid          :: PostKey (R "guid") --(RenderableText "guid" ctx)
  PostId            :: PostKey Int
  PostLink          :: PostKey Text
  PostModified      :: PostKey LocalTime
  PostModifiedGmt   :: PostKey LocalTime
  PostSlug          :: PostKey Slug
  PostStatus        :: PostKey Status
  PostType          :: PostKey Text
  PostPassword      :: PostKey Text
  PostTitle         :: PostKey (R "title")
  PostContent       :: PostKey (RP "content")
  PostAuthor        :: PostKey Author
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

instance ToJSON1 f => ToJSON (DMap PostKey f) where
  toJSON = toJSONDMap

instance (Applicative f, FromJSON1 f) => FromJSON (DMap PostKey f) where
  parseJSON = mkParseJSON "Post"

-- TODO: use TH and Symbol to get rid of this
instance JSONKey PostKey where
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

-- | When a post is force (fully) deleted, the return type changes slightly
data DeletedPost =
  TrashedPost PostMap
  | DeletedPost
    { deleted  :: Bool
    , previous :: PostMap
    }
  deriving (Generic)

deriving instance Show DeletedPost
deriving instance Eq DeletedPost

instance FromJSON DeletedPost where
  parseJSON v =
        TrashedPost <$> parseJSON v
    <|> withObject "DeletedPost" (\o -> DeletedPost <$> o .: "deleted" <*> o .: "previous") v

-- | Type to capture force deleting
data ForceDelete =
  ForceDelete
  deriving (Eq, Show)

instance ToHttpApiData ForceDelete where
  toQueryParam _ = "true"

data NoForceDelete =
  NoForceDelete
  deriving (Eq, Show)

instance ToHttpApiData NoForceDelete where
  toQueryParam _ = "false"

data Status =
    Publish
  | Future
  | Draft
  | Pending
  | Private
  | Trash
  deriving (Eq, Enum, Bounded, Show)

instance ToHttpApiData Status where
  toQueryParam  = \case
    Publish -> "publish"
    Future -> "future"
    Draft -> "draft"
    Pending -> "pending"
    Private -> "private"
    Trash -> "trash"

instance ToJSON Status where
  toJSON = String . toQueryParam

instance FromJSON Status where
  parseJSON v = case v of
    "publish" -> pure Publish
    "future"  -> pure Future
    "draft"   -> pure Draft
    "pending" -> pure Pending
    "private" -> pure Private
    "trash"   -> pure Trash
    _         -> fail $ "Unknown status " <> show v

data CommentStatus =
    CommentsOpen
  | CommentsClosed
  deriving (Eq, Show)

instance ToHttpApiData CommentStatus where
  toQueryParam = \case
    CommentsOpen -> "open"
    CommentsClosed -> "closed"

instance ToJSON CommentStatus where
  toJSON = String . toQueryParam

instance FromJSON CommentStatus where
  parseJSON v = case v of
    "open"   -> pure CommentsOpen
    "closed" -> pure CommentsClosed
    _        -> fail $ "Unknown comment status " <> show v

data PingStatus =
    PingsOpen
  | PingsClosed
  deriving (Eq, Show)

instance ToHttpApiData PingStatus where
  toQueryParam = \case
    PingsOpen -> "open"
    PingsClosed -> "closed"

instance ToJSON PingStatus where
  toJSON = String . toQueryParam

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

instance ToHttpApiData Format where
  toQueryParam = \case
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

instance ToJSON Format where
  toJSON = String . toQueryParam

data Context =
    View
  | Edit
  | Embed
  deriving (Eq, Show)

instance ToHttpApiData Context where
  toQueryParam = \case
    View -> "view"
    Edit -> "edit"
    Embed -> "embed"

instance ToJSON Context where
  toJSON = String . toQueryParam

data Sticky =
    Sticky
  | NotSticky
  deriving (Eq, Show)

instance ToHttpApiData Sticky where
  toQueryParam = \case
    Sticky -> "true"
    NotSticky -> "false"

instance ToJSON Sticky where
  toJSON = \case
    Sticky -> Bool True
    NotSticky -> Bool False

data Renderable =
    RCreate Text
  | RGet { _rendered :: Text}
  | RCreated { _rendered :: Text, raw :: Text}
  deriving (Show)

class HasRendered r where
  rendered :: Getter r Text

-- | Our notion of equality for renderable things is that one of their renderings is a substring of
-- the other. For example, when we create a post, WordPress might add HTML tags around it when
-- rendering.
renderedEq
  :: HasRendered r
  => r
  -> r
  -> Bool
renderedEq r r' =
     T.isInfixOf (r ^. rendered) (r' ^. rendered)
  || T.isInfixOf (r' ^. rendered) (r ^. rendered)

instance HasRendered Renderable where
  rendered = to $ \case
    RCreate t -> t
    RGet t -> t
    RCreated t _ -> t

instance Eq Renderable where
  (==) = renderedEq

instance ToJSON Renderable where
  toJSON = \case
    RCreate t -> toJSON t
    RGet{..} -> object [("rendered", toJSON _rendered)]
    RCreated{..} -> object [ ("rendered", toJSON _rendered)
                           , ("raw", toJSON raw)
                           ]

-- TODO: reintroduce type level context to avoid trying different parsers?
instance FromJSON Renderable where
  -- WARNING order is important here. We need to try parsing both the rendered and raw keys
  -- before trying only the "rendered" key.
  parseJSON v =
        withText "RCreate" (pure . RCreate) v
    <|> withObject "RCreated" (\o -> RCreated <$> o .: "rendered" <*> o .: "raw") v
    <|> withObject "RGet" (fmap RGet . (.: "rendered")) v

data ProtectedRenderable =
    PRCreate Text
  | PRGet { _rendered :: Text, _protected :: Bool }
  | PRCreated { _rendered :: Text, _raw :: Text, _protected :: Bool }
  deriving (Show)

instance HasRendered ProtectedRenderable where
  rendered = to $ \case
    PRCreate t -> t
    PRGet t _ -> t
    PRCreated t _ _ -> t

instance Eq ProtectedRenderable where
  (==) = renderedEq

instance ToJSON ProtectedRenderable where
  toJSON = \case
    PRCreate t -> toJSON t
    PRGet{..} -> object [ ("rendered", toJSON _rendered)
                        , ("protected", toJSON _protected)
                        ]
    PRCreated{..} -> object [ ("rendered", toJSON _rendered)
                            , ("protected", toJSON _protected)
                            , ("raw", toJSON _raw)
                            ]

instance FromJSON ProtectedRenderable where
  -- WARNING order is important here. We need to try parsing both the rendered and raw keys
  -- before trying only the "rendered" key.
  parseJSON v =
        withText "PRCreate" (pure . PRCreate) v
    <|> withObject "PRCreated" (\o -> PRCreated <$> o .: "rendered" <*> o .: "raw" <*> o .: "protected") v
    <|> withObject "PRGet" (\o -> PRGet <$> (o .: "rendered") <*> (o .: "protected")) v

data R (l :: Symbol) where
  R :: L l Renderable -> R l

deriving instance Show (R l)
deriving instance Eq (R l)

instance ToJSON (R l) where
  toJSON (R l) = toJSON l
instance KnownSymbol l => FromJSON (R l) where
  parseJSON = fmap R . parseJSON

data RP (l :: Symbol) where
  RP :: L l ProtectedRenderable -> RP l

deriving instance Show (RP l)
deriving instance Eq (RP l)

instance ToJSON (RP l) where
  toJSON (RP l) = toJSON l
instance KnownSymbol l => FromJSON (RP l) where
  parseJSON = fmap RP . parseJSON

data RESTContext =
    Create
  | Created
  | Get
  deriving (Show, Eq)

data L (l :: Symbol) a where
  L :: a -> L l a
  deriving (Eq, Show)

instance (KnownSymbol l, FromJSON a) => FromJSON (L l a) where
  parseJSON v =
    case parse parseJSON v of
      Error s   -> fail $ "While parsing '" <> symName @l <> "': " <> s
      Success a -> pure (L a)

instance ToJSON a => ToJSON (L l a) where
  toJSON (L a) = toJSON a

newtype Author =
  Author Int
  deriving (Eq, Show, ToJSON, FromJSON, ToHttpApiData)

newtype Slug =
  Slug Text
  deriving (Eq, Show, ToJSON, FromJSON, ToHttpApiData)

maxSlugLength :: Int
maxSlugLength = 200

mkSlug ::
  Text
  -> Slug
mkSlug t =
  Slug . T.toLower $ T.take maxSlugLength t

trashSlug ::
  Slug
  -> Slug
trashSlug (Slug t) =
  let suffix = "__trashed"
   in Slug . (<> suffix) $ T.take (maxSlugLength - T.length suffix) t

mkCreateR
  :: Text
  -> R l
mkCreateR t =
  R (L (RCreate t))

mkCreatePR
  :: Text
  -> RP l
mkCreatePR t =
  RP (L (PRCreate t))

-- mkGetRenderable
--   :: Text
--   -> Bool
--   -> Renderable l
-- mkGetRenderable t b =
--   L (GetCtx (RenderedGet (RGet t b)))

-- mkCreatedRenderable
--   :: Text
--   -> Bool
--   -> Text
--   -> Renderable l
-- mkCreatedRenderable t b raw =
--   L (CreatedCtx (RenderedCreated (RCreated t b raw)))

deriveGEq ''PostKey
deriveGCompare ''PostKey
deriveGShow ''PostKey

deriveFromJSONViaKey ''PostKey
deriveToJSONViaKey ''PostKey
deriveShowTag ''PostKey
deriveEqTag ''PostKey

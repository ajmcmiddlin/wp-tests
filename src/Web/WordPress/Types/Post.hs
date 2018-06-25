{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
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

module Web.WordPress.Types.Post where

import           Control.Applicative   ((<|>))
import           Control.Lens          (Getter, to, (^.))
import           Data.Aeson            (FromJSON (..), ToJSON (..),
                                        Value (Bool), object,
                                        withObject, withText, (.:))
import           Data.Aeson.Types      (FromJSON1, Result (..), ToJSON1 (..),
                                        parse, parseJSON1, toJSON1)
import           Data.Dependent.Map    (DMap)
import           Data.Dependent.Sum    (ShowTag (..))
import           Data.Functor.Classes  (Eq1, Show1, eq1, showsPrec1)
import           Data.Functor.Identity (Identity)
import           Data.GADT.Compare.TH  (deriveGCompare, deriveGEq)
import           Data.GADT.Show.TH     (deriveGShow)
import           Data.Semigroup        ((<>))
import           Data.Some             (Some (This))
import           Data.Text             (Text)
import           Data.Time             (LocalTime)
import           GHC.TypeLits          (KnownSymbol, Symbol)

import           Data.GADT.Aeson       (FromJSONViaKey (..), GKey (..),
                                        ToJSONViaKey (..), mkParseJSON, symName, EqViaKey (..))

-- TODO ajmccluskey: maybe we can/should hide all of the JSON names in the types to keep everything
-- together and simplify To/FromJSON instances.
data PostKey a where
  PostDate          :: PostKey LocalTime
  PostDateGmt       :: PostKey LocalTime
  PostGuid          :: PostKey (R "guid")--(RenderableText "guid" ctx)
  PostId            :: PostKey Int
  PostLink          :: PostKey Text
  PostModified      :: PostKey LocalTime
  PostModifiedGmt   :: PostKey LocalTime
  PostSlug          :: PostKey Text
  PostStatus        :: PostKey Status
  PostType          :: PostKey Text
  PostPassword      :: PostKey Text
  PostTitle         :: PostKey (R "title")
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

instance Eq1 f => EqViaKey PostKey f where
  eqViaKey PostDate          = eq1
  eqViaKey PostDateGmt       = eq1
  eqViaKey PostGuid          = eq1
  eqViaKey PostId            = eq1
  eqViaKey PostLink          = eq1
  eqViaKey PostModified      = eq1
  eqViaKey PostModifiedGmt   = eq1
  eqViaKey PostSlug          = eq1
  eqViaKey PostStatus        = eq1
  eqViaKey PostType          = eq1
  eqViaKey PostPassword      = eq1
  eqViaKey PostTitle         = eq1
  eqViaKey PostContent       = eq1
  eqViaKey PostAuthor        = eq1
  eqViaKey PostExcerpt       = eq1
  eqViaKey PostFeaturedMedia = eq1
  eqViaKey PostCommentStatus = eq1
  eqViaKey PostPingStatus    = eq1
  eqViaKey PostFormat        = eq1
  eqViaKey PostMeta          = eq1
  eqViaKey PostSticky        = eq1
  eqViaKey PostTemplate      = eq1
  eqViaKey PostCategories    = eq1
  eqViaKey PostTags          = eq1

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
  deriving (Eq, Enum, Bounded, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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

data Sticky =
    Sticky
  | NotSticky
  deriving (Eq, Show)

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

-- | Our notion of equality for renderable things is that their renderings are equal, and nothing
-- else matters. This allows us to make comparisons regardless of the contexts in which a renderable
-- was created.
renderedEq
  :: HasRendered r
  => r
  -> r
  -> Bool
renderedEq r r' =
  r ^. rendered == r' ^. rendered

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


{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}

module Web.WordPress.Types.Post where

import           Control.Applicative   (liftA2, liftA3)
import           Data.Aeson            (FromJSON (..), Object, ToJSON (..),
                                        Value (Bool), object, withObject, (.:),
                                        (.:?), (.=))
import           Data.Aeson.Types      (Parser)
import           Data.Dependent.Map    (DMap, DSum (..), GCompare (..), empty,
                                        fromList, insert)
import           Data.Dependent.Sum    (ShowTag (..), (==>))
import           Data.Functor.Identity (Identity (Identity))
import           Data.GADT.Compare     (GEq)
import           Data.GADT.Compare.TH  (deriveGCompare, deriveGEq)
import           Data.GADT.Show.TH     (deriveGShow)
import           Data.Maybe            (maybe)
import           Data.Semigroup        ((<>))
import           Data.Set              (Set)
import           Data.Text             (Text)
import           Data.Time             (LocalTime, UTCTime)
import           GHC.Generics          (Generic)

defaultListPosts :: ListPosts
defaultListPosts =
  ListPosts Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
            Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data PostKey a where
  PostDate :: PostKey LocalTime
  PostDateGmt :: PostKey LocalTime
  PostGuid :: PostKey Guid
  PostId :: PostKey Int

deriving instance Show (PostKey a)

instance ShowTag PostKey Identity where
  showTaggedPrec PostDate    = showsPrec
  showTaggedPrec PostDateGmt = showsPrec
  showTaggedPrec PostGuid    = showsPrec
  showTaggedPrec PostId      = showsPrec

type PostMap = DMap PostKey Identity

addIfPresent
  :: (Applicative f, GCompare k, FromJSON v)
  => Object
  -> Text
  -> k v
  -> Parser (DMap k f)
  -> Parser (DMap k f)
addIfPresent o name key pDm =
  maybe id (insert key . pure) <$> (o .:? name) <*> pDm

instance Applicative f => FromJSON (DMap PostKey f) where
  parseJSON = withObject "Post" $ \o ->
      addIfPresent o "date" PostDate
    . addIfPresent o "date_gmt" PostDateGmt
    . addIfPresent o "guid" PostGuid
    . addIfPresent o "id" PostId
    $ pure empty

data Post =
  Post
  { postDate          :: LocalTime
  , postDateGmt       :: LocalTime
  , postGuid          :: Guid
  , postId            :: Int
  , postLink          :: Text
  , postModified      :: LocalTime
  , postModifiedGmt   :: UTCTime
  , postSlug          :: Text
  , postStatus        :: Status
  , postType          :: Text
  , postPassword      :: Text
  , postTitle         :: Text
  , postContent       :: Text
  , postAuthor        :: Text
  , postExcerpt       :: Text
  , postFeaturedMedia :: Text
  , postCommentStatus :: CommentStatus
  , postPingStatus    :: PingStatus
  , postFormat        :: Format
  , postMeta          :: Text
  , postSticky        :: Text
  , postTemplate      :: Text
  , postCategories    :: Text
  , postTags          :: Text
  }
  deriving Show

instance FromJSON Post where
  parseJSON =
    withObject "Post" $ \v ->
      Post
        <$> v .: "date"
        <*> v .: "date_gmt"
        <*> v .: "guid"
        <*> v .: "id"
        <*> v .: "link"
        <*> v .: "modified"
        <*> v .: "modifiedGmt"
        <*> v .: "slug"
        <*> v .: "status"
        <*> v .: "type"
        <*> v .: "password"
        <*> v .: "title"
        <*> v .: "content"
        <*> v .: "author"
        <*> v .: "excerpt"
        <*> v .: "featured_media"
        <*> v .: "comment_status"
        <*> v .: "ping_status"
        <*> v .: "format"
        <*> v .: "meta"
        <*> v .: "sticky"
        <*> v .: "template"
        <*> v .: "categories"
        <*> v .: "tags"

data ListPosts =
  ListPosts
  { listPostsContext           :: Maybe Context
  , listPostsPage              :: Maybe Int
  , listPostsPerPage           :: Maybe Int
  , listPostsSearch            :: Maybe Text
  , listPostsAfter             :: Maybe LocalTime
  , listPostsAuthor            :: Maybe Int
  , listPostsAuthorExclude     :: Maybe (Set Int)
  , listPostsBefore            :: Maybe LocalTime
  , listPostsExclude           :: Maybe (Set Int)
  , listPostsInclude           :: Maybe (Set Int)
  , listPostsOffset            :: Maybe Int
  , listPostsOrder             :: Maybe Order
  , listPostsSlug              :: Maybe (Set Text)
  , listPostsStatus            :: Maybe Status
  , listPostsCategories        :: Maybe (Set Text)
  , listPostsCategoriesExclude :: Maybe (Set Text)
  , listPostsTags              :: Maybe (Set Text)
  , listPostsTagsExclude       :: Maybe (Set Text)
  , listPostsSticky            :: Maybe Sticky
  }
  deriving Show

instance ToJSON ListPosts where
  toJSON ListPosts{..} =
    object []
    -- object
    -- [ "context"            .= listPostsContext
    -- , "page"               .= listPostsPage
    -- , "per_page"           .= listPostsPerPage
    -- , "search"             .= listPostsSearch
    -- , "after"              .= listPostsAfter
    -- , "author"             .= listPostsAuthor
    -- , "author_exclude"     .= listPostsAuthorExclude
    -- , "before"             .= listPostsBefore
    -- , "exclude"            .= listPostsExclude
    -- , "include"            .= listPostsInclude
    -- , "offset"             .= listPostsOffset
    -- , "order"              .= listPostsOrder
    -- , "slug"               .= listPostsSlug
    -- , "status"             .= listPostsStatus
    -- , "categories"         .= listPostsCategories
    -- , "categories_exclude" .= listPostsCategoriesExclude
    -- , "tags"               .= listPostsTags
    -- , "tags_exclude"       .= listPostsTagsExclude
    -- , "sticky"             .= listPostsSticky
    -- ]

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

newtype Guid =
  Guid { rendered :: Text }
  deriving (Show)

instance FromJSON Guid where
  parseJSON = withObject "guid" $ \v ->
    Guid <$> v .: "rendered"

deriveGEq ''PostKey
deriveGCompare ''PostKey
deriveGShow ''PostKey

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.WordPress.Types.Post where

import           Data.Aeson     (FromJSON (..), ToJSON (..), Value (Bool),
                                 object, withObject, (.:), (.=))
import           Data.Semigroup ((<>))
import           Data.Set       (Set)
import           Data.Text      (Text)
import           Data.Time      (LocalTime, UTCTime)

data Post =
  Post
  { postDate          :: LocalTime
  , postDateGmt       :: UTCTime
  , postGuid          :: Text
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
        <$> v .: "postDate"
        <*> v .: "postDateGmt"
        <*> v .: "postGuid"
        <*> v .: "postId"
        <*> v .: "postLink"
        <*> v .: "postModified"
        <*> v .: "postModifiedGmt"
        <*> v .: "postSlug"
        <*> v .: "postStatus"
        <*> v .: "postType"
        <*> v .: "postPassword"
        <*> v .: "postTitle"
        <*> v .: "postContent"
        <*> v .: "postAuthor"
        <*> v .: "postExcerpt"
        <*> v .: "postFeaturedMedia"
        <*> v .: "postCommentStatus"
        <*> v .: "postPingStatus"
        <*> v .: "postFormat"
        <*> v .: "postMeta"
        <*> v .: "postSticky"
        <*> v .: "postTemplate"
        <*> v .: "postCategories"
        <*> v .: "postTags"

data ListPost =
  ListPost
  { listPostContext           :: Maybe Context
  , listPostPage              :: Maybe Int
  , listPostPerPage           :: Maybe Int
  , listPostSearch            :: Maybe Text
  , listPostAfter             :: Maybe LocalTime
  , listPostAuthor            :: Maybe Int
  , listPostAuthorExclude     :: Maybe (Set Int)
  , listPostBefore            :: Maybe LocalTime
  , listPostExclude           :: Maybe (Set Int)
  , listPostInclude           :: Maybe (Set Int)
  , listPostOffset            :: Maybe Int
  , listPostOrder             :: Maybe Order
  , listPostSlug              :: Maybe (Set Text)
  , listPostStatus            :: Maybe Status
  , listPostCategories        :: Maybe (Set Text)
  , listPostCategoriesExclude :: Maybe (Set Text)
  , listPostTags              :: Maybe (Set Text)
  , listPostTagsExclude       :: Maybe (Set Text)
  , listPostSticky            :: Maybe Sticky
  }
  deriving Show

instance ToJSON ListPost where
  toJSON ListPost{..} =
    object
    [ "context"           .= listPostContext
    , "page"              .= listPostPage
    , "per_page"           .= listPostPerPage
    , "search"            .= listPostSearch
    , "after"             .= listPostAfter
    , "author"            .= listPostAuthor
    , "author_exclude"     .= listPostAuthorExclude
    , "before"            .= listPostBefore
    , "exclude"           .= listPostExclude
    , "include"           .= listPostInclude
    , "offset"            .= listPostOffset
    , "order"             .= listPostOrder
    , "slug"              .= listPostSlug
    , "status"            .= listPostStatus
    , "categories"        .= listPostCategories
    , "categories_exclude" .= listPostCategoriesExclude
    , "tags"              .= listPostTags
    , "tags_exclude"       .= listPostTagsExclude
    , "sticky"            .= listPostSticky
    ]

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
    "open" -> pure CommentsOpen
    "closed" -> pure CommentsClosed
    _ -> fail $ "Unknown comment status " <> show v

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
    "open" -> pure PingsOpen
    "closed" -> pure PingsClosed
    _ -> fail $ "Unknown ping status " <> show v

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
    "aside" -> pure Aside
    "chat" -> pure Chat
    "gallery" -> pure Gallery
    "link" -> pure Link
    "image" -> pure Image
    "quote" -> pure Quote
    "status" -> pure Status
    "video" -> pure Video
    "audio" -> pure Audio
    _ -> fail $ "Unknown format " <> show v

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

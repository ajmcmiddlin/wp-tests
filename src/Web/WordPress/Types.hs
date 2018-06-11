module Web.Wordpress.Types where

import           Data.Aeson (FromJSON (..), ToJSON (..))
import           Data.Text  (Text)
import           Data.Time  (LocalTime, UTCTime)

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

data Status =
    Publish
  | Future
  | Draft
  | Pending
  | Private
  deriving Show

data CommentStatus =
    CommentsOpen
  | CommentsClosed
  deriving Show

data PingStatus =
    PingsOpen
  | PingsClosed
  deriving Show

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

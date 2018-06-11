module Web.WordPress.Types.Post where

import           Data.Aeson (FromJSON (..), ToJSON (..))
import           Data.Set   (Set)
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

data Context =
    View
  | Edit
  | Embed
  deriving Show

data Order =
    Asc
  | Desc
  deriving Show

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

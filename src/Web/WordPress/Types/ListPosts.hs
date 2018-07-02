{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

{-# OPTIONS_GHC -Wno-unused-matches#-}

module Web.WordPress.Types.ListPosts where

import           Data.Aeson               (ToJSON (..), Value (String))
import           Data.Aeson.Types         (ToJSON1 (..), toJSON1)
import           Data.Dependent.Map       (DMap)
import           Data.Dependent.Sum       (EqTag (..), ShowTag (..))
import           Data.Functor.Classes     (Eq1, Show1, eq1, showsPrec1)
import           Data.Functor.Identity    (Identity (..))
import           Data.GADT.Compare.TH     (deriveGCompare, deriveGEq)
import           Data.GADT.Show.TH        (deriveGShow)
import           Data.List.NonEmpty       (NonEmpty)
import           Data.Some                (Some (This))
import           Data.Text                (Text)
import           Data.Time                (LocalTime)
import           Web.HttpApiData          (ToHttpApiData (toQueryParam))

import           Data.GADT.Aeson          (GKey (..), ToJSONViaKey (..),
                                           toJSONDMap)
import           Servant.QueryParamMap    (ToQueryParamKey (..),
                                           ToQueryParamKeyValues (..),
                                           defaultToQueryParamKeyValues,
                                           nonEmptyQueryParamKeyValues,
                                           toQueryParam1)
import           Web.WordPress.Types.Post (Author, Context, Status, Sticky)

type ListPostsMap = DMap ListPostsKey Identity

data ListPostsKey a where
  ListPostsContext           :: ListPostsKey Context
  ListPostsPage              :: ListPostsKey Int
  ListPostsPerPage           :: ListPostsKey Int
  ListPostsSearch            :: ListPostsKey Text
  ListPostsAfter             :: ListPostsKey LocalTime
  ListPostsAuthor            :: ListPostsKey Author
  ListPostsAuthorExclude     :: ListPostsKey (NonEmpty Author)
  ListPostsBefore            :: ListPostsKey LocalTime
  ListPostsExclude           :: ListPostsKey (NonEmpty Int)
  ListPostsInclude           :: ListPostsKey (NonEmpty Int)
  ListPostsOffset            :: ListPostsKey Int
  ListPostsOrder             :: ListPostsKey Order
  ListPostsSlug              :: ListPostsKey (NonEmpty Text)
  ListPostsStatus            :: ListPostsKey Status
  ListPostsCategories        :: ListPostsKey (NonEmpty Text)
  ListPostsCategoriesExclude :: ListPostsKey (NonEmpty Text)
  ListPostsTags              :: ListPostsKey (NonEmpty Text)
  ListPostsTagsExclude       :: ListPostsKey (NonEmpty Text)
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

instance ToQueryParamKey ListPostsKey where
  toQueryParamKey = toFieldName

instance ToQueryParamKeyValues ListPostsKey Identity where
  toQueryParamKeyValues ListPostsContext = defaultToQueryParamKeyValues ListPostsContext
  toQueryParamKeyValues ListPostsPage = defaultToQueryParamKeyValues ListPostsPage
  toQueryParamKeyValues ListPostsPerPage = defaultToQueryParamKeyValues ListPostsPerPage
  toQueryParamKeyValues ListPostsSearch = defaultToQueryParamKeyValues ListPostsSearch
  toQueryParamKeyValues ListPostsAfter = defaultToQueryParamKeyValues ListPostsAfter
  toQueryParamKeyValues ListPostsAuthor = defaultToQueryParamKeyValues ListPostsAuthor
  toQueryParamKeyValues ListPostsAuthorExclude = nonEmptyQueryParamKeyValues ListPostsAuthorExclude
  toQueryParamKeyValues ListPostsBefore = defaultToQueryParamKeyValues ListPostsBefore
  toQueryParamKeyValues ListPostsExclude = nonEmptyQueryParamKeyValues ListPostsExclude
  toQueryParamKeyValues ListPostsInclude = nonEmptyQueryParamKeyValues ListPostsInclude
  toQueryParamKeyValues ListPostsOffset = defaultToQueryParamKeyValues ListPostsOffset
  toQueryParamKeyValues ListPostsOrder = defaultToQueryParamKeyValues ListPostsOrder
  toQueryParamKeyValues ListPostsSlug = nonEmptyQueryParamKeyValues ListPostsSlug
  toQueryParamKeyValues ListPostsStatus = defaultToQueryParamKeyValues ListPostsStatus
  toQueryParamKeyValues ListPostsCategories = nonEmptyQueryParamKeyValues ListPostsCategories
  toQueryParamKeyValues ListPostsCategoriesExclude = nonEmptyQueryParamKeyValues ListPostsCategoriesExclude
  toQueryParamKeyValues ListPostsTags = nonEmptyQueryParamKeyValues ListPostsTags
  toQueryParamKeyValues ListPostsTagsExclude = nonEmptyQueryParamKeyValues ListPostsTagsExclude
  toQueryParamKeyValues ListPostsSticky = defaultToQueryParamKeyValues ListPostsSticky

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

instance ToJSON1 f => ToJSON (DMap ListPostsKey f) where
  toJSON = toJSONDMap

instance Eq1 f => EqTag ListPostsKey f where
  eqTagged ListPostsContext ListPostsContext                     = eq1
  eqTagged ListPostsPage ListPostsPage                           = eq1
  eqTagged ListPostsPerPage ListPostsPerPage                     = eq1
  eqTagged ListPostsSearch ListPostsSearch                       = eq1
  eqTagged ListPostsAfter ListPostsAfter                         = eq1
  eqTagged ListPostsAuthor ListPostsAuthor                       = eq1
  eqTagged ListPostsAuthorExclude ListPostsAuthorExclude         = eq1
  eqTagged ListPostsBefore ListPostsBefore                       = eq1
  eqTagged ListPostsExclude ListPostsExclude                     = eq1
  eqTagged ListPostsInclude ListPostsInclude                     = eq1
  eqTagged ListPostsOffset ListPostsOffset                       = eq1
  eqTagged ListPostsOrder ListPostsOrder                         = eq1
  eqTagged ListPostsSlug ListPostsSlug                           = eq1
  eqTagged ListPostsStatus ListPostsStatus                       = eq1
  eqTagged ListPostsCategories ListPostsCategories               = eq1
  eqTagged ListPostsCategoriesExclude ListPostsCategoriesExclude = eq1
  eqTagged ListPostsTags ListPostsTags                           = eq1
  eqTagged ListPostsTagsExclude ListPostsTagsExclude             = eq1
  eqTagged ListPostsSticky ListPostsSticky                       = eq1
  eqTagged _ _ = \_ _ -> False

data Order =
    Asc
  | Desc
  deriving (Eq, Show)

instance ToHttpApiData Order where
  toQueryParam = \case
    Asc -> "asc"
    Desc -> "desc"

instance ToJSON Order where
  toJSON = String . toQueryParam

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


deriveGEq ''ListPostsKey
deriveGCompare ''ListPostsKey
deriveGShow ''ListPostsKey

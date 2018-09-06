{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Form.Example where

import qualified Data.Aeson      as JSON
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

import           Form.Question
import           Form.Validation


newtype Name
  = Name Text
  deriving stock   (Generic, Show)
  deriving newtype (JSON.ToJSON, Question)


newtype Age
  = Age Int
  deriving stock   (Generic, Show)
  deriving newtype (JSON.ToJSON, Question)


newtype LikesDogs
  = LikesDogs Bool
  deriving stock   (Generic, Show)
  deriving newtype (JSON.ToJSON, Question)


data Person
  = Person
      { name      :: Name
      , age       :: Age
      , likesDogs :: LikesDogs
      , friends   :: [Person]
      }
  deriving stock    (Generic, Show)
  deriving anyclass (JSON.ToJSON, PopulatedBy PersonForm)


data PersonForm
  = PersonForm
      { name      :: Name
      , age       :: Age
      , likesDogs :: LikesDogs
      , friends   :: [Nested PersonForm]
      }
  deriving stock    (Generic, Show)
  deriving anyclass (FormValidation)

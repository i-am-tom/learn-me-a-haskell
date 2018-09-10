{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

{-|
Module      : Contains
Description : Extract sets of types a product.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

Csongor Kiss' [generic-lens](https://github.com/kcsongor/generic-lens) package
is a masterpiece of type-level wizardry, and I thoroughly recommend that you
give it a look if you're not already using it. Among the /many/ things it does
@generic-lens@ provides a 'typed' function that provides you a lens to a
particular type inside a structure providing that it is always unique within
the structure.

This module is a tiny extension of the idea to allow us to extract a list of
types as an 'HList'. It's really not comparable.

>>> :{
data Person
  = Person
      { name      :: String
      , age       :: Int
      , likesDogs :: Bool
      }
  deriving (G.Generic)
:}

>>> getTypes @'[String] (Person "Tom" 25 True)
HCons "Tom" (HNil)

-}
module Contains where

import Control.Applicative (Const (..))
import           Data.Kind             (Type)
import qualified GHC.Generics          as G

import           Data.Generics.Product (HasType (..))
import           HList                 (HList (..))


-- | Check that a given set of types exist within some input type, and retrieve
-- them as an HList. All the real tricks (and glorious type errors) come from
-- @kcsongor's awe-inspiring @generic-lens@ package.
class HasTypes (required :: [Type]) (input :: Type) where
  getTypes :: input -> HList required


-- | An empty list of requirements is trivially satisfied.
instance HasTypes '[] input where
  getTypes _ = HNil

-- | Recursively satisfy non-empty lists, and select the head using the 'typed'
-- lens from @generic-lens@. Note that we don't even need a type application
-- here because the type-checker can work it out from the result type (which
-- must be @head@!)
instance (HasType head input, HasTypes tail input)
    => HasTypes (head ': tail) input where
  getTypes input = HCons (getConst (typed Const input)) (getTypes input)

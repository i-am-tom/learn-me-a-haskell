{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module      : Bag.Include
Description : Generically "absorb" a value into a 'Bag'.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

This module lets us take any 'Generic'-implementing type and insert all its
"inner types" into the bag. This is particularly useful if we have a big
record, and want to import each field.

Let's imagine we have a couple types:

>>> :set -XDataKinds -XDeriveGeneric -XTypeApplications
>>> newtype Name = Name String deriving (Generic, Show)
>>> newtype Age  = Age  Int    deriving (Generic, Show)

... and some product type of these:

>>> :{
data Person = Person
  { name :: Name
  , age  :: Age
  }
  deriving (Generic, Show)
:}

The 'include' function lets us pull the 'Name' and 'Age' from a 'Person' type,
and insert them into a 'Bag':

>>> import Bag.QuantifiedInstances -- Import 'Bag' and you don't need this!
>>> include (Person (Name "Tom") (Age 25)) (mempty @(Bag '[Show]))
Bag (fromList [(Name,Name "Tom"),(Age,Age 25)])

Generics are __magical__.
-}
module Bag.Include where

import Data.Kind       (Constraint, Type)
import GHC.Generics
import Type.Reflection (Typeable)

import Bag.Types       (Bag (..), insert)
import Utils           (All)


-- | Use generics to "absorb" a value into a bag.
include
  :: ( Generic object
     , GInclude (Rep object) constraints
     )
  => object
  -> Bag constraints
  -> Bag constraints

include
  = ginclude . from


-- | Walk the generic representation of a type looking for "inner types" that
-- will be inserted into the bag.
class GInclude
    (rep         ::  Type -> Type       )
    (constraints :: [Type -> Constraint])
  where
    ginclude
      :: rep p
      -> Bag constraints
      -> Bag constraints


-- | Ignore any sort of metadata, as it won't help us.
instance GInclude constructor constraints
    => GInclude (M1 sort meta constructor) constraints where
  ginclude = ginclude . unM1


-- | If we see a sum type, populate the bag with whichever side is present.
instance
    ( GInclude left constraints
    , GInclude right constraints
    )
    => GInclude (left :+: right) constraints where
  ginclude sum bag = case sum of
    L1 branch -> ginclude branch bag
    R1 branch -> ginclude branch bag


-- | If we see a product type, populate the bag with both sides.
instance
    ( GInclude left constraints
    , GInclude right constraints
    )
    => GInclude (left :*: right) constraints where
  ginclude (left :*: right) bag
    = ginclude left
    $ ginclude right bag


-- | Finally, the most interesting bit: when we encounter an "inner type",
-- assuming that the constraints of the bag are satisfied, we can insert it
-- straight into the bag using the 'insert' function. Job's a good'un!
instance (Typeable a, All constraints a)
    => GInclude (Rec0 a) constraints where
  ginclude = insert . unK1

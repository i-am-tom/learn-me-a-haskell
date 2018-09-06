{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Bag
Description : An opaque heterogeneous 'HashMap'.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

Often in life, we have plenty of data that we collect through all sorts of
means - say, interaction with users - in unpredictable orders and flows. This
makes it very difficult to determine, statically, which data will be available
at which point in time. Because of this, we end up writing huge types with
every field wrapped in 'Maybe'. Think of the 'Bag' as this idea pushesd to its
conclusion.

Inserts into the 'Bag' are guaranteed:

>>> :set -XDataKinds -XDeriveGeneric -XTypeApplications
>>> import Prelude hiding (lookup)
>>> import GHC.Generics
>>> newtype Name = Name String deriving (Show, Generic)
>>> newtype Age  = Age  Int    deriving (Show, Generic)

>>> let named = insert (Name "Tom") (mempty @(Bag '[Show]))
>>> named
Bag (fromList [(Name,Name "Tom")])

... but lookups are definitely not:

>>> lookup @Age named
Nothing

The basic API is just 'insert' and 'lookup', though this can be used to build
more powerful layers on top. For example, this module exposes an 'include'
function to hydrate a 'Bag' from types:

>>> :{
data Person
  = Person
      { name :: Name
      , age  :: Age
      }
  deriving (Generic, Show)
:}

>>> populated = include (Person (Name "Tom") (Age 25)) (mempty @(Bag '[Show]))
>>> populated
Bag (fromList [(Name,Name "Tom"),(Age,Age 25)])

... and we can hydrate types from a 'Bag'!

>>> populate @Person populated
Success (Person {name = Name "Tom", age = Age 25})

... and that's all, folks! You'll notice that, unlike the other experiments in
this repo, we're quite light on the ground for documentation. In particular,
we've given no thought to fancy type errors! Why? Well, there won't /be/ any
fancy type errors - once a value is in the 'Bag', you can only get it out again
in a 'Maybe'. There's no static, compile-time analysis of the 'Bag'!
-}
module Bag
  ( Bag
  , Bag'

  -- * Convenience functions
  --
  -- The full API of the 'Bag' type.

  , include
  , populate
  , populate'
  , insert
  , lookup
  ) where

import Bag.Include  (include)
import Bag.Populate (populate, populate')
import Bag.Types    (Bag, Bag', insert, lookup)

-- We can use these pragmas to say "these instances are only available when
-- building with a compiler new enough to understand how to parse them.
#if __GLASGOW_HASKELL__ >= 806
import Bag.QuantifiedInstances ()
#endif

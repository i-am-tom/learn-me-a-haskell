{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module      : HList.Types
Description : The 'HList' type and its instances.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

If 'OneOf' is the generalised sum type, 'HList' is the generalised product
type. Here, we use a type-level list to define the inhabitants (and their
types) within the list. This means we can perform well-typed operations on the
inhabitants.

>>> :t HNil
HNil :: HList '[]

>>> :t HCons "I am" (HCons 25 (HCons " - it's " (HCons True HNil)))
HCons "I am" (HCons 25 (HCons " - it's " (HCons True HNil)))
  :: Num head => HList '[[Char], head, [Char], Bool]
-}
module HList.Types where

import Data.Kind (Type)
import Utils     (Every)

-- | The 'HList' type is a generalised product, as it holds a product of any
-- number of values, the types of which are tracked using the type-level list.
-- We can think of @(a, b)@ as being isomorphic to @HList '[a, b]@
data HList (elements :: [Type]) :: Type where
  HNil  :: HList '[]
  HCons :: head -> HList tail -> HList (head ': tail)

instance Every elements Show => Show (HList elements) where

  -- | We can @Show@ our @HList@ if we can @Show@ every element within it. The
  -- format in which we print it isn't perfect - it would be nice to mimic
  -- something closer to the list syntax - but this at least means it may
  -- occasionally print expressions that are valid Haskell?

  show  HNil        = "HNil"
  show (HCons x xs) = "HCons " <> show x <> " (" <> show xs <> ")"

instance Every elements Eq => Eq (HList elements) where

  -- | If every element in our HList is @Eq@, we can check for equality. This
  -- is exactly as you'd imagine - two empty lists are equal, and two non-empty
  -- lists are equal if their heads are equal and their tails are equal.

  HNil       == HNil       = True
  HCons x xs == HCons y ys = x == y && xs == ys

instance (Every elements Eq, Every elements Ord) => Ord (HList elements) where

  -- | If every element is @Ord@, our @HList@ has an @Ord@ instance. Namely, we
  -- compare the heads, and recurse if they're equal. This is the same way as
  -- list's sorting works.

  compare (HCons x xs) (HCons y ys)
    = case compare x y of
        EQ -> compare xs ys
        no -> no

  -- | Ordering for empty lists is trivial - we treat empty HLists exactly as
  -- we treat the unit type - all units are equal (because there's only one).

  compare HNil HNil
    = EQ

instance Every elements Semigroup => Semigroup (HList elements) where

  -- | An `HList` is @Semigroup@ if all the elements are. Appending is
  -- point-wise (which is, I think the only thing that would type-check).

  HCons x xs <> HCons y ys = HCons (x <> y) (xs <> ys)
  HNil       <> HNil       = HNil

instance Monoid (HList '[]) where
  -- | An empty `HList` is trivially @Monoid@ as it is isomorphic to @()@.
  mempty = HNil

instance (Monoid x, Monoid (HList xs), Every xs Semigroup)
    => Monoid (HList (x ': xs)) where

  -- | A non-empty `HList` is @Monoid@ if we can fill every element with
  -- @mempty@.
  mempty = HCons mempty mempty

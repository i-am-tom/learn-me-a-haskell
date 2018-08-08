{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

{-|
Module      : HList.Fold
Description : Fold an HList using some constraint.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

The thing that makes an HList useful is the idea that we can hold a list of
different types. This does, however, mean that we can't implement our
traditional 'Foldable' methods like 'foldr', 'foldl', and 'foldMap'.

Instead, we'll use a /constrained/ fold. If we can do something for any
implementation of a given class, and we can verify that all the types in our
list have an instance of that class, we can call that function and mappend the
results together.

>>> :set -XTypeApplications
>>> fold @Show show (HCons 2 (HCons 2.0 (HCons "hello" HNil)))
"22.0\"hello\""

Note that we type apply the constraint constructor. This seems like duplication
for the 'Show' class, but the classes aren't always so simple. It's also worth
mentioning at this point that @((~) a)@ is a valid constraint. Why do we care?

This means that we can use the constraint to specify the type of the argument,
and this is why a constrained fold is more general than our 'Foldable' friends.
An example implementation of 'foldMap' is given in this file, and I encourage
you to try implementing 'foldl' and 'foldr' if this doesn't make much sense to
you.

>>> :set -XFlexibleContexts
>>> toList = foldMap (pure :: a -> [a])
>>> toList (HCons 'T' (HCons 'o' (HCons 'm' HNil)))
"Tom"
-}
module HList.Fold where

import Data.Kind   (Type, Constraint)
import HList.Types
import Prelude     hiding (foldMap)


class Fold (constraint :: Type -> Constraint) (list :: [Type]) where

  -- | The signature, I think, is pretty helpful here. We first need a function
  -- that will work for /any/ type implementing our constraint. Think of
  -- 'show': it returns a string for any type that implements 'Show'. We know
  -- all elements of the list satisfy this constraint, so we can call the
  -- function on each and mappend the results.
  --
  -- Don't forget that we can simply extract the results of these functions
  -- using '[]' as our (free) monoid.
  fold
    :: Monoid monoid
    => (forall element. constraint element => element -> monoid)
    -> HList list
    -> monoid


instance Fold constraint '[] where

  -- | Folding an empty list is nice and straightforward: we need to produce a
  -- monoid value from nowhere, we use 'mempty'.
  fold _ HNil = mempty


instance (constraint head, Fold constraint tail)
    => Fold constraint (head ': tail) where

  -- | Aside from the type application, this should also just look like a
  -- regular 'foldMap'. We need the type application because GHC isn't clever
  -- enough to work out that we'll want the same constraint all the way down.
  fold f (HCons x xs) = f x <> fold @constraint f xs


-- | Fold a heterogenous HList into a monoid value. Note the constraint: every
-- value in our list must equal the input type of our function. At that point,
-- we don't need to worry about the @forall@, and we can just talk about our
-- input type specifically.
foldMap
  :: forall element monoid list
   . (Fold ((~) element) list, Monoid monoid)
  => (element -> monoid)
  -> HList list
  -> monoid

foldMap f = fold @((~) element) f

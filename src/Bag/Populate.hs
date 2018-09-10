{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module      : Bag.Populate
Description : Attempt to build a given type using a 'Bag'.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

If the 'Bag.Include' module gave us ways to populate bags from product types
using generics, we can think of 'Bag.Populate' as the opposite: we are
populating product types from bags! Let's take the same example:

>>> :set -XDataKinds -XDeriveGeneric -XTypeApplications
>>> newtype Name = Name String deriving (Generic, Show)
>>> newtype Age  = Age  Int    deriving (Generic, Show)

>>> :{
data Person = Person
  { name :: Name
  , age  :: Age
  }
  deriving (Generic, Show)
:}

With a sufficiently-populated bag, we can build a 'Person'!

>>> import Bag.Types
>>> import Data.Validation (validation)
>>> let populated = insert (Name "Tom") $ insert (Age 25) $ mempty @Bag'
>>> populate' @Person populated
Just (Person {name = Name "Tom", age = Age 25})

Hooray! 'populate'' will do its best to fill a type's fields (or product
values), returning 'Maybe' the result. This is great, but it's annoying when it
fails: how do we know what was missing? 'populate'!

>>> populate @Person (mempty @Bag')
Failure ["Name","Age"]

Now, I know what you're thinking: these are /value-level strings/! Where are
the dependent types? Let's talk about an adventure I had this evening.

It is actually not too difficult to update `populate` to take a universal
continuation with the failure side containing a proxy of the missing types in a
type-level list. It's also not particularly difficult to write 'populate'' in
terms of 'populate'. What is /very/ hard, it turns out, is actually doing
anything with that list of types.

The problem is that we know /nothing/ about them. Nothing. If I want to get
their names, I need to know they're 'Typeable', or 'Generic', or /something/.
In fact, if I want to do anything, I need to know more than I know - I can't
even use them in an error because I don't know what they'll be until run-time!
What this means is that the list is largely useless. Boo.

This is an interesting trade-off to consider when we move away from type-level
indexing: there's not a lot we can do "safely" at run-time: we're just going to
have to deal with 'Maybe' values everywhere and sacrifice the opportunity for
comprehensive type-checking.

>>> populate @Person (insert (Name "Tom") (mempty @Bag'))
Failure ["Age"]

One last little trick: thanks to the `OVERLAPPING` pragma, we can treat some
types as "special cases" in this function. For example, this library will treat
a 'Maybe'-wrapped field as an "acceptable failure": if the value can't be found
in the bag, the field is set to 'Nothing'.

>>> :{
data MysteryPerson = MysteryPerson
  { name :: Name
  , age  :: Maybe Age
  }
  deriving (Generic, Show)
:}

>>> populate @MysteryPerson (insert (Name "Tom") (mempty @Bag'))
Success (MysteryPerson {name = Name "Tom", age = Nothing})

-}
module Bag.Populate where

import           Data.Kind       (Constraint, Type)
import           Data.Proxy      (Proxy (..))
import qualified Data.Text       as Text
import           Data.Validation (Validation (..))
import qualified Data.Validation as Validation
import           GHC.Generics
import           GHC.TypeLits    (ErrorMessage (..), KnownSymbol, TypeError)
import           Prelude         hiding (lookup)
import           Type.Reflection (Typeable (..))
import qualified Type.Reflection as Typeable

import           Bag.Types       (Bag (..), lookup)
import           Utils           (All, Every, TypeName, type (++))


-- | Attempt to populate a type using values from a dynamic bag. At run-time,
-- we can pattern-match on 'Validation' to see whether we succeeded, or which
-- types were missing. The latter may be useful if your pattern-matching is
-- exhaustive enough!
class Populate (structure :: Type) (constraints :: [Type -> Constraint]) where
  populate
    :: Bag constraints
    -> Validation [Text.Text] structure


-- | As per usual, we can get the instance for free with any valid (and
-- generics-implementing) type.
instance (Generic structure, GPopulate (Rep structure) constraints)
    => Populate structure constraints where
  populate
    = fmap to . gpopulate


-- | Like 'populate', but as a 'Maybe'. Just a little neater.
populate'
  :: forall structure constraints
   . Populate structure constraints
  => Bag constraints
  -> Maybe structure

populate' bag = case populate bag of
  Success object -> Just object
  Failure _      -> Nothing


-- | This class is where the magic happens. Once we know the generic rep for
-- our type, we can walk the tree looking for other types to insert into the
-- bag!
class GPopulate
    (rep         ::  Type -> Type)
    (constraints :: [Type -> Constraint])
  where
    gpopulate
      :: Bag constraints
      -> Validation [Text.Text] (rep p)


-- | We can ignore all forms of metadata entirely.
instance GPopulate constructor constraints
    => GPopulate (M1 sort meta constructor) constraints where
  gpopulate = fmap M1 . gpopulate


-- | If we encounter a product type, we populate the bag with both sides of the
-- product.
instance (GPopulate left constraints, GPopulate right constraints)
    => GPopulate (left :*: right) constraints where
  gpopulate bag
    = (:*:) <$> gpopulate bag <*> gpopulate bag


-- | Sums seem like a difficult one to clarify, so let's just ignore them.
instance TypeError
    (     'Text "I can't populate a sum type generically!"
    ':$$: 'Text "After all, which constructor should we try to populate?"
    )
    => GPopulate (left :+: right) constraints where

  -- | If this is something you did want to do, for example to populate an
  -- `Either e a`, you'd have to attempt to populate the sides, and then define
  -- how they would work together:
  --
  -- @
  --   result :: Maybe (Either e a)
  --   result = fmap Left (populate bag) <|> fmap Right (populate bag)
  -- @
  --
  -- This snippet allows us to provide an "order of preference" to our sum
  -- type.
  gpopulate bag = undefined


-- | The interesting instance is when we hit an "inner type". First of all, we
-- have to check that it matches the bag constraints (or we can't put it in the
-- bag!). Then, in case we /don't/ find the type, we need to know that it's
-- 'Typeable', so we can find the 'TypeRep', convert this to a 'TyCon', and
-- then finally get hold of the name of the type in order to tell the users it
-- was missing. Phew.
instance (Typeable inner, All constraints inner)
    => GPopulate (K1 R inner) constraints where
  gpopulate bag
    = case lookup bag of
        Just result ->
          Success (K1 result)

        Nothing ->
          Failure
            [ Text.pack
            $ Typeable.tyConName
            $ Typeable.typeRepTyCon
            $ Typeable.typeRep @inner
            ]

-- | One last sneaky trick: we can populate types whose values may be missing
-- by writing an overlapping instance looking out for a 'Maybe' constructor.
-- When it's spotted, there's no failure case: we just use the result of
-- 'lookup' directly! This means that we can populate values with optional
-- fields.
instance {-# OVERLAPPING #-} (Typeable inner, All constraints inner)
    => GPopulate (K1 R (Maybe inner)) constraints where
  gpopulate = Success . K1 . lookup

{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-|
Module      : Bag.Types
Description : The internals and core API of the bag type.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

We've seen 'HList' and 'HTree' - examples of /type-indexed heterogeneous
structures/. That is to say, in both of these cases, we know the types of all
the elements in the structure _explicitly_, and can manipulate them however we
want. However, this comes with a couple disadvantages:

- Types can become unmanageable as we do more complex operations: we have to
  care about what, specifically, enters and leaves the structure, and in what
  order, and even where /in/ the structure. Sometimes, we just don't need to
  know.

- The type-checker just isn't a very quick program. There's no caching, no loop
  fusion, you name it - programs at the type level simply run much more slowly.

The bag is a different structure: at its simplest, the type could be @Bag '[]@,
but contain 100,000 elements. This is what's interesting about this structure:
we have /no idea/ what's inside the bag. All we can do is list some constraints
that we require of all its inhabitants!

This means that type-checking is a much faster process as we don't have to do
anything clever at the type-level: it's just your regular old type-checking.

>>> :set -XDataKinds -XTypeApplications
>>> lookup @Bool $ insert True (mempty @Bag')
Just True
>>> lookup @Bool $ insert 3 (mempty @Bag')
Nothing

'Bag'' is the simplest of 'Bag' types, and has no constraints. However, if we
specify that our 'Bag' also asserts 'Show', we can 'show' it:

>>> import Bag.QuantifiedInstances -- You don't need this if you import Bag!
>>> insert True $ insert 5 $ insert "Tom" (mempty @(Bag '[Show]))
Bag (fromList [(Integer,5),(Bool,True),([Char],"Tom")])
-}
module Bag.Types where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Kind           (Constraint, Type)
import           Data.Proxy          (Proxy (..))
import           GHC.TypeLits        (ErrorMessage (..), TypeError)
import           Prelude             hiding (lookup)
import           Type.Reflection     (Typeable (..))
import qualified Type.Reflection     as Typeable

import           Utils               (All)


-- | 'Data.Dynamic' exposes the 'Dynamic' type, which existentialises some type
-- along with its 'TypeRep'. This version of 'Dynamic' extends the idea by
-- allowing for a list of further constraint-constructors (using a list to
-- allow for an "empty set" of constructors).
data Dynamic (constraints :: [Type -> Constraint]) where
  Dynamic
    :: All constraints item
    => Typeable.TypeRep item
    -> item
    -> Dynamic constraints


-- | The actual type under the bag is a little bit horrifying: we hold a
-- hashmap from 'SomeTypeRep' - a label for some unknown type (spoiler: we /do/
-- know what this is) - to our 'Dynamic' type with /at least/ a 'Typeable'
-- constraint. In practice, we don't have to worry too much about this type
-- beyond writing instances, as it's all hidden behind 'Bag' and managed by
-- 'insert' and 'lookup'.
type Innards cs = HashMap Typeable.SomeTypeRep (Dynamic (Typeable ': cs))


-- | A bag is a constraints-indexed wrapper around the ugly innards. Because
-- it's a 'HashMap', it's conveniently also 'Semigroup' and 'Monoid'. Note
-- that, in order to write instances that depend on the constraints containing
-- certain constructors, we'll need @QuantifiedConstraints@. Why?
--
-- We need to say that, /for all/ types, applying a type to these constructors
-- will give rise to the constraint I need. For example, I can only 'Show' a
-- 'Bag' if I know that the constraints /on/ it assure that everything /in/ it
-- has a 'Show' instance.
newtype Bag (constraints :: [Type -> Constraint])
  = Bag (Innards constraints) deriving (Semigroup, Monoid)


-- | The simplest of 'Bag' instances is the one whose only constraint is
-- 'Typeable'. This cannot, for example, be serialised or printed for
-- debugging, and is thus really just a convenient "type store".
type Bag'
  = Bag '[]


-- | We can insert a value into a 'Bag' if all the 'Bag''s constraints are
-- satisfied /and/ we have a 'Typeable' instance for the type. This 'Typeable'
-- constraint is about the only internal detail that actually leaks, but it
-- tells our user what's really going on here. So... what's really going on
-- here?
--
-- 'Typeable' is a class that gives us a way of producing a 'SomeTypeRep' for a
-- given type, which we sometimes call a "fingerprint": some unique identifier
-- that only applies to this particular type. What's useful is that it has an
-- 'Ord' instance, so we can use this 'SomeTypeRep' as a key in our 'Map'!
--
-- The other consequence of this is that we can /know/ that, if I look up a
-- particular 'SomeTypeRep' as a key in the maw and find a value, what I'm
-- looking at is /definitely/ the type that produces this rep. This leads us
-- nicely onto 'Dynamic'...
--
-- 'Dynamic' existentialises a type, but also packs up its type rep (note:
-- unlike 'SomeTypeRep', this is indexed by the type - @TypeRep a@ vs
-- @SomeTypeRep@). Here's the magic: if I have @(a, TypeRep a)@ (for some
-- unknown @a@) and the typeRep for some type I /do/ known, I can check whether
-- the type reps match. If they /do/, GHC can "learn" that these types are
-- equal by pattern-matching on @HRefl@ (a "proof"). We'll see more of this in
-- 'lookup', so be sure to check the source!
insert
  :: forall value constraints
   . (All constraints value, Typeable value)
  => value
  -> Bag constraints
  -> Bag constraints

insert value (Bag bag)
  = Bag (HashMap.insert typeRep dynamic bag)
  where
    typeRep = Typeable.someTypeRep (Proxy @value)
    dynamic = Dynamic Typeable.typeRep value
    -- GHC infers this for us  ^^^^^^^!


-- | Insertion is pretty straightforward: generate the 'SomeTypeRep' as the
-- key, then insert a new 'Dynamic' value holding the actual 'TypeRep' and our
-- value. Lookup is a little more complicated, though: we need to convince the
-- compiler to give us a real (non-'Dynamic') type for the thing we get back!
--
-- Firstly, we use the result type of the function to tell us the 'SomeTypeRep'
-- we need to look up, and we get back a 'Dynamic' value. Then, we use
-- 'eqTypeRep' from the 'Typeable' package to check whether our generated
-- 'TypeRep' matches the one in 'Dynamic'. Now, why doesn't this function
-- return 'Bool'? What's an 'HRefl'?
--
-- @
--   data (a :: k) :~~: (b :: l) where
--     HRefl :: a :~~: a
-- @
--
-- What we're saying here is that the only constructor I have for this type is
-- one that requires both sides to be the same type ('HRefl' is short, I'd
-- imagine, for /heterogeneous reflexivity/ - two types are the same and have
-- the same kind!). So, when we run 'eqTypeRep', we /maybe/ get an 'HRefl'.
-- However, if we /do/, and we pattern-match on it, the fact that the two sides
-- are equal "comes into scope", and GHC "learns" about this equality.
--
-- At that point, we have learnt that the existential in our 'Dynamic' is
-- actually the type that we want as a result, and we can return it! Hooray!
-- Note that, if we /don't/ get back an 'HRefl' and pattern-match on it, GHC
-- doesn't learn this - how could it? - and in the 'Nothing' branch, you'd get
-- an error if you tried to return @value@! GHC actually says that it couldn't
-- deduce that @item ~ value@ - "I have no proof that these two things are the
-- same, so you can't treat them as though they are!"
lookup
  :: forall value constraints
   . (All constraints value, Typeable value)
  => Bag constraints
  -> Maybe value

lookup (Bag bag)
  = HashMap.lookup typeRep bag >>= \(Dynamic rep value) ->
      case Typeable.eqTypeRep rep (Typeable.typeRep @value) of
        Just Typeable.HRefl -> Just value
        _                   -> Nothing
  where
    typeRep = Typeable.someTypeRep (Proxy @value)

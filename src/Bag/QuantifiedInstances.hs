{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

{-|
Module      : Bag.QuantifiedInstances
Description : Derived instances for Bag using QuantifiedConstraints.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

Thanks to version-specific conditionals in the cabal file, as well as CPP
pragmas in the root @Bag@ module, this file only actually gets built if GHC is
modern enough to understand @QuantifiedConstraints@ (at least version 8.6.0).
This means that we can play with some new toys without forcing our users to
figure out how to upgrade!

The @QuantifiedConstraints@ extension allows us to use @forall@ in our
constraints. This can be useful when we want to express relationships between
constraint constructors (for @t@ to be a monad transformer, @t m@ should be a
monad /for all/ monads @m@), which is, as luck would have it, what we need to
do!

The problem is this: my 'Bag' type is indexed by a set of constraints that hold
for all its inhabitants. In order to write, say, 'Show' for a 'HashMap' - the
internal representation of 'Bag' - I need my value type to have a 'Show'
instance. So, what I need to say is "This 'Show' instance works for any bag
whose constraints contain (or imply) 'Show'". In other words, we can write a
'Show' instance if, /for all/ types 't', having the given constraints implies
that it has a 'Show' instance. We need a @forall@ in our constraints!
-}
module Bag.QuantifiedInstances where

import qualified Type.Reflection as Typeable

import           Bag.Types
import           Utils           (All)


-- | 'Transparent' is like 'Identity', but with an even simpler 'Show'
-- instance: we literally just pass the call through to the underlying type.
newtype Transparent a
  = Transparent a
  deriving newtype (Eq, Show)


-- | We can 'Show' any 'Dynamic' whose @constraints@ are enough to imply
-- 'Show'. All we do is show the value within. Note the use of 'Transparent'
-- here, which highlights some interesting @QuantifiedConstraints@ behaviour...
--
-- When we remove the newtype, we get a funny-looking error when GHC tries to
-- write one of the other functions, such as 'showsPrec'. Specifically, it
-- complains that it couldn't deduce @All constraints (Dynamic constraints)@.
-- What?
--
-- This seems weird - it's not what we want, right? Why does GHC think it needs
-- to construct 'All constraints' for the whole 'Dynamic' type this time? As
-- far as I understand it (hat-tip to @kcsongor aka @Lowert who actually
-- figured this out and patiently explained), the problem is that we have two
-- matching instances for 'showsPrec':
--
-- - @(forall item. All constraints item => Show item) => Show (Dynamic
-- constraints)@, which is to say, "the one we're currently building" and would
-- let us use our implementation of 'show'.
--
-- - @(forall item. All constraints item => Show item)@ - the quantified
-- constraint itself!
--
-- Unfortunately for /us/, the latter is picked as the better candidate. It
-- /looks/ like the reason is that GHC prioritises "local instances" (which is
-- to say, the instances that arise from our /constraints/) over others
-- (including the one in our instance head). Because both result in 'Show'
-- heads that match our situation, both are matches. Disaster! Knowing this,
-- how do we get round the problem?
--
-- By giving 'Transparent' a 'Show' instance for any 'Show'able inner type, we
-- let GHC figure out 'Show (Transparent item)' any time it can figure out
-- 'Show item'. The crucial difference /now/ is that, when 'showsPrec' makes a
-- call to 'show' with a 'Dynamic' value, the instance head in the @forall@
-- /won't match/! This leaves us with the one instance - the one we actually
-- want to use - and we can once again have our other methods defined in terms
-- of 'show' (hat-tip to @goodacre_liam for this very sneaky idea).
--
-- (The alternative is that we'd have to define all the methods explicitly to
-- make sure GHC doesn't have to make any of these decisions for us).
instance (forall item. All constraints item => Show (Transparent item))
    => Show (Dynamic constraints) where
  show (Dynamic _ x) = show (Transparent x)


-- | Having gone through all the pain up there, we can get the 'Show' instance
-- for bag automatically derived for us with @StandaloneDeriving@! Truly a
-- magical experience.
deriving instance (forall item. All constraints item => Show (Transparent item))
  => Show (Bag constraints)

---

-- | As another example, we can implement binary operations for constraints by
-- using their type reps to determine whether the two types are the same. In
-- this case, our quantified constraint tells us that our constraints on the
-- 'Dynamic' value will imply 'Eq', so we can use '(==)' as long as we know
-- they're the same type. By pattern matching on an 'HRefl', we bring into
-- scope the proof that @x@ and @y@ have the same type, and we can use regular
-- old '(==)'!
instance (forall item. All constraints item => Eq (Transparent item))
    => Eq (Dynamic constraints) where
  Dynamic repX x == Dynamic repY y
    = case Typeable.eqTypeRep repX repY of
        Just Typeable.HRefl -> Transparent x == Transparent y
        Nothing             -> False


-- | Exactly as with 'Show', we can derive 'Eq' based on the instance for
-- 'HashMap' and 'Dynamic'! @StandaloneDeriving@ is magical.
deriving instance (forall item. All constraints item => Eq (Transparent item))
  => Eq (Bag constraints)

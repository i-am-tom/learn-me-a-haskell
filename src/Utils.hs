{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Utils where

import Data.Kind    (Constraint)
import GHC.TypeLits

-- | Produce a constraint from a type-level list using some constraint
-- constructor. See any instance in this module for usage examples.
type family Every (xs :: [k]) (c :: k -> Constraint) :: Constraint where

  -- An empty list of constraints has no constraint.
  Every '[] c = ()

  -- A non-empty list of constraints is the head constraint... and all the
  -- others... constraint-flattening is weird.
  Every (x ': xs) c = (c x, Every xs c)


-- | Produce one of two possible types based on whether a type-level boolean be
-- true or false. Note that this is /eager/, so both paths will be evaluated
-- fully. Worth bearing this in mind when using it with complex logic that
-- comes up more often than as a type error.
type family If (predicate :: Bool) (true :: k) (false :: k) where

  -- A true value picks the left...
  If 'True  true _     = true

  -- ... and a false value picks the right.
  If 'False _    false = false


-- | Recursively calculate the length of a type-level list.
type family Length (list :: [k]) :: Nat where

  -- Empty lists are 0-length.
  Length '[]       = 0

  -- Non-empty lists are one element longer than their tail.
  Length (x ': xs) = 1 + Length xs


-- | Get the element at a given index from a type-level list.
type family Lookup (n :: Nat) (list :: [k]) :: k where

  -- Looking up 0 is just getting the head of a list.
  Lookup 0 (x ': xs) = x

  -- Looking up any bigger number is looking up one less in the tail.
  Lookup n (x ': xs) = Lookup (n - 1) xs


-- | Is the third number bigger greater than or equal to the first and less
-- than or equal to the second? Full disclosure: I sat down, wrote up a truth
-- table, then let GHC figure out where my overlaps were. 90% sure this is
-- right?
type family Between (lower :: Nat) (upper :: Nat) (input :: Nat) :: Bool where

  Between 0 h 0 = 'True
  Between 0 0 n = 'False
  Between 0 h n =  Between 0 (h - 1) (n - 1)
  Between l 0 n = 'False
  Between l h 0 = 'False
  Between l h n =  Between (l - 1) (h - 1) (n - 1)

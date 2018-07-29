{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : OneOf.Types
Description : The 'OneOf' type and its instances.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

'OneOf' is @Eq@, @Ord@, @Show@, and @Semigroup@. In all cases, we require all
elements within the 'OneOf' to have these intsances, as we effectively just
delegate to them.

>>> Here "hello"            :: OneOf '[String, Bool, [()]]
>>> There (Here True)       :: OneOf '[String, Bool, [()]]
>>> There (There (Here [])) :: OneOf '[String, Bool, [()]]
-}
module OneOf.Types where

import Data.Kind (Constraint, Type)

-- | The 'OneOf' type is effectively a generalised @Either@, in the sense that
-- @Either a b@ is isomorphic to @OneOf '[a, b]@. 'OneOf', however, will hold
-- one of any number of greater-than-zero possible types.
data OneOf (xs :: [Type]) where
  Here  :: x -> OneOf (x ': xs)
  There :: OneOf xs -> OneOf (y ': xs)

-- | Produce a constraint from a type-level list using some constraint
-- constructor. See any instance in this module for usage examples.
type family Every (xs :: [k]) (c :: k -> Constraint) :: Constraint where

  -- An empty list of constraints is no constraint.
  Every '[]       c = ()

  -- A non-empty list of constraints is the head constraint... and all the
  -- others... constraint-flattening is weird.
  Every (x ': xs) c = (c x, Every xs c)

instance Every xs Show => Show (OneOf xs) where

  -- | We can @Show@ a @OneOf xs@ as long as @Every@ member of @xs@ can @Show@.
  -- What @Every@ does is bring evidence for this into scope for all the types.

  show (Here  x) = "Here "   <> show x
  show (There x) = "There (" <> show x <> ")"

instance Every xs Eq => Eq (OneOf xs) where

  -- | We can @Eq@ a @OneOf xs@ in the same way as we can @Show@: if all things
  -- are @Eq@, then the 'OneOf' is @Eq@.

  Here  x == Here  y = x == y
  There x == There y = x == y
  _       == _       = False

instance (Every xs Eq, Every xs Ord) => Ord (OneOf xs) where
  -- | We can have @Ord@ if all of our 'OneOf' members are @Ord@. Interesting
  -- quirk of GHC here: we can't magically get @Every xs Eq@ given @Every xs
  -- Ord@ here (maybe this is that entailment thing Kmett talks about?)
  -- because, while @Ord@ might imply @Eq@, @Every xs Ord@ is a very different
  -- constraint.

  Here  _ <= There _ = True
  There _ <= Here  _ = False

  Here  x <= Here  y = x <= y
  There x <= There y = x <= y

instance Every xs Semigroup => Semigroup (OneOf xs) where

  -- | The @Semigroup@ instance is slightly different to that of @Either@: if
  -- we were using @OneOf '[a, b]@ as an analogue to @Either a b@, the
  -- difference in behaviour is that we're requiring @Semigroup@ of the @a@,
  -- and will @mappend@ two @a@ values if we're given them. In the case of
  -- differing values, we'll prefer the one latest in the list.

  Here  x <> Here  y = Here  (x <> y)
  There x <> Here  _ = There  x
  Here  _ <> There y = There       y
  There x <> There y = There (x <> y)

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module      : OneOf.Inject
Description : Injection function for producing 'OneOf' values.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

Dealing with a 'OneOf' can quickly become ugly if you have to write all the
constructors yourself. To that end, @inject@ uses the type of its argument and
the type of its result to insert them for you:

>>> :set -XDataKinds
>>> inject True :: OneOf '[Bool, String]
Here True

>>> inject "Hello" :: OneOf '[Bool, String]
There (Here "Hello")
-}
module OneOf.Inject where

import Data.Kind    (Type)
import GHC.TypeLits (ErrorMessage(..), TypeError)
import OneOf.Types  (OneOf(..))

-- | Really underwhelming class: assuming that @x@ is an element of @xs@, we
-- can decorate @x@ in constructors to build a @OneOf xs@ mechanically.
class Inject (x :: Type) (xs :: [Type]) where
  -- Lift a type into a 'OneOf'.
  inject :: x -> OneOf xs

-- | This class is identical to @Inject@, except for the @initial@ parameter,
-- which is a copy of whatever the original list was. Keeping this around means
-- that we can produce some much nicer type errors.
class InjectLoop x xs initial where
  inject' :: x -> OneOf xs

-- | This is our @go@-function-style instance. Any time someone calls @inject@
-- from the @Inject@ typeclass, we'll just call @inject'@ and hold a copy of
-- the list at this point for errors.
instance InjectLoop x xs xs => Inject x xs where
  inject = inject' @_ @_ @xs

-- | If the head of our 'OneOf' list is the same type as the value we're
-- looking to inject, we just wrap our value in a @Here@ and we're done.
instance InjectLoop x (x ': xs) initial where
  inject' = Here

-- | If we're not so lucky, and the heads don't match, we'll @inject@ the value
-- into a @OneOf tail@, and then wrap that in a @There@.
instance {-# OVERLAPPABLE #-} InjectLoop x xs initial
    => InjectLoop x (y ': xs) initial where
  inject' = There . inject' @_ @_ @initial

-- | Conveniently, we know that anyone who manages to match this instance
-- /must/ have gone wrong. We can't construct a 'OneOf' with an empty list, so
-- the only reason we could ever match this instance is if we've failed to find
-- the type we're injecting inside the 'OneOf''s type list. Now that we're
-- carrying @initial@, we can show a pretty decent type error!
instance TypeError
      ( 'Text "You can't lift "  ':<>: 'ShowType x
  ':<>: 'Text " into "           ':<>: 'ShowType (OneOf initial)

  ':$$: 'Text "This is because " ':<>: 'ShowType x
  ':<>: 'Text " is not one of these types!"
      )
    =>  InjectLoop x '[] initial where
  inject' = undefined

{-|
Module      : OneOf
Description : A generalised @Either@ type.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

The @Either@ type has two constructors – @Left@ and @Right@ – to represent one
of two possible options. If we wanted to represent three possible options, we
could nest an @Either@ inside another, giving us @Left x@, @Right (Left y)@,
and @Right (Right z)@ to represent our three options. We can do this forever,
as long as we're happy to deal with the types that it will inflict upon us.

The @OneOf@ type is a generalisation of this idea: instead of nesting the type,
we declare the number of constructors we want using a type-level list of types.
We can, for example, give an isomorphism between @Either a b@ and
@OneOf '[a, b]@, and it is clear to see how we could extend this to a third,
fourth, and so on!
-}
module OneOf
  ( OneOf (..)

    -- * Convenience functions
    --
    -- We can work with a @OneOf@ directly using its constructors, but this
    -- becomes a pain as we generalise to larger sets of types. Consequently,
    -- we have three functions to make things a little easier.

  , interpret
  , inject
  , fold
  ) where

import OneOf.Fold   (fold, interpret)
import OneOf.Inject (inject)
import OneOf.Types  (OneOf (..))

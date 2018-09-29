{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module      : Utils.List
Description : A variadic lifting function.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

In Haskell, we can (and do!) define a suite of functions to lift operations
over applicative contexts:

@
  liftA1 :: Applicative f => (a -> b)           -> f a -> f b
  liftA2 :: Applicative f => (a -> b -> c)      -> f a -> f b -> f c
  liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
@

... However, there seems to be a reasonably clear pattern: all we're doing is
wrapping each parameter in our @f@ context. What if we could generalise this?

>>> :{
liftA1 :: Applicative f => (a -> b) -> f a -> f b
liftA1 = lift
:}

>>> :{
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 = lift
:}

>>> :{
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 = lift
:}
-}
module Utils.Lift
  ( Lift
  , lift
  ) where

import Data.Kind (Type)


-- | All the arguments are wrapped in some 'Applicative' @f@. As per usual,
-- we'll lift the function one argument at a time. If we think about the usual
-- definition of a lifting function - @f '<$>' a '<*>' b '<*>' ...@ - we see
-- that everything is just "applied" with '<*>' after the first one. This is an
-- annoying inconsistency, but we're in luck: @f '<$>' a@ just happens to be
-- equal to @'pure' f '<*>' a@! In this form, we get two helpful properties:
--
-- - Every argument is just '<*>'-applied, with no exceptions.
-- - Every argument - including the function itself - to 'lift' is @f@-wrapped.
--
-- Because of this, we end up with a _reasonably_ straightforward recursive
-- solution!
class Applicative context => Lift
    ( context :: Type -> Type ) -- Into which applicative do we want to lift?
    ( input   ::         Type ) -- What is our function?
    ( lifted  ::         Type ) -- What is our result?
    | lifted -> context        -- We can figure out the context from the result
    , input context -> lifted  -- we can figure out the result with the rest
  where
    -- | Note that @input@ is @context@-wrapped here: we're saying that the
    -- first argument to any call to 'lift'' is always wrapped in some
    -- 'Applicative'.
    lift' :: context input -> lifted


-- | If the input is a function, we can figure out the next parameter to the
-- output: it's the input's parameter, but wrapped in our 'Applicative'! After
-- that, we just recurse.
--
-- For reasons I still don't /really/ understand, the type inference gets much,
-- much better if we add the equality constraint instead of matching this in
-- the head. My current understanding is that polymorphic types cause problems
-- in instance matching because resolution might be ambiguous until we know
-- more, whereas constraints avoid this problem _and_ give information to the
-- constraint solver.
instance
    ( Applicative context
    , Lift context inputTail outputTail
    , output ~ (context input -> outputTail)
    )
    => Lift context (input -> inputTail) output where
  lift' fs as = lift' (fs <*> as)


-- | If we _don't_ have a function, we're done! At this point, we just return
-- whatever we have. Easy!
--
-- The @INCOHERENT@ pragma here looks a bit scary, but I'm sure it's safe. If
-- we try @OVERLAPPING@, we get a type error if we try to lift something like
-- (+): our type is @Num a => a -> a -> a@, and we don't know that @a@ isn't a
-- function, so we can't actually tell which instance is more relevant!
--
-- The @INCOHERENT@ pragma gets us around the problem. Firstly, it defers the
-- concrete type check until we actually use the function. At which point, GHC
-- will attempt to find a __non-incoherent__ instance to match. If no instance
-- is found, it will then look for an incoherent instance to match.
--
-- This is what we want! If we /can/ match the previous instance, we definitely
-- should, and this instance is simply a fall-back. The consequence? We retain
-- some pretty decent type inference!
--
-- >>> :t lift (++)
-- lift (++)
--   :: Applicative context => context [a] -> context [a] -> context [a]
--
-- >>> :t lift (+)
-- lift (+)
--   :: (Applicative context, Num a) =>
--      context a -> context a -> context a
instance {-# INCOHERENT #-}
    ( Applicative context
    , output ~ context input
    )
    => Lift context input output where
  lift' = id


-- | This is the function we'll actually expose. We lift the function into our
-- context, and then pass it to lift' to do all the work.
lift :: (Applicative context, Lift context input output) => input -> output
lift = lift' . pure


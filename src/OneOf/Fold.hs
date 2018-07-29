{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module      : OneOf.Fold
Description : Functions for working with a 'OneOf' value.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

Given a @OneOf '[x, y, z]@, there are two sensible ways to interpret this:

- If everything in its list – @[x, y, z]@ – satisfy some constraint @c@, and we
  can write some @∀x. c x => x -> r@, then we can call this function on the
  value, regardless of its position, and produce some result.

- Instead of using implementations of a typeclass, we can produce a fold,
  taking functions to work with each of the possible branches.

There are trade-offs with both. The first method leads to much neater code, as
we can simply write `interpret f xs` and get our "value" from inside. With the
latter, however, we'll save on boilerplate in cases where we would potentially
like many interpretations of the same value. The choice is yours.
-}
module OneOf.Fold where

import Data.Kind   (Constraint, Type)
import OneOf.Types (OneOf(..))

-- | Interpret a @OneOf xs@ value, using knowledge that every member of @xs@
-- satisfies the given constraint, @c@.

class Interpret (c :: Type -> Constraint) (xs :: [Type]) where

  -- | The usual rank-2 trick: "give me a function that relies on @c@'s
  -- interface to produce some value, and I'll produce that value".

  interpret :: (forall x. c x => x -> r) -> OneOf xs -> r

instance c x => Interpret c '[x] where

  -- | Interpret a singleton list. Nothing especially clever: we know by
  -- construction that we must be looking at the type that is actually inside
  -- the 'OneOf', so we can just call the function on it.

  interpret f (Here  x) = f x
  interpret _ (There _) = error "Impossible"

instance (tail ~ (x' ': xs), Interpret c tail, c x)
    => Interpret c (x ': x' ': xs) where

  -- | For a non-singleton list (empty lists are impossible within 'OneOf'), we
  -- pattern match on whether we've found it or not. If we have, we do as we
  -- did above with the singleton. If we don't, we recurse deeper into the
  -- 'OneOf'.
  --
  -- Note that we can avoid an overlapping instance by pattern-matching @tail@
  -- a @Cons@ deeper.

  interpret f (Here x ) = f x
  interpret f (There x) = interpret @c f x

-- | A @OneOf '[x, y]@ can be folded with @(x -> r) -> (y -> r) -> r@ – we just
-- take a function for each possible value, and then apply the one that is
-- relevant. This type family produces the signature, given the list.

type family FoldSignature (xs :: [Type]) r where
  -- More recursion: take the head, add its fold, recurse.
  FoldSignature (x ': xs) r = (x -> r) -> FoldSignature xs r

  -- If we have no inputs, here's our output!
  FoldSignature '[] r = r

-- | This type class builds the fold for a given 'OneOf' value. Once that has
-- happened, usage is straightforward:
--
-- > fold (inject True :: OneOf '[Int, String, Bool])
-- >   (\_ -> "Int!")
-- >   (\case
-- >       "hello" -> "String!"
-- >       _       -> "Rude string :(")
-- >   (\x -> if x then "YAY" else "BOO")
--
-- We can now fold out of our datatype just as we would with @either@.

class BuildFold xs result where

  -- | Fold a 'OneOf' value by providing a function for each possible type.
  --
  -- >>> :t fold (undefined :: OneOf '[a, b])
  -- fold (undefined :: OneOf '[a, b])
  --   :: (a -> result) -> (b -> result) -> result

  fold :: OneOf xs -> FoldSignature xs result

instance BuildFold '[x] result where

  -- | For a singleton list (again, empties are impossible), we needn't do
  -- anything too clever: we just apply the function to the head.

  fold (Here  x) f = f x
  fold (There _) _ = error "Impossible"

instance (tail ~ (x' ': xs), BuildFold tail result, Ignore tail result)
    => BuildFold (x ': x' ': xs) result where

  -- | Things get more tricky for a non-singleton list if you find a match; how
  -- do you pass it over all the arguments in your fold?
  --
  -- Again, we avoid an overlapping instance by matching @tail@ as a @Cons@.

  fold (There x) _ = fold @_ @result x
  fold (Here  x) f = ignore @tail (f x)

class Ignore (args :: [Type]) result where

  -- | @Ignore@ is a class whose only purpose is to generate n-ary @const@
  -- functions. Give it a result, and it'll figure out the type of a function
  -- that ignores all its arguments.

  ignore :: result -> FoldSignature args result

-- | An empty list of arguments means we're ready to return the result!
instance Ignore '[] result where
  ignore result = result

instance Ignore xs result
    => Ignore (x ': xs) result where

  -- | Provided I can ignore the tail, I can remove the whole thing. How? I
  -- just insert another argument and ignore that as well!

  ignore result _ = ignore @xs result

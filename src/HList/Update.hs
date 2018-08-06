{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module      : HList.Update
Description : Conveniently update a value within an HList.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

Let's imagine an @HList '[2, 2.0, True]@ and I want to run a function on the
boolean value. In order to do this, I need to tell the type-checker the index
to which I want to apply the function so that it can figure out if the types
align.

Note that we _could_ take the function's input type and search for that in the
HList, which would make the index redundant. However, this doesn't help us in
the case of multiple entries for the same type.

Note this is most easily-achieved by turning on 'DataKinds' and
'TypeApplications':

>>> :set -XDataKinds -XScopedTypeVariables -XTypeApplications
>>> :set -fprint-potential-instances
>>> :{
xs :: HList '[Integer, Double, Bool]
xs = HCons 2 (HCons 2.0 (HCons True HNil))
:}

>>> update @2 not xs
HCons 2 (HCons 2.0 (HCons False (HNil)))

>>> update @1 (\_ -> "hello") xs
HCons 2 (HCons "hello" (HCons True (HNil)))

>>> update @4 show xs
...
... • 4 is out of bounds for '[Integer, Double, Bool]!
...   We'll need at least 0, and at most 2.
...

>>> update @1 (++ "!") xs -- Type mismatch!
...
... • You can't apply [Char] -> [Char] to the Double at index #1.
...   I'm sorry, captain; I just won't do it.
...
-}
module HList.Update where

import Data.Kind    (Type)
import HList.Types  (HList (..))
import GHC.TypeLits
import Utils

class Update (i :: Nat) (s :: [Type]) (t :: [Type]) (a :: Type) (b :: Type)
    | i s b -> t, i t a -> s where

  -- | The update class is a convenience wrapper around 'UpdateLoop', which
  -- will provide far better documentation. :)
  --
  -- This class only has one instance, and it's a passthru to UpdateLoop. The
  -- only interesting thing is that it sets @original@ to whatever @s@ is at
  -- the start, meaning we can write some more helpful type errors. 'OneOf''s
  -- 'inject' function is another good example of this.

  update :: (a -> b) -> HList s -> HList t

-- | Updating typeclass (I used the optics convention of @s t a b@ - hopefully
-- that's a nice intuition?).
class UpdateLoop
    (i ::  Nat         ) -- Which index should we update?
    (o :: (Nat, [Type])) -- What was in our /original/ index and list?
    (s ::       [Type])  -- What's left to search?
    (t ::       [Type])  -- What will that search look like once updated?
    (a ::        Type )  -- What's the type of our indexed element?
    (b ::        Type )  -- What type do we want it to be?
    | i s b -> t, i t a -> s where

  -- | We have five type variables going on here:
  --
  -- - @i@: Which index would you like to update?
  -- - @o@: For type errors, how did the list look when we began?
  -- - @s@: What was in the HList before the update?
  -- - @t@: What will be in the HList after the update?
  -- - @a@: What is the type of the thing you want to update?
  -- - @b@: What will be the type of that thing after the update?
  --
  -- Our functional dependencies say that "there is always a unique @t@ for a
  -- given @i@, @s@, and @b@", and that "there is always a unique @s@ for a
  -- given @i@, @t@, and @a@.
  --
  -- It may not be obvious why we can't determine @a@ from @i@ and @s@ (or,
  -- indeed, @b@ from @i@ and @t@). The reason is our custom type error:
  -- because we want to match on the empty list for @s@ and @t@, we're stuck:
  -- there's no unique @a@ and @b@ at that point, because it's whatever you
  -- failed to find in your list.

  update' :: (a -> b) -> HList s -> HList t

instance {-# OVERLAPPING #-} xs ~ ys
    => UpdateLoop 0 o (a ': xs) (b ': ys) a b where

  -- | When we're on index 0, our chosen @a@ should be at the front of the
  -- list.  This means that we can apply the @a -> b@ function, and we have the
  -- result type. We're done!

  update' f (HCons x xs) = HCons (f x) xs

-- | TODO: think about how to solve this problem more carefully. We ideally
-- want some custom type error when the function we've been given doesn't match
-- the types at that index (rather than just "couldn't satisfy constraint").

-- instance {-# OVERLAPPABLE #-} TypeError (Text "Type mismatch!")
--     => UpdateLoop 0 (a ': xs) (b ': xs) c d where
--   update = undefined

instance {-# OVERLAPPABLE #-} (x ~ y, UpdateLoop (n - 1) o xs ys a b)
  => UpdateLoop n o (x ': xs) (y ': ys) a b where

  -- | When we're _not_ on index 0, the types at the head of the input and
  -- output lists should be the same. Thus, we cons the input type onto the
  -- updated output tail. The recursion here looks a bit weird thanks to
  -- TypeLits: we can't apply type families in instance heads, and @+@ is a
  -- type family, which means we can't use @n + 1@ in the head.
  --
  -- We can, however, use @n - 1@ in the constraint. This is how we recurse,
  -- and also why the @UpdateLoop 0@ step was labelled with an OVERLAPPING pragma:
  -- whenever we have @0@ as an index, we should definitely pick that instance!
  -- If we picked this one, we'd never stop recursing.

  update' f (HCons x xs) = HCons x (update' @(n - 1) @o f xs)

-- | Slightly more interesting type error this time round (with a slightly more
-- ugly construction, of course!) to detect whether an index be out-of-bounds
-- for a given list. If this happens, we'll get a friendly error in the GHC
-- output.

type family ShowOverflowError (index :: Nat) (list :: [k]) :: ErrorMessage where
  ShowOverflowError index list
    = ShowType index :<>: Text " is out of bounds for "
        :<>: ShowType list :<>: Text "!"

      :$$: Text "We'll need at least 0, and at most "
        :<>: ShowType (Length list - 1) :<>: Text "."

-- | If we focus on a type that isn't the same as our function input, we should
-- also probably say something helpful. Thus, here we build up a type error by
-- looking up what the expected input was, and printing that alongside the
-- function we were given.

type family ShowIndexTypeError (n :: Nat) (o :: [k]) (a :: Type) :: ErrorMessage where
  ShowIndexTypeError n o f
    = Text "You can't apply " :<>: ShowType f :<>: Text " to the "
        :<>: ShowType (Lookup n o) :<>: Text " at index #"
        :<>: ShowType n :<>: Text "."

      :$$: Text "I'm sorry, captain; I just won't do it."

-- | Here's our actual error instance: we use our 'If' family to determine
-- which of the usual suspects is most likely causing the problem, and return
-- it to the user. The instance is empty as it'll never compile, so we're all
-- good :)

instance TypeError
       ( If (Between 0 (Length list - 1) index)
            (ShowIndexTypeError index list (a -> b))
            (ShowOverflowError index list) )
    => UpdateLoop n '(index, list) '[] '[] a b where
  update' = undefined

instance UpdateLoop i '(i, s) s t a b
    => Update i s t a b where

  -- | Finally, here's our convenience instance for 'Update': we pass the
  -- requested index and the original list in as the "original state" that we
  -- carry in case of errors. Kinda underwhelming.

  update = update' @i @'(i, s)


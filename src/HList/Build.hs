{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module      : HList.Build
Description : Build an HList with a specialised function.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

The construction of an HList can be quite tedious because there's no special
syntax (and we can't use OverloadedLists because it's homogeneous). To make it
a bit neater, this module provides a `build` function, which produces a
parameter for each element of the HList.

>>> :{
f :: HList '[]
f = build
:}

>>> :{
g :: Int -> HList '[Int]
g = build
:}

>>> :{
h :: String -> Int -> HList '[String, Int]
h = build
:}

This pattern generalises to any number of parameters, and inference seems to be
pretty good. The one gotcha is that you'll see plenty of internals if you try
to ask for the type of a call to 'build' without annotating the eventual
@HList@ anywhere.

>>> :t build 2 "hello" True
build 2 "hello" True
  :: (BuildLoop tail (t : [Char] : Bool : tail), Num t) =>
     Signature tail (t : [Char] : Bool : tail)

This is unfortunate, but makes sense: is @build 2@ an @HList '[Int]@, or is it
@String -> HList '[Int, String]@? For any call to build, there are a literally
infinite number of possible interpretations, so it's worth specifying the HList
you actually want to build (as above).
-}
module HList.Build where

import Data.Kind    (Type)
import HList.Types  (HList (..))
import Prelude      hiding (head)

-- | This type family takes a list of types and an outupt type, and rolls it
-- out into a function. Originally, it was called @Uncurry@, and the result
-- wasn't necessarily an HList. However, I ran into (what I think is) a pretty
-- fun problem.
--
-- The type of 'build' was /always/ ambiguous. I needed to type-apply the
-- entire list of types to get it to behave, which is obviously not the nicest
-- developer experience. At the time, the signature of 'build' was @Uncurry
-- list (HList list)@, which would expand depending on list. However, it wasn't
-- injective. In other words, you couldn't figure out the inputs if you knew
-- the outputs. For example:
--
-- @
--    Uncurry '[a, b] c     == a -> b -> c
--    Uncurry '[a] (b -> c) == a -> b -> c
-- @
--
-- What this meant was that, if I annotated a call to 'build' with @Int ->
-- HList '[Int]@, that wasn't enough information to figure out what the inputs
-- would be, so it would complain about ambiguity until I made it explicit.
--
-- Enter @TypeFamilyDependencies@. If our result is always an 'HList', we /can/
-- always know the inputs. In fact, with this extension, we can even make it
-- explicit with something that looks an awful lot like a functional
-- dependency. Now inference works, and I learnt a new thing!
type family Signature (list :: [Type]) (output :: [Type]) = (done :: Type)
    | done -> list output where
  Signature '[]            original = HList original
  Signature (head ': tail) original = head -> Signature tail original

-- | There's an interesting problem to be solved here: our arguments for this
-- function are the reverse to what would be easiest (we can't just @HCons@
-- them on!). A naïve implementation would take each parameter, put it into
-- an @\x -> HCons x HNil@ singleton, and then concat them all together. We
-- can do slightly better, however, with a DList:
--
-- @
--   type DList a = [a] -> [a]
-- @
--
-- It's definitely an unintuitive type, but the summary is: instead of all
-- the appends, we build a function of /prepends/. When we want to append
-- something, we pre-compose it with our current function, and then it goes
-- first. As all the functions are just 'HCons', we get exactly what we want!
-- Even better: it's O(1) to add each element!
class BuildLoop (todo :: [Type]) (original :: [Type]) where

  buildLoop :: (HList todo -> HList original) -> Signature todo original


instance BuildLoop '[] original where

  -- | We have none left "to do", so we can just pass in an empty @HList@ and
  -- set off the composition chain. The result of this will be our list!
  buildLoop :: (HList '[] -> HList original) -> HList original
  buildLoop diff = diff HNil


instance BuildLoop tail original => BuildLoop (head ': tail) original where

  -- | Here, we have a DList that requires a @head@ type from us. So, we ask
  -- for that as a parameter, and then pre-compose a function to our DList that
  -- will 'HCons' it on. This hurts my head if I think about it too much, but
  -- we're really just saying "each step of this recursion should produce a
  -- parameter /before/ the last", rather than after. It's neat.
  buildLoop
    :: (HList (head ': tail) -> HList original)
    -> head
    -> Signature tail original

  buildLoop diff head
    = buildLoop @tail (diff . (HCons head))


-- | This is the class (and function) that users will actually see, and all
-- it does  is pass through to 'buildLoop'. However, it does so with a fully
-- injective type signature (i.e. one whose output is unique for each input),
-- so GHC can work backwards to figure out our parameters without type
-- application. GHC is way cleverer than we admit.
class Build original where

  build :: Signature original original


instance BuildLoop original original => Build original where

  -- | Our instance is straightforward: we can deduce the type of the output
  -- loop based on the call site for this function, but we can't necessarily
  -- deduce what's left "to do". However, we know that the 'build' function is
  -- called when /everything/ is left to do, so we can type-apply @original@ to
  -- the @todo@ list in @BuildLoop@ and everything works!
  build = buildLoop @original id

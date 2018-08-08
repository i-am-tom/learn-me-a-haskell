{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : OneOf
Description : A generalised @Either@ type.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

The notion of a "product type" captures the idea that a type is made up of some
number of other types. The simplest product type is @()@, which holds no types.
Any type @a@ (or @Identity a@) is a product of one type - itself. To hold _two_
types, the simplest structure we have is @Tuple@ (or @(,)@). What do we do if
we want more types? We nest, of course!

@
  type Tuple0         = ()
  type Tuple1 a       = a
  type Tuple2 a b     = (a, b)
  type Tuple3 a b c   = (a, (b, c))
  type Tuple4 a b c d = (a, (b, (c, d)))
@

This is hopefully an obvious progression: if we think of @a@ as being
equivalent to @(a)@, it's a very clear pattern indeed. However, dealing with a
very nested tuple isn't straightforward, and it's certainly ripe for
encapsulation. So, just as we did with the sum type, 'OneOf', we'll generalise
this concept with 'HList'.

An 'HList' is a /heterogeneous/ list. In other words, unlike a normal list, the
types of each element in an HList can be totally different: the restriction
(/homogeneity/) is lifted. This raises a good question, though: if I can have
/any/ type in my HList, how is this type-safe? How does Haskell remember which
types are where? The answer is in the representation.

@
  type Tuple0         = HList '[          ]
  type Tuple1 a       = HList '[a         ]
  type Tuple2 a b     = HList '[a, b      ]
  type Tuple3 a b c   = HList '[a, b, c   ]
  type Tuple4 a b c d = HList '[a, b, c, d]
@

The type of an @HList@ contains the types within it! When we @HCons@ something
onto an HList, we @Cons@ its type into the type-level list. This ensures that
we always know the types!

== Construction

>>> :set -XDataKinds
>>> :{
f :: HList '[String]
f = HCons "Hello!" HNil
:}

>>> :{
g :: HList '[String, Bool]
g = HCons "Hello" (HCons True HNil)
:}

>>> :{
h :: HList '[String, Bool, Int]
h = HCons "Hello" (HCons True (HCons 3 HNil))
:}

== `build`

>>> build :: HList '[]
HNil

>>> build "Hello"        :: HList '[String]
HCons "Hello" (HNil)

>>> build "Hello" True   :: HList '[String, Bool]
HCons "Hello" (HCons True (HNil))

>>> build "Hello" True 2 :: HList '[String, Bool, Int]
HCons "Hello" (HCons True (HCons 2 (HNil)))

== Indexed updates

>>> :set -XTypeApplications
>>> update @1 not h
HCons "Hello" (HCons False (HCons 3 (HNil)))

>>> update @4 (\_ -> "OUT OF BOUNDS") h
...
... • 4 is out of bounds for '[String, Bool, Int]!
...   We'll need at least 0, and at most 2.
...

>>> update @2 (++ "!") h
...
... • You can't apply [Char] -> [Char] to the Int at index #2.
...   I'm sorry, captain; I just won't do it.
...
-}
module HList
  ( HList (..)

  -- * Convenience functions
  --
  -- Similar to the @OneOf@ functions, these are available to make working with
  -- HList less frustrating and boilerplate-riddled.

  , build
  , fold
  , foldMap
  , update
  ) where

import HList.Types  (HList (..))
import HList.Build  (build)
import HList.Fold   (fold, foldMap)
import HList.Update (update)

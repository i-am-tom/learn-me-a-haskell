{-# LANGUAGE DataKinds #-}

{-|
Module      : HTree
Description : A heterogeneous red-black tree.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

An 'HList' is a wonderful general-purpose heterogeneous container, but it has
some performance issues as the list grows in size, largely due to the
typechecker's process: type-checking is not optimised in the same way as
value-level computations are, so compile times can really suffer when every
lookup is linear and without caching.

A nice alternative, I think, is a binary tree: now, lookups are logarithmic,
rather than linear, thanks to generic representations giving us a nice way to
"order" types.

I had a whole host of problems while trying to build this particular library.
First of all, I built an HTree indexed by a simple tree type:

@
  data Tree a = Empty | Node (Tree a) a (Tree a)
@

As I started looking into balancing, my first attempt involved indexing the
tree by the number of nodes within it:

@
  data Tree (count :: Nat) a where
    Empty :: Tree 0 a
    Node  :: Tree m a -> a -> Tree n a -> Tree (1 + m + n) a
@

Then, in the type class instances, we could match on the count and use that to
balance things:

@
  class 'Insert' element (tree :: Tree n a) where
    insert
      :: element
      -> 'HTree' (tree :: Tree n a)
      -> 'HTree' (tree :: Tree (n + 1) a)
@

However, we then run into all sorts of fun issues. Let's say we want to balance
a left-heavy tree. The signature of balance is `HTree count a -> HTree count a`
because balancing shouldn't change the size of the tree, but we're going to pop
a thing from the left and insert it onto the right. Popping reduces the count,
inserting increases it, so GHC will complain: the original tree had size @1 +
left + right@, but this new one has size @1 + (left - 1) + (right + 1)@, and
GHC simply isn't clever enough on its own to normalise this expression.

Enter @ghc-typelits-natnormalise@, a plugin to normalise these expressions at
the type-level. Exit @natnormalise@, as it doesn't seem to work at the kind
level?

The problems continue: because the kind of the tree is now calculated via a
type family, you run into a GHC error I'd never seen, which says you can't use
type families to calculate a kind within a type family! Similar problems arise
when you play with instances of classes, and we get royally stuck.

So, it looks like kind-indexing is a no-go, even /with/ TypeInType (I would be
hugely impressed if someone /could/ fix this!). So, instead of getting too deep
into this, I changed the tree to a red-black tree and removed the kind-level
indexing in favour of a regular type-level index. The result is not necessarily
"correct by construction", but certainly 100% more functioning.

== Constructions

>>> (&) = flip ($)
>>> empty -- We represent this with a dot.
.
>>> import GHC.Generics
>>> :set -XDeriveGeneric
>>> newtype Name = Name String deriving (Generic, Show)
>>> newtype Age  = Age  Int    deriving (Generic, Show)
>>> example = empty & insert (Name "Tom") & insert (Age 25)
>>> :t example
example
  :: HTree
       ('Node 'Black ('Node 'Black 'Empty Age 'Empty) Name 'Empty)
>>> example
((.)<-(Age 25)->(.))<-(Name "Tom")->(.)

>>> :{
f :: (Insert Name i m, Insert Age m o) => HTree i -> HTree o
f = insert (Age 25) . insert (Name "Tom")
:}

== Deletion

>>> example & delete @Name
(.)<-(Age 25)->(.)

>>> example & delete @Name & delete @Age
.

== Access

>>> example & getType @Name
Name "Tom"

>>> example & getType @Bool
...
... I couldn't find any Bool in this tree...
... If it helps, here's what I did find:
... - Age
... - Name
...
-}

module HTree
  ( HTree
  , Tree   (..)
  , Colour (..)

  -- * Manipulation functions
  --
  -- We don't allow access to any of the constructors directly, instead only
  -- allowing manipulation through these functions values. This ensures that
  -- invariants are maintained.

  , empty
  , Insert  (..)
  , Delete  (..)
  , HasType (..)
  ) where

import HTree.Access (HasType (..))
import HTree.Delete (Delete (..))
import HTree.Insert (Insert (..))

import HTree.Types
  ( Colour (..)
  , HTree  (..)
  , Tree   (..)
  )

empty :: HTree 'Empty
empty
  = HEmpty

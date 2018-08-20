{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}

{-|
Module      : HTree.Types
Description : The 'HTree' type and its instances.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

As an 'HList' grows, repeated access can cause compile times to suffer
massively. The reason is that the GHC typechecker does no caching, so every
lookup requires the type-level list to be walked linearly. In a practical
setting, where one may use an HList to store user input, a few hundred items
being accessed throughout the codebase will make compile times insufferably
slow.

We /could/ solve this with a type-checker plugin, but let's try something a bit
more interesting: let's pick a data structure with better access times than a
regular list. Let's build a tree. Specifically, a red-black tree.

>>> :t HEmpty
HEmpty :: HTree 'Empty

>>> :t HNode HEmpty True HEmpty
HNode HEmpty True HEmpty :: HTree ('Node colour 'Empty Bool 'Empty)

>>> :t HNode (HNode HEmpty True HEmpty) 3 HEmpty
HNode (HNode HEmpty True HEmpty) 3 HEmpty
  :: Num centre =>
     HTree
       ('Node colour1 ('Node colour2 'Empty Bool 'Empty) centre 'Empty)

There's also a (pretty messy) 'Show' instance for debugging:

>>> HNode (HNode HEmpty True HEmpty) 3 HEmpty
((.)<-(True)->(.))<-(3)->(.)

Note that we won't expose these constructors, instead requiring that trees be
assembled through the provided methods.
-}
module HTree.Types where

import Data.Kind (Constraint, Type)


-- | Every node within a tree is either red or black. Here's me avoiding the
-- ol' boolean blindness with some domain types.
data Colour = Red | Black


-- | Our 'HTree' will be indexed by /this/ tree type, which holds a colour for
-- each node. These are used for tree-balancing, as per the literature.
data Tree a = Empty | Node Colour (Tree a) a (Tree a)


-- | Finally, the 'HTree' itself. The HTree itself isn't very interesting, as
-- all the type information exists within the index - a red/black tree of
-- types.
data HTree (tree :: Tree Type) where

  -- | An empty tree.
  HEmpty :: HTree 'Empty

  -- | A non-empty tree is two subtrees and a centre element. The colour isn't
  -- enforced in this type, just to make things easier. Invariants are
  -- maintained by the 'insert' and 'delete' functions.
  HNode
    :: HTree left
    -> centre
    -> HTree right
    -> HTree ('Node colour left centre right)


-- | An empty tree that we can expose to our users.


-- | We'll use a dot to represent an empty tree (and thus the leaves at the
-- extremeties of any non-empty tree). Note we can't write one 'Show' instance
-- for all 'HTree's as we want to drill into the type!
instance Show (HTree 'Empty) where
  show _
    = "."

-- | If we can show both sides of a tree, as well as the thing in the middle,
-- we can show a tree! This could be much more intelligent, but it's more than
-- enough for debugging purposes.
instance (Show (HTree left), Show centre, Show (HTree right))
    => Show (HTree ('Node colour left centre right)) where

  show (HNode left centre right)
    = "(" ++ show left ++ ")<-(" ++ show centre ++ ")->(" ++ show right ++ ")"


-- | HEmpty's semigroup instance is exactly the same as ()'s.
instance Semigroup (HTree 'Empty) where
  _ <> _ = HEmpty

-- | Similarly, the monoid empty tree is the empty tree.
instance Monoid (HTree 'Empty) where
  mempty = HEmpty


-- | For more complicated trees, we recurcse down the sides and append as
-- usual. Note that we're requiring the trees to have exactly the same shape!
instance (Semigroup (HTree left), Semigroup centre, Semigroup (HTree right))
    => Semigroup (HTree ('Node colour left centre right)) where

  -- | I think this is really neat...
  HNode left centre right <> HNode left' centre' right'
    = HNode (left <> left') (centre <> centre') (right <> right')


-- | Mempty is just a tree of mempty. As we know our subtrees are Monoid, we
-- don't even have to recurse by hand: GHC will do all the work for us.
instance (Monoid (HTree left), Monoid centre, Monoid (HTree right))
    => Monoid (HTree ('Node colour left centre right)) where

  mempty
    = HNode mempty mempty mempty


-- | All empty trees are equal!
instance Eq (HTree 'Empty) where
  _ == _ = True


-- | Hopefully, the pattern is becoming clear: we use constraints on our
-- subtrees to get GHC to do the recursion, and we write the (very declarative)
-- pattern-matching to define equality: if the left sides are the same, the
-- centres are the same, and the right sides are the same, they're the same.
instance (Eq (HTree left), Eq centre, Eq (HTree right))
    => Eq (HTree ('Node colour left centre right)) where

  HNode left centre right == HNode left' centre' right'
    = left == left' && centre == centre' && right == right'

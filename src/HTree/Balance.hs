{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module      : HTree.Balance
Description : Balance a potentially-unbalanced HTree.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

When we add or remove elements from our HTree, it can become imbalanced. To
correct this, we apply the balancing algorithm from Chris Okasaki's "Red-Black
Trees in a Functional Setting", which is... pretty dense. I'll try to talk
through each step in turn.
-}
module HTree.Balance where

import Data.Kind
import HTree.Types


class Balance
    (input  :: Tree Type)
    (output :: Tree Type)
    | input -> output where

  -- | The functional dependency here says that we can generate a balanced tree
  -- from any input tree. This is as we'd hope, really: it's essentially a
  -- function from potentially-unbalanced trees to balanced trees.
  balance
    :: HTree input
    -> HTree output


-- | Given the "state of imbalance", we can balance a tree. To do that, we use
-- the 'FindStatus' type family below.
instance
    ( status ~ FindStatus input
    , BalanceBranch status input output
    )
    => Balance input output where

  balance
    = balanceBranch @status


-- | There are really only four types of imbalance we care about, all expressed
-- through the red nodes in two levels of the tree. Again, I defer to the paper
-- to explain this more precisely, but the invariant is that we can't have red
-- children of red nodes, so here we identify violations of this rule.
data BalanceStatus
  = RedLeftInRedLeft
  | RedRightInRedLeft
  | RedLeftInRedRight
  | RedRightInRedRight
  | AllClear


-- | Look for violations and return a 'BalanceStatus' to represent the
-- imbalance. Of course, 'AllClear' means that we haven't unbalanced the tree.
type family FindStatus (input :: Tree Type) :: BalanceStatus where

  FindStatus ('Node 'Black ('Node 'Red ('Node 'Red _ _ _) _ _) _ _)
    = RedLeftInRedLeft

  FindStatus ('Node 'Black ('Node 'Red _ _ ('Node 'Red _ _ _)) _ _)
    = RedRightInRedLeft

  FindStatus ('Node 'Black _ _ ('Node 'Red ('Node 'Red _ _ _) _ _))
    = RedLeftInRedRight

  FindStatus ('Node 'Black _ _ ('Node 'Red _ _ ('Node 'Red _ _ _)))
    = RedRightInRedRight

  FindStatus tree
    = AllClear


-- | Given one of the above statuses, we can determine the balanced version of
-- the tree. Note that @status@ is ambiguous, so we'll need to type-apply it.
-- The instances for this class were copied directly from the paper, which,
-- once more, will do a much better job of explaining how the rotation and
-- balancing works, so I encourage you to read that to see what's going on in
-- more detail.
class BalanceBranch
    (status :: BalanceStatus)
    (input  ::     Tree Type)
    (output ::     Tree Type)
    | status input -> output where

  balanceBranch
    :: HTree input
    -> HTree output


-- | A balance tree can be left alone!
instance BalanceBranch 'AllClear tree tree where
  balanceBranch
    = id


instance BalanceBranch 'RedLeftInRedLeft
    ('Node 'Black ('Node 'Red   ('Node 'Red lll llx llr) lx               lr) x r)
    ('Node 'Red   ('Node 'Black             lll llx llr) lx ('Node 'Black lr  x r)) where
  balanceBranch (HNode (HNode (HNode lll llx llr) lx lr) x r)
    = HNode (HNode lll llx llr) lx (HNode lr x r)

instance BalanceBranch 'RedRightInRedLeft
    ('Node 'Black ('Node 'Red ll lx ('Node 'Red lrl lrx lrr)) x r)
    ('Node 'Red ('Node 'Black ll lx lrl) lrx ('Node 'Black lrr x r)) where
  balanceBranch (HNode (HNode ll lx (HNode lrl lrx lrr)) x r)
    = HNode (HNode ll lx lrl) lrx (HNode lrr x r)

instance BalanceBranch 'RedLeftInRedRight
    ('Node 'Black l x ('Node 'Red ('Node 'Red rll rlx rlr) rx rr))
    ('Node 'Red ('Node 'Black l x rll) rlx ('Node 'Black rlr rx rr)) where
  balanceBranch (HNode l x (HNode (HNode rll rlx rlr) rx rr))
    = HNode (HNode l x rll) rlx (HNode rlr rx rr)

instance BalanceBranch 'RedRightInRedRight
    ('Node 'Black l x ('Node 'Red  ll lx ('Node 'Red lrl lrx lrr)))
    ('Node 'Red ('Node 'Black l x ll) lx ('Node 'Black lrl lrx lrr)) where
  balanceBranch (HNode l x (HNode ll lx (HNode lrl lrx lrr)))
    = HNode (HNode l x ll) lx (HNode lrl lrx lrr)

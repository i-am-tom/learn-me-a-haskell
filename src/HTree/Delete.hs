{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module      : HTree.Delete
Description : Remove a value from an HTree.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

Really just the complementing operation to 'insert', we can delete things from
our tree and then rebalance. Nothing too interesting to note here.

>>> import HTree.Types
>>> import HTree.Insert
>>> :t delete @Bool (insert True HEmpty)
delete @Bool (insert True HEmpty) :: HTree 'Empty

-}
module HTree.Delete where

import Data.Kind     (Type)
import GHC.Generics
import HTree.Balance
import HTree.Types
import Utils


class Delete
    (element ::      Type)
    (input   :: Tree Type)
    (output  :: Tree Type)
    | element input -> output where

  -- | Very similar to 'insert', we present 'delete' as our interface to our
  -- users. If we know the element we're deleting, and the tree from which
  -- we're deleting it, we can determine the tree that we'll have afterwards.
  delete
    :: HTree input
    -> HTree output


-- | Deleting an element from an empty tree is a no-op. This actually means
-- that you can delete a type from a tree regardless of whether it's actually
-- there, which is perhaps a good and bad thing...
instance Delete element 'Empty 'Empty where
  delete = id

instance {-# OVERLAPPING #-} Delete element
    ('Node 'Black 'Empty element 'Empty) 'Empty where

  -- | Deleting from the singleton tree is fairly straightforward - if the
  -- types match, we just return an empty tree!
  delete (HNode empty _ _) = empty


-- | Here's our recursive step. Just as with 'insert', the 'Generic' constraint
-- allows us to get the name of the type for comparison. Note that we also have
-- to type-apply the element type here as it is otherwise ambiguous.
instance
    ( order ~ CmpType element centre
    , Generic element
    , DeleteLoop order element ('Node 'Black left centre right) output
    )
    => Delete element ('Node 'Black left centre right) output where

  delete
    = deleteLoop @order @element


-- | The loop is just the same as the API for our users, except we also carry a
-- direction of travel to know which subtree to search for the thing we want to
-- delete.
class DeleteLoop
    (order   ::  Ordering)
    (element ::      Type)
    (input   :: Tree Type)
    (output  :: Tree Type)
    | order element input -> output where

  deleteLoop
    :: HTree input
    -> HTree output


-- | In the event that we want to delete a node with non-empty subtrees, we
-- have to try a bit harder to adjust the tree: namely, we have to take the
-- smallest thing from the right tree, and use that as our new centre. In
-- JavaScript, taking the smallest thing from a list is a "shift" operation,
-- hence the name!
class Shift
    (input    :: Tree Type)
    (smallest ::      Type)
    (output   :: Tree Type)
    | input -> smallest output where

  shift
    :: HTree input
    -> (smallest, HTree output)


-- If the left subtree is empty, the smallest thing in our tree is the centre,
-- and the "left over" tree is the right subtree. This is our "simple case".
instance Shift ('Node colour 'Empty centre right) centre right where

  shift (HNode _ centre right)
    = (centre, right)


-- | If the left subtree is /not/ empty, we have to recurse into it. On the way
-- "back out", we also have to reconstruct the tree without the smallest node.
instance Shift (Node c l x r) smallest left'
    => Shift ('Node colour (Node c l x r)  centre right) smallest
             ('Node colour left'           centre right) where

  shift (HNode left centre right)
    = let (smallest, left') = shift left
      in (smallest, HNode left' centre right)


-- | If our comparison suggests the type will be in the left-hand branch, we
-- recurse down the left side, and rebalance the tree at this level after
-- deletion.
instance
    ( Delete element left left'
    , input  ~ 'Node colour left  centre right
    , middle ~ 'Node colour left' centre right
    , Balance middle output
    )
    => DeleteLoop 'LT element input output where

  deleteLoop (HNode left centre right)
    = balance @middle
    $ HNode (delete @element left) centre right


-- | If the thing we want is in the centre of the tree we're searching, and the
-- right-hand subtree is empty, we can just return the left-hand subtree.
instance DeleteLoop 'EQ target
    ('Node colour ('Node colour' left centre right) target 'Empty)
    ('Node 'Black                left centre right) where

  deleteLoop (HNode (HNode left centre right) _ HEmpty)
    = HNode left centre right


-- | If the thing we want is in the centre of the tree we're searching, but the
-- right-hand subtree is /not/ empty, we need to remove the "smallest" type
-- from the right subtree and use that as the new centre element, and then
-- rebalance. It's a bit of a dance.
instance
    ( Shift ('Node c l x r) smallest right'
    , unbalanced ~ 'Node colour left smallest right'
    , Balance unbalanced output
    )
    => DeleteLoop 'EQ element ('Node colour left centre ('Node c l x r)) output where

  deleteLoop (HNode left centre right)
    = let (smallest, right') = shift right
      in balance @unbalanced (HNode left smallest right')


-- | Finally, if the comparison says we should search the /right/ tree, we
-- recurse down the right side and then rebalance.
instance
    ( Delete element right right'
    , input  ~ 'Node colour left centre right
    , middle ~ 'Node colour left centre right'
    , Balance middle output
    )
    => DeleteLoop 'GT element input output where

  deleteLoop (HNode left centre right)
    = balance @middle
    $ HNode left centre (delete @element right)

{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module      : HTree.Insert
Description : Insert a value into an HTree polymorphically.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

Keeping track of the type of an HTree is difficult as that HTree grows, even if
we're talking only in terms of signature readability. To help with this, we can
offer an 'Insert' typeclass whose parameters can determine the shape of the new
tree. This means we never have to write the explicit types of our trees in our
functions, as we implement them polymorphically:

>>> :{
f :: Insert Bool input output => HTree input -> HTree output
f = insert True
:}

Not only will this now work with /any/ tree, we can totally forget about the
ugly type of the tree we're actually using! Better code reuse, better
readability; what more could you want?
-}
module HTree.Insert
  ( Insert (..)
  ) where

import Data.Kind     (Type)
import GHC.Generics  (Generic)
import HTree.Balance (Balance (..))
import HTree.Types
import Utils         (CmpType)

class Insert
    (element ::      Type)
    (input   :: Tree Type)
    (output  :: Tree Type)
    | element input -> output where

  -- | This is the class we present to our users. Notice the functional
  -- dependency: if we know what @element@ and @input@ are, we can determine
  -- @output@, so we don't have to know what @output@ /should/ be in our
  -- caller.
  insert
    :: element
    -> HTree input
    -> HTree output


instance Insert element 'Empty ('Node 'Black 'Empty element 'Empty) where

  -- | The trivial case for insertion is when we're inserting into an empty
  -- tree. In this case, we just build a single-node tree with two empty
  -- subtrees, and shortcut everything below this point.
  insert element empty
    = HNode empty element empty


-- | In order to find the point at which we should insert the type, we do a
-- comparison on the two types to get an "order". The trick here is that all
-- types must have a 'Generic' instance, which we can use to get the name of
-- the type in its representation, which we can use to "sort" the types.
--
-- Once we know where to insert the type, we can recurse.
instance
    ( order ~ CmpType element centre
    , Generic element
    , InsertLoop order element ('Node colour left centre right) output
    )
    => Insert element ('Node colour left centre right) output where

  -- | We need to type-apply 'order' as it isn't a determined argument and is
  -- therefore ambiguous.
  insert
    = insertLoop @order


-- | The actual loop is the same as our user API, but now we're considering the
-- comparison of our type to the current "focus" node. Note we also require
-- @order@ in this functional dependency now!
class InsertLoop
    (order   ::  Ordering)
    (element ::      Type)
    (input   :: Tree Type)
    (output  :: Tree Type)
    | order element input -> output where

  insertLoop
    :: element
    -> HTree input
    -> HTree output


-- | If our type to insert is "less than" the current focus type's, we'll
-- insert it into the left subtree. There's nothing too exciting here, except
-- perhaps the 'balance' logic, in which we apply the standard red-black
-- corrections to "balance" the tree.
instance
    ( Insert element left left'
    , input  ~ 'Node colour left  centre right
    , middle ~ 'Node colour left' centre right
    , Balance middle output
    )
    => InsertLoop 'LT element input output where

  insertLoop element (HNode left centre right)
    = balance @middle
    $ HNode (insert element left) centre right


-- | Exactly as with the last instance, we insert into the /right/ subtree when
-- our type to insert is "greater than" the focus type.
instance
    ( Insert element right right'
    , input  ~ 'Node colour left centre right
    , middle ~ 'Node colour left centre right'
    , Balance middle output
    )
    => InsertLoop 'GT element input output where

  insertLoop element (HNode left centre right)
    = balance @middle
    $ HNode left centre (insert element right)


-- | If our node is "equal" to the focus type, we can safely assume that
-- they're the same type! This instance requires that they /actually/ be the
-- same type, as well as the same rep names, which is a potential future
-- confusion that should be addressed... If I have two types of the same name
-- from different modules, this will potentially lead to some confusion.
instance
    ( input  ~ 'Node colour left element right
    , output ~ input
    )
    => InsertLoop 'EQ element input output where
  insertLoop element (HNode left _ right)
    = HNode left element right

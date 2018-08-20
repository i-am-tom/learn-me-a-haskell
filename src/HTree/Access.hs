{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module      : HTree.Access
Description : Get a value from an HTree by its type.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

With any non-trivial 'HTree', the type is going to become pretty unwieldy.
Rather than requiring our users to write the full type every time someone wants
to access a value from it, the 'HasType' constraint can be used to get a value
out polymorphically.

>>> :set -XDeriveGeneric
>>> import GHC.Generics
>>> import HTree.Insert
>>> newtype Name = Name String deriving (Generic, Show)
>>> newtype Age  = Age  Int    deriving (Generic, Show)

>>> getType @Name (insert (Name "Tom") HEmpty)
Name "Tom"

>>> getType @Age (insert (Age 25) (insert (Name "Tom") HEmpty))
Age 25

>>> getType @Name HEmpty
...
... You won't find any Name here!
... Your tree is empty; there's nothing to access!
...

>>> getType @Bool (insert (Age 25) . insert (Name "Tom") $ HEmpty)
...
... I couldn't find any Bool in this tree...
... If it helps, here's what I did find:
... - Age
... - Name
...

-}
module HTree.Access
  ( HasType (..)
  ) where


import Data.Kind    (Type)
import GHC.TypeLits (ErrorMessage (..), TypeError)
import HTree.Types  (Colour (..), HTree (..), Tree (..))
import Utils        (CmpType, TypeName)


-- | If we search a tree down to a leaf and haven't found what we want, it
-- would be nice to show a proper type error. This family just builds us a nice
-- type error to show. Specifically, it lists the fields it has and the one it
-- couldn't find!
type family TypeNotFound
    (element ::      Type)
    (tree    :: Tree Type) :: ErrorMessage where

  TypeNotFound element tree
        = 'Text "I couldn't find any "
            ':<>: ShowType element
            ':<>: 'Text " in this tree..."

    ':$$: 'Text "If it helps, here's what I did find:"
    ':$$: PrintTree tree


-- | In order to print the error above, we need to list all the items in the
-- tree. Luckily, this is pretty straightforward: we print leaves by printing
-- only the item in the middle, and everything else is done by printing
-- subtrees first.
type family PrintTree (tree :: Tree Type) :: ErrorMessage where
  PrintTree ('Node colour 'Empty centre 'Empty)
    = 'Text "- " ':<>: 'Text (TypeName centre)

  PrintTree ('Node colour left centre 'Empty)
    = PrintTree left ':$$: PrintTree ('Node colour 'Empty centre 'Empty)

  PrintTree ('Node colour 'Empty centre right)
    = PrintTree ('Node colour 'Empty centre 'Empty) ':$$: PrintTree right

  PrintTree ('Node colour left centre right)
    = PrintTree left
        ':$$: PrintTree ('Node colour 'Empty centre 'Empty)
        ':$$: PrintTree right


-- | When we want to interrogate a structure, we need to supply the type of the
-- element we're trying to get from the structure. This is probably most easily
-- done with a type application, such as `getType @TypeToFind`.
class HasType (element :: Type) (tree :: Tree Type) where

  -- | Get a value of a given type from the HList.
  getType
    :: HTree tree
    -> element


-- | If we try to access an element in an empty tree, we're going to have a bad
-- time. Rather than giving the "No instance found" error, we can actaully
-- write something that will be helpful with debugging.
instance TypeError
    ( 'Text "You won't find any " ':<>: ShowType element ':<>: 'Text " here!"
        ':$$: Text "Your tree is empty; there's nothing to access!"
    )
    => HasType element 'Empty where

  -- | No need to define a method that A) can't be defined, and B) can't be
  -- used.
  getType
    = undefined


-- | We separate 'HasType' and 'HasTypeLoop' for the sake of inference -
-- 'HasType' should only be used on top-level trees, whose root node is always
-- black. By splitting this class, we can assert that invariant in 'HasType'
-- and use 'HasTypeLoop' recursively.
--
-- As with similar classes, we'll also carry the "original" tree with us in
-- order to write some more useful error messages in the failure case.
class HasTypeLoop
    (element  ::      Type)
    (tree     :: Tree Type)
    (original :: Tree Type) where

  -- | Recursively try to find a type.
  getTypeLoop
    :: HTree tree
    -> element


-- | We can write a 'HasType' instance for anything that implements the
-- 'HasTypeLoop' for its parent node. This is our call to the "go" function.
instance
    ( original ~ 'Node 'Black left centre right
    , HasTypeLoop element original original
    )
    => HasType element ('Node 'Black left centre right) where

  -- | As usual, we must type-apply @original@ to pass it down to 'getTypeLoop'
  -- is it isn't a value we can determine from @element@ and @tree@.
  getType
    = getTypeLoop @_ @_ @original


-- | The core idea behind our lookup is that we must pick which side of the
-- tree to search based on the type we're looking for. Here, we use the
-- 'CmpType' family to give us an ordering on types, and therefore determine
-- which side of the tree to check.
instance
    ( order ~ CmpType element centre
    , HasTypeBranch order element ('Node colour left centre right) original
    )
    => HasTypeLoop element ('Node colour left centre right) original where

  -- | This time, both @order@ /and/ @original@ are undetermined and ambiguous,
  -- so we must type-apply both of them to keep GHC happy.
  getTypeLoop
    = getTypeBranch @order @_ @_ @original


-- | 'HasTypeBranch' looks exactly like 'HasTypeLoop' and 'HasType', except
-- it's carrying the result of comparing our desired type with the type at the
-- top of the tree we're searching.
class HasTypeBranch
    (order    ::  Ordering)
    (element  ::      Type)
    (tree     :: Tree Type)
    (original :: Tree Type) where

  getTypeBranch :: HTree tree -> element


-- | If our left subtree is empty, but our comparison says our key should be in
-- there, it's safe to say we've been unable to find the type, and we can show
-- our type error from above.
instance {-# OVERLAPPING #-} TypeError (TypeNotFound element original)
  => HasTypeBranch 'LT element ('Node colour 'Empty centre right) original where

  getTypeBranch
    = undefined


-- | If our type is "less than" the current focus, we'll check the left subtree
-- recursively and propagate the result.
instance HasTypeLoop element left original
    => HasTypeBranch 'LT element ('Node colour left centre right) original where

  -- | Moving back to 'HasTypeLoop', we need to re-apply the 'original'
  -- variable. Ambiguous variables are a headache.
  getTypeBranch (HNode left _ _)
    = getTypeLoop @_ @_ @original left


-- | If our two types are /equal/, we've found it! All we do now is return the
-- value at this point, and we're done. We don't need to check the subtrees,
-- and we no longer care about what the tree originally was because we won't
-- need a type error :)
instance HasTypeBranch 'EQ element ('Node 'Black left element right) original where

  getTypeBranch (HNode _ centre _)
    = centre


-- | If our type is "greater than" the current focus, we look down the right
-- subtree. It's really just the standard binary tree lookup, but promoted to
-- the type-level.
instance HasTypeLoop element right original
    => HasTypeBranch 'GT element ('Node colour left centre right) original where

  -- | Again, more type application as we "move" to 'HasTypeLoop'.
  getTypeBranch (HNode _ _ right)
    = getTypeLoop @_ @_ @original right


-- | If we're told to look into the right subtree, and that subtree is empty,
-- we've gone wrong. For the user's sake, we just show our type error from
-- earlier.
instance {-# OVERLAPPING #-} TypeError (TypeNotFound element original)
  => HasTypeBranch 'GT element ('Node colour left centre 'Empty) original where

  getTypeBranch
    = undefined

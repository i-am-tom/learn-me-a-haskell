{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Utils
  ( Between
  , CmpType
  , Every
  , If
  , Length
  , Lookup
  , TypeName
  ) where

import Data.Kind    (Constraint, Type)
import GHC.Generics (D1, Meta(..), Rep)
import GHC.TypeLits


-- | Produce a constraint that is built from a type-level list of things
-- applied to a constraint constructor.
--
-- >>> :kind! Every '[Int, Bool] Show
-- Every '[Int, Bool] Show :: Constraint
-- = (Show Int, (Show Bool, () :: Constraint))
--
-- Note that nested constraints are automatically flattened, so this is totally
-- equivalent to @(Show Int, Show Bool)@.
type family Every (xs :: [k]) (c :: k -> Constraint) :: Constraint where
  Every '[]       c = ()
  Every (x ': xs) c = (c x, Every xs c)


-- | Depending on the value of a predicate, evaluate to the @true@ or @false@
-- value provided. Note that this is entirely eager, so both options will be
-- evaluated in full before returning a result. I really only use this for type
-- errors, where we're already sure we're not actually going to continue
-- compiling.
--
-- >>> :kind! If 'True "Hello" "Goodbye"
-- If 'True "Hello" "Goodbye" :: Symbol
-- = "Hello"
--
-- >>> :kind! If 'False "Hello" "Goodbye"
-- If 'False "Hello" "Goodbye" :: Symbol
-- = "Goodbye"
type family If (predicate :: Bool) (true :: k) (false :: k) where
  If 'True  true _     = true
  If 'False _    false = false


-- | Get the length of a type-level list.
--
-- >>> :kind! Length '[]
-- Length '[] :: Nat
-- = 0
--
-- >>> :kind! Length '["T", "o", "m"]
-- Length '["T", "o", "m"] :: Nat
-- = 3
type family Length (list :: [k]) :: Nat where
  Length '[]       = 0
  Length (x ': xs) = 1 + Length xs


-- | Append two type-level lists.
--
-- >>> :set -XTypeOperators
-- >>> :kind! '[] ++ '[]
-- '[] ++ '[] :: [k]
-- = '[]
--
-- >>> :kind! '[1, 2] ++ '[3, 4]
-- '[1, 2] ++ '[3, 4] :: [Nat]
-- = '[1, 2, 3, 4]
type family (xs :: [k]) ++ (ys :: [k]) where
  '[]       ++ ys =             ys
  (x ': xs) ++ ys = x ': (xs ++ ys)


-- | Lookup an item in a type-level list by index. Note that an out-of-bounds
-- index just won't reduce, which is possibly not the best user experience, but
-- certainly good enough for our needs.
--
-- >>> :kind! Lookup 2 '[1, 2, 3]
-- Lookup 2 '[1, 2, 3] :: Nat
-- = 3
--
-- >>> :kind! Lookup 4 '[1, 2, 3]
-- Lookup 4 '[1, 2, 3] :: Nat
-- = Lookup 1 '[]
type family Lookup (n :: Nat) (list :: [k]) :: k where
  Lookup 0 (x ': xs) = x
  Lookup n (x ': xs) = Lookup (n - 1) xs


-- | Are two things equal? Note that this is homogeneous: the items must be of
-- the same kind.
--
-- >>> :kind! 2 == 2
-- 2 == 2 :: Bool
-- = 'True
--
-- >>> :kind! 2 == 3
-- 2 == 3 :: Bool
-- = 'False
type family (x :: k) == (y :: k) :: Bool where
  x == x = 'True
  _ == _ = 'False


-- | Type-level boolean conjunction. Just like regular (&&), but promoted.
--
-- >>> :kind! 'False && 'False
-- 'False && 'False :: Bool
-- = 'False
--
-- >>> :kind! 'True && 'True
-- 'True && 'True :: Bool
-- = 'True
type family (x :: Bool) && (y :: Bool) :: Bool where
  'True  && y = y
  'False && y = 'False


-- | Return the bigger of two type-level naturals.
--
-- >>> :kind! Max 1 3
-- Max 1 3 :: Nat
-- = 3
type Max (left :: Nat) (right :: Nat)
  = If (CmpNat left right == 'GT) left right


-- | Return the smaller of two type-level naturals.
--
-- >>> :kind! Min 1 3
-- Min 1 3 :: Nat
-- = 1
type Min (left :: Nat) (right :: Nat)
  = If (CmpNat left right == 'LT) left right


-- | Check whether the third value is at least the first and at most the
-- second.
--
-- >>> :kind! Between 0 10 5
-- Between 0 10 5 :: Bool
-- = 'True
--
-- >>> :kind! Between 0 1 5
-- Between 0 1 5 :: Bool
-- = 'False
type Between (lower :: Nat) (upper :: Nat) (value :: Nat)
   = (Max upper value == upper)
  && (Min lower value == lower)


-- | Get the name of a type from its generic representation. Note that this is
-- partial, but private.
type family RepName (x :: Type -> Type) :: Symbol where
  RepName (D1 ('MetaData name _ _ _) _) = name


-- | Get the name of any type with a generic representation as a type-level
-- symbol.
--
-- >>> :kind! TypeName Bool
-- TypeName Bool :: Symbol
-- = "Bool"
--
-- >>> :set -XDeriveGeneric
-- >>> import GHC.Generics
-- >>> newtype Name = Name String deriving Generic
-- >>> :kind! TypeName Name
-- TypeName Name :: Symbol
-- = "Name"
type family TypeName (x :: Type) :: Symbol where
  TypeName x = RepName (Rep x)


-- | Compare two types using their names as symbols. It's not perfect, but it's
-- deterministic, which is all we need for sorting types.
--
-- >>> import GHC.Generics
-- >>> newtype Name = Name String deriving Generic
-- >>> newtype Age  = Age  Int    deriving Generic
--
-- >>> :kind! CmpType Age Name
-- CmpType Age Name :: Ordering
-- = 'LT
--
-- >>> :kind! CmpType Name Age
-- CmpType Name Age :: Ordering
-- = 'GT
--
-- >>> :kind! CmpType Age Age
-- CmpType Age Age :: Ordering
-- = 'EQ
type CmpType this that
  = CmpSymbol (TypeName this) (TypeName that)


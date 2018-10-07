{-|
Module      : Utils.Tuple.TH
Description : @TemplateHaskell@-generated instances for the `Morph` typeclass.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

Rather than writing out many - /many/ - instances for conversions between
tuples and @HList@s, we can use a mechanism I've been very much trying to
avoid: @TemplateHaskell@.

Unlike @GHC.Generics@, the @TemplateHaskell@ library gives us a way to build
n-ary tuples recursively. In other words, '(,)' and '(,,)' are, in the eyes of
@GHC.Generics@, totally unrelated - the same is not true in @TemplateHaskell@.
-}
module Utils.Tuple.TH where

import Data.Foldable       (foldl')
import Data.Function       ((&))
import Language.Haskell.TH


-- 1. Types

-- | We generate a tuple 'Type' from a list of names using a left fold. At the
-- 'Type' level, tuples are made by 'AppT' applications to some fixed-length
-- 'TupleT'.
mkTupleT :: [Name] -> Type
mkTupleT xs = foldl' (\head -> AppT head . VarT) (TupleT (length xs)) xs

-- | We generate an 'HList' 'Type' from the names by repeated application, in
-- nearly exactly the same way as we generated the tuple.
mkHListT :: [Name] -> Type
mkHListT = foldr (AppT . AppT PromotedConsT . VarT) PromotedNilT


-- 2. Patterns

-- | The presentation of tuples (frustratingly) changes at the value level.
-- Instead of repeated application to some constructor, we now pass a list of
-- its inhabitants to the 'TupP' constructor, which works for n-ary tuples.
mkTupleP :: [Name] -> Pat
mkTupleP = TupP . fmap VarP

-- | An HList is represented with a two-item constructor, 'HCons', or a nullary
-- constructor, 'HNil'.
mkHListP :: [Name] -> Pat
mkHListP (x : xs) = ConP (mkName "HCons") [VarP x, mkHListP xs]
mkHListP []       = ConP (mkName  "HNil") []


-- 3. Expressions

-- | We construct tuple expressions just as we construct tuple patterns: turn
-- the names into variables, and wrap them in a 'TupE' constructor.
mkTupleE :: [Name] -> Exp
mkTupleE = TupE . fmap VarE

-- | Exactly the same as 'mkHListT', but with 'Expression'-level constructors,
-- rather than type-level 'Cons'.
mkHListE :: [Name] -> Exp
mkHListE = foldr (AppE . AppE cons . VarE) nil
  where
    nil  = ConE (mkName  "HNil")
    cons = ConE (mkName "HCons")


-- 4. All together now!

-- | We build the morph instance out of the pieces that we have collected.
makeMorphInstance :: Int -> Q Dec
makeMorphInstance count = do
  -- | Names to use for each type/value parameter.
  names <- traverse (\_ -> newName "t") [1 .. count]

      -- HList pieces...
  let hlistT = mkHListT names
      hlistP = mkHListP names
      hlistE = mkHListE names

      -- Tuple pieces...
      tupleT = mkTupleT names
      tupleP = mkTupleP names
      tupleE = mkTupleE names

      -- The instance head.
      morph = ConT (mkName "Morph")
      head  = AppT (AppT morph hlistT) tupleT

  -- The entire instance.
  pure $ InstanceD Nothing [] head
    [ FunD (mkName "fromTuple") [ Clause [ tupleP ] (NormalB hlistE) [] ]
    , FunD (mkName "toTuple")   [ Clause [ hlistP ] (NormalB tupleE) [] ]
    ]

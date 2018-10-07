{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TemplateHaskell        #-}

{-|
Module      : Utils.Tuple
Description : Tuple-to-'HList' conversion.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

Now we have the 'HList' type, we can use it as a more flexible representation
of a @Tuple@. However, it is clear to us that tuple syntax is still much, much
nicer to use than the 'HList' syntax, and the 'OverloadedLists' extension is
not flexible enough for us to exploit it.

This module allows us to convert between (flat) tuples and their corresponding
'HList' types, the goal of which being that we can present tuple-based
interfaces to our users while maintaining 'HList' flexibilities internally.
-}
module Utils.Tuple where

import HList               (HList (..))
import Language.Haskell.TH
import Utils.Tuple.TH


-- | As we only generate instances for 'HList' types of at least length @2@,
-- the functional dependency works in both ways. As a singleton tuple is just
-- the element itself, we would unfortunately not be able to maintain the
-- second functional dependency - in GHC's eyes, there's no way to tell the
-- difference between (a) and ((a)), and thus whether @types@ is '[a] or
-- '[(a)] or '[((a))] or... well, you get the picture.
class Morph types tuple | types -> tuple, tuple -> types where
  fromTuple :: tuple -> HList types
  toTuple   :: HList types -> tuple


-- | I will admit that this is my first time using @TemplateHaskell@, and as
-- such I have probably demonstrated exactly how not to optimise compile time.
-- This splice independently generates sixty-odd 'Morph' instances: one for
-- each possible tuple.
$(traverse makeMorphInstance [2 .. 62])

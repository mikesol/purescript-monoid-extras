{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Split
-- Copyright   :  (c) 2011-2015 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Sometimes we want to accumulate values from some monoid, but have
-- the ability to introduce a \"split\" which separates values on
-- either side.  Only the rightmost split is kept.  For example,
--
-- > a b c | d e | f g h == a b c d e | f g h
--
-- In the diagrams graphics framework this is used when accumulating
-- transformations to be applied to primitive diagrams: the 'freeze'
-- operation introduces a split, since only transformations occurring
-- outside the freeze should be applied to attributes.
--
-----------------------------------------------------------------------------

module Data.Monoid.Split
       ( Split(..)
       , split
       , unsplit

       ) where

-- import Data.Data

import Prelude

import Data.Foldable (class Foldable, foldMapDefaultL)
import Data.Generic.Rep (class Generic)
import Data.Monoid.Action (class Action, act)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequenceDefault)

-- | A value of type @Split m@ is either a single @m@, or a pair of
--   @m@'s separated by a divider.  Single @m@'s combine as usual;
--   single @m@'s combine with split values by combining with the
--   value on the appropriate side; when two split values meet only
--   the rightmost split is kept, with both the values from the left
--   split combining with the left-hand value of the right split.
--
--   "Data.Monoid.Cut" is similar, but uses a different scheme for
--   composition.  @Split@ uses the asymmetric constructor @:|@, and
--   @Cut@ the symmetric constructor @:||:@, to emphasize the inherent
--   asymmetry of @Split@ and symmetry of @Cut@.  @Split@ keeps only
--   the rightmost split and combines everything on the left; @Cut@
--   keeps the outermost splits and throws away everything in between.
data Split m = M m
             | Split m m
derive instance Eq m => Eq (Split m)
derive instance Functor Split
derive instance Generic (Split m) _
instance Show m => Show (Split m) where
  show = genericShow
instance Foldable Split where
  foldr f b (M a) = f a b
  foldr f b (Split a a') = f a' (f a b)
  foldl f b (M a) = f b a
  foldl f b (Split a a') = f (f b a) a'
  foldMap = foldMapDefaultL
instance Traversable Split where
  traverse f (M a) = M <$> (f a)
  traverse f (Split a a') = Split <$> (f a) <*> (f a')
  sequence = sequenceDefault

infix 5 Split as :|


-- | If @m@ is a @Semigroup@, then @Split m@ is a semigroup which
--   combines values on either side of a split, keeping only the
--   rightmost split.
instance Semigroup m => Semigroup (Split m) where
  append (M m1)          (M m2)       = M (m1 <> m2)
  append (M m1)          (m1' :| m2)  = (m1 <> m1')         :| m2
  append (m1  :| m2)     (M m2')      = m1                :| (m2 <> m2')
  append (m11 :| m12)    (m21 :| m22) = (m11 <> m12 <> m21 ):| m22


instance (Semigroup m, Monoid m) => Monoid (Split m) where
  mempty  = M mempty

-- | A convenient name for @mempty :| mempty@, so @M a \<\> split \<\>
--   M b == a :| b@.
split :: forall m. Monoid m => Split m
split = mempty :| mempty

-- | \"Unsplit\" a split monoid value, combining the two values into
--   one (or returning the single value if there is no split).
unsplit :: forall m. Semigroup m => Split m -> m
unsplit (M m)      = m
unsplit (m1 :| m2) = m1 <> m2

-- | By default, the action of a split monoid is the same as for
--   the underlying monoid, as if the split were removed.
instance Action m n => Action (Split m) n where
  act (M m) n      = act m n
  act (m1 :| m2) n = act m1 (act m2 n)

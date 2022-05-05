{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-imports       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Inf
-- Copyright   :  (c) 2012-2015 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Make semigroups under 'min' or 'max' into monoids by adjoining an
-- element corresponding to infinity (positive or negative,
-- respectively). These types are similar to @Maybe (Min a)@ and
-- @Maybe (Max a)@ respectively, except that the 'Ord' instance
-- matches the 'Monoid' instance.
--
-----------------------------------------------------------------------------

module Data.Monoid.Inf
       ( Inf(..)
       , Pos, Neg
       , PosInf, NegInf
       , minimum, maximum
       -- * Type-restricted constructors
       , posInfty, negInfty
       , posFinite, negFinite
       ) where

-- import           Data.Data

import Prelude

import Data.Foldable (class Foldable, foldMapDefaultL)
import Data.Foldable as F
import Data.Maybe (fromMaybe)
import Data.Traversable (class Traversable, sequenceDefault)

-- | Type index indicating positive infinity.
data Pos
-- | Type index indicating negative infinity.
data Neg

-- | @Inf p a@ represents the type 'a' extended with a new "infinite"
--   value, which is treated as either positive or negative infinity
--   depending on the type index 'p'.  This type exists mostly for its
--   'Ord', 'Semigroup', and 'Monoid' instances.
data Inf :: forall k. k -> Type -> Type
data Inf p a = Infinity | Finite a
--  deriving (Data, Typeable, Show, Read, Eq, Functor, Foldable,
--            Traversable)

derive instance Eq a => Eq (Inf p a)
derive instance Functor (Inf p)
instance Foldable (Inf p) where
  foldr f b (Finite a) = f a b
  foldr _ b Infinity = b
  foldl f b (Finite a) = f b a
  foldl _ b Infinity = b
  foldMap = foldMapDefaultL
instance Traversable (Inf p) where
  traverse f (Finite a) = Finite <$> (f a)
  traverse _ Infinity = pure Infinity
  sequence = sequenceDefault

-- | The type 'a' extended with positive infinity.
type PosInf a = Inf Pos a

-- | The type 'a' extended with negative infinity.
type NegInf a = Inf Neg a

-- | Positive infinity is greater than any finite value.
instance Ord a => Ord (Inf Pos a) where
  compare Infinity Infinity = EQ
  compare Infinity (Finite _) = GT
  compare (Finite _) Infinity = LT
  compare (Finite a) (Finite b) = compare a b

-- | Negative infinity is less than any finite value.
instance Ord a => Ord (Inf Neg a) where
  compare Infinity Infinity = EQ
  compare Infinity (Finite _) = LT
  compare (Finite _) Infinity = GT
  compare (Finite a) (Finite b) = compare a b

-- | An ordered type extended with positive infinity is a semigroup
--   under 'min'.
instance Ord a => Semigroup (Inf Pos a) where
  append = min

-- | An ordered type extended with negative infinity is a semigroup
--   under 'max'.
instance Ord a => Semigroup (Inf Neg a) where
  append = max

-- | An ordered type extended with positive infinity is a monoid under
--   'min', with positive infinity as the identity element.
instance Ord a => Monoid (Inf Pos a) where
  mempty = Infinity
  -- mappend = (<>)

-- | An ordered type extended with negative infinity is a monoid under
--   'max', with negative infinity as the identity element.
instance Ord a => Monoid (Inf Neg a) where
  mempty = Infinity
  -- mappend = (<>)

instance Applicative (Inf p) where
    pure = Finite
instance Apply (Inf p) where
    apply Infinity _ = Infinity
    apply _ Infinity = Infinity
    apply (Finite f) (Finite x) = Finite $ f x

instance Bind (Inf p) where
    bind Infinity _ = Infinity
    bind (Finite x) f = f x

instance Monad (Inf p)

instance Bounded a => Bounded (NegInf a) where
    bottom = Infinity
    top = Finite top

instance Bounded a => Bounded (PosInf a) where
    bottom = Finite bottom
    top = Infinity

-- | Find the minimum of a list of values.  Returns positive infinity
--   iff the list is empty.
minimum :: forall a. Ord a => Array a -> PosInf a
minimum xs = fromMaybe Infinity (F.minimum (map Finite xs))

-- | Find the maximum of a list of values.  Returns negative infinity
--   iff the list is empty.
maximum :: forall a. Ord a => Array a -> NegInf a
maximum xs = fromMaybe Infinity (F.maximum (map Finite xs))

-- | Positive infinity.
posInfty :: forall a. PosInf a
posInfty = Infinity

-- | Negative infinity.
negInfty :: forall a. NegInf a
negInfty = Infinity

-- | Embed a finite value into the space of such values extended with
--   positive infinity.
posFinite :: forall a. a -> PosInf a
posFinite = Finite

-- | Embed a finite value into the space of such values extended with
--   negative infinity.
negFinite :: forall a. a -> NegInf a
negFinite = Finite

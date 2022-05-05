{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Recommend
-- Copyright   :  (c) 2012-2015 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A type for representing values with an additional bit saying
-- whether the value is \"just a recommendation\" (to be used only if
-- nothing better comes along) or a \"commitment\" (to certainly be
-- used, overriding merely recommended values), along with
-- corresponding @Semigroup@ and @Monoid@ instances.
--
-----------------------------------------------------------------------------

module Data.Monoid.Recommend
       ( Recommend(..)
       , getRecommend
       ) where

-- #if __GLASGOW_HASKELL__ < 710
import Data.Traversable (class Foldable, class Traversable, foldMapDefaultL, sequenceDefault)
-- #endif

-- import           Data.Data
import           Prelude
-- | A value of type @Recommend a@ consists of a value of type @a@
--   wrapped up in one of two constructors.  The @Recommend@
--   constructor indicates a \"non-committal recommendation\"---that
--   is, the given value should be used if no other/better values are
--   available.  The @Commit@ constructor indicates a
--   \"commitment\"---a value which should definitely be used,
--   overriding any @Recommend@ed values.
data Recommend a = Recommend a
                 | Commit a
--  deriving (Show, Read, Functor, Eq, Ord, Typeable, Data, Foldable, Traversable)

derive instance Eq a => Eq (Recommend a)
derive instance Ord a => Ord (Recommend a)
derive instance Functor Recommend
instance Foldable Recommend where
  foldr f b (Recommend a) = f a b
  foldr f b (Commit a) = f a b
  foldl f b (Recommend a) = f b a
  foldl f b (Commit a) = f b a
  foldMap = foldMapDefaultL
instance Traversable Recommend where
  traverse f (Recommend a) = Recommend <$> (f a)
  traverse f (Commit a) = Commit <$> (f a)
  sequence = sequenceDefault

-- | Extract the value of type @a@ wrapped in @Recommend a@.
getRecommend :: forall a. Recommend a -> a
getRecommend (Recommend a) = a
getRecommend (Commit a)    = a

-- | 'Commit' overrides 'Recommend'. Two values wrapped in the same
--   constructor (both 'Recommend' or both 'Commit') are combined
--   according to the underlying @Semigroup@ instance.
instance Semigroup a => Semigroup (Recommend a) where
  append (Recommend a) (Recommend b) = Recommend (a <> b)
  append (Recommend _) (Commit b)    = Commit b
  append (Commit a)    (Recommend _) = Commit a
  append (Commit a)    (Commit b)    = Commit (a <> b)

  -- stimes n (Recommend m) = Recommend (stimes n m)
  -- stimes n (Commit    m) = Commit    (stimes n m)

instance (Semigroup a, Monoid a) => Monoid (Recommend a) where
  -- mappend = (<>)
  mempty  = Recommend mempty

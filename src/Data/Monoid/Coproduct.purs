{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Coproduct.Strict
-- Copyright   :  (c) 2015 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A strict coproduct of two monoids.
--
-----------------------------------------------------------------------------

module Data.Monoid.Coproduct
  (
    -- * Coproduct
    type (:+:)
  , inL, inR
  , prependL, prependR
  , killL, killR
  , untangle
  , MonoidalCoproduct(..)
  -- ** Lenses
  , untangled
  , _L
  , _R

  ) where

import Prelude

import Data.Lens as L
import Data.Maybe (Maybe(..))
import Data.Monoid.Action (class Action, act)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))

-- | @m :+: n@ is the coproduct of monoids @m@ and @n@. Concatentation
--   is equivilent to
--
-- @
-- (m1 :+: n1) <> (m2 :+: n2) = (m1 <> m2) :+: (n1 <> act m1 n2)@
-- @
--
--   but has a more efficient internal implimentation.
data MonoidalCoproduct m n = C (Maybe n) (Maybe m) (Maybe n)
infix 5 type MonoidalCoproduct as :+:
-- The left n already has the action m applied. The right n still needs
-- m applied, but it kept there incase more n comes to reduce the number
-- of actions that need to be applied.

-- instance (Action m n, Monoid m, Monoid n, Show m, Show n) => Show (m :+: n) where
--   showsPrec p c = showParen (p > 5) $
--     showsPrec 11 m . showString " :+: " . showsPrec 11 n
--     where (m /\ n) = untangle c

instance (Action m n, Semigroup m, Semigroup n) => Semigroup (m :+: n) where
  append (C n1 m1 o1) (C n2 m2 o2) = C (n1 <> act' m1 (o1 <> n2)) (m1 <> m2) o2
  {-# INLINE (<>) #-}

instance (Action m n, Semigroup m, Semigroup n) => Monoid (m :+: n) where
  mempty  = C Nothing Nothing Nothing
  {-# INLINE mempty #-}
  -- mappend = (<>)
  {-# INLINE mappend #-}

-- | Coproducts act on other things by having each of the components
--   act individually.
instance (Action m n, Action m r, Action n r, Semigroup n) => Action (m :+: n) r where
  act (C n m o) = act'' n' <<< act'' m
    where n' = n <> act' m o
  {-# INLINE act #-}

-- | Construct a coproduct with a left value.
inL :: forall m n. m -> m :+: n
inL m = C Nothing (Just m) Nothing
{-# INLINE inL #-}

-- | Construct a coproduct with a right value.
inR :: forall m n. n -> m :+: n
inR r = C (Just r) Nothing Nothing
{-# INLINE inR #-}

-- | Prepend a value from the left.
prependL :: forall m n. Semigroup m => m -> m :+: n -> m :+: n
prependL m' (C n m o) = C n (Just m' <> m) o
{-# INLINE prependL #-}

-- | Prepend a value from the right.
prependR :: forall m n. Semigroup n => n -> m :+: n -> m :+: n
prependR n' (C n m o) = C (Just n' <> n) m o
{-# INLINE prependR #-}

-- | Extract @m@ from a coproduct.
killR :: forall m n. Monoid m => m :+: n -> m
killR (C _ m _) = get m
{-# INLINE killR #-}

-- | Extract @n@ from a coproduct.
killL :: forall m n. Action m n => Monoid n => m :+: n -> n
killL (C n m o) = get $ n <> act' m o
{-# INLINE killL #-}

untangle :: forall m n. Action m n => Monoid m => Monoid n => m :+: n -> (m /\ n)
untangle (C n m o) = (get m /\ get n')
  where n' = n <> act' m o
{-# INLINE untangle #-}

-- Lenses --------------------------------------------------------------

-- | Lens onto the both @m@ and @n@.
untangled :: forall m n m' n'. Action m n => Monoid m => Monoid n => L.Lens (m :+: n) (m' :+: n') (m /\ n) (m' /\ n')
untangled = L.lens untangle (\(C _ _ _) (m /\ n) -> C (Just n) (Just m) Nothing)
{-# INLINE untangled #-}
-- this could be an iso if we depended on profunctors

-- | Lens onto the left value of a coproduct.
_L :: forall m n m'. Action m n => Monoid m => Monoid n => Semigroup n => L.Lens (m :+: n) (m' :+: n) m m'
_L = L.lens (untangle >>> fst) (\(C n _ _) m -> C n (Just m) Nothing)
{-# INLINE _L #-}
-- this could be a prism if we depended on profunctors

-- | Lens onto the right value of a coproduct.
_R :: forall m n n'. Action m n => Monoid m => Monoid n => L.Lens (m :+: n) (m :+: n') n n'
_R = L.lens (untangle >>> snd) (\(C _ m _) n -> C (Just n) m Nothing)
{-# INLINE _R #-}

-- Internal utilities --------------------------------------------------

get :: forall a. Monoid a => Maybe a -> a
get (Just a) = a
get _        = mempty
{-# INLINE get #-}

-- Act on a Maybe with a Maybe
act' :: forall m n. Action m n => Maybe m -> Maybe n -> Maybe n
act' (Just m) (Just n) = Just (act m n)
act' _        n        = n
{-# INLINE act' #-}

-- Act with a Maybe
act'' :: forall m n. Action m n => Maybe m -> n -> n
act'' (Just m) = act m
act'' _        = identity
{-# INLINE act'' #-}

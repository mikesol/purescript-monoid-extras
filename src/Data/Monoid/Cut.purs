{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Cut
-- Copyright   :  (c) 2012-2015 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- The @Cut@ monoid transformer introduces \"cut points\" such that
-- all values between any two cut points are thrown away.  That is,
--
-- > a b c | d e | f g h i | j k  ==  a b c | j k
--
-----------------------------------------------------------------------------

module Data.Monoid.Cut
       ( Cut(..), cut

       ) where

-- import Data.Data

import Prelude

import Data.Traversable (class Foldable, class Traversable, foldMapDefaultL, sequenceDefault)

-- infix 5 :||:

-- | A value of type @Cut m@ is either a single @m@, or a pair of
--   @m@'s separated by a divider.  The divider represents a \"cut
--   point\".
--
--   @Cut@ is similar to "Data.Monoid.Split", but split keeps only the
--   rightmost divider and accumulates all values, whereas cut always
--   keeps the leftmost and rightmost divider, coalescing them into
--   one and throwing away all the information in between.
--
--   @Split@ uses the asymmetric constructor @:|@, and @Cut@ the
--   symmetric constructor @:||:@, to emphasize the inherent asymmetry
--   of @Split@ and symmetry of @Cut@.  @Split@ keeps only the
--   rightmost split and combines everything on the left; @Cut@ keeps
--   the outermost splits and throws away everything in between.
data Cut m = Uncut m
           | Cut m  m
--  deriving (Data, Typeable, Show, Read, Functor, Foldable, Traversable)

derive instance Functor Cut
instance Foldable Cut where
  foldr f b (Uncut a) = f a b
  foldr f b (Cut a a') = f a' (f a b)
  foldl f b (Uncut a) = f b a
  foldl f b (Cut a a') = f (f b a) a'
  foldMap = foldMapDefaultL
instance Traversable Cut where
  traverse f (Uncut a) = Uncut <$> (f a)
  traverse f (Cut a a') = Cut <$> (f a) <*> (f a')
  sequence = sequenceDefault

infixl 5 Cut as :||:

-- | If @m@ is a @Semigroup@, then @Cut m@ is a semigroup which
--   contains @m@ as a sub-semigroup, but also contains elements of
--   the form @m1 :||: m2@.  When elements of @m@ combine with such
--   \"cut\" elements they are combined with the value on the
--   corresponding side of the cut (/e.g./ @(Uncut m1) \<\> (m1' :||:
--   m2) = (m1 \<\> m1') :||: m2@).  When two \"cut\" elements meet, the
--   two inside values are thrown away and only the outside values are
--   kept.
instance Semigroup m => Semigroup (Cut m) where
  append (Uncut m1)    (Uncut m2)    = Uncut (m1 <> m2)
  append (Uncut m1)    (m1' :||: m2) = (m1 <> m1') :||: m2
  append (m1  :||: m2)  (Uncut m2')   = m1        :||: (m2 <> m2')
  append (m11 :||: _)   (_ :||: m22)  = m11       :||: m22

  -- stimes n (Uncut m) = Uncut (stimes n m)
  -- stimes _ (m      ) = m

instance (Semigroup m, Monoid m) => Monoid (Cut m) where
  mempty  = Uncut mempty
  -- mappend = (<>)

-- | A convenient name for @mempty :||: mempty@, so composing with
-- @cut@ introduces a cut point.  For example, @Uncut a \<\> cut \<\>
-- Uncut b == a :||: b@.
cut :: forall m. Monoid m => Cut m
cut = mempty :||: mempty

-- Note that it is impossible for a cut monoid to have an action in
-- general -- the composition operation can throw away information so
-- it is impossible to satisfy the law (act (m1 <> m2) x = act m1 (act
-- m2 x)) in general (although it may be possible for specific types
-- x).

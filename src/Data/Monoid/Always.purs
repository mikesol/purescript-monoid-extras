{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Always
--
-- Always a monoid, or never not a monoid!
--
-----------------------------------------------------------------------------

module Data.Monoid.Always
       ( class Always, always
       ) where

import Prelude


------------------------------------------------------------
--  Always a monoid
------------------------------------------------------------

-- | Always is guaranteed to produce a value and _may_ produce the input if it is equal to the type of the output.
class Monoid b <= Always a b where
  always :: a -> b

instance Monoid a => Always a a where
  always = identity
else instance Monoid b => Always a b where
  always = const mempty
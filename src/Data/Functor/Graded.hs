{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- |
-- Module       : Data.Functor.Graded
-- Copyright    : (c) 2021 Emily Pillmore
-- License      : BSD-style
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains definitions for grade semigroups, monoids,
-- and groups.
--
-- A graded algebra is an algebra that can be decomposed into a
-- direct sum \( S = ⨁ᵢ Sᵢ \) where each \( Sᵢ \) is an example of
-- the algebra itself.
--
-- This structure presents two opportunities for algebraic manipulation:
--
--   1. If the indexing set is a semigroup (resp. monoid, group etc.)
--      with respect to some operation, then the set can be appended
--      indexing-set wise (e.g. map unions over @k@ - keys implicitly
--      form a list, list appends using integer sums).
--   2. Using the direct sum structure of the semigroup to append at the
--      indices (e.g. @unionWith@ operations).
--
-- In addition, every graded algebra should be able to take a value
-- and produce its corresponding degree, or fibre of the map `S -> I`,
-- which is its index. In Haskell, since indexing sets are discrete,
-- this amounts to locating the discrete index for a particular value.
-- Canonically, we choose the first we see.
{-# LANGUAGE FlexibleInstances #-}
module Data.Functor.Graded
( -- * Graded semigroups
  GradedSemigroup(..)
  -- * Graded monoids
, GradedMonoid(..)
  -- * Graded groups
, GradedGroup(..)
) where


import Data.Functor.WithIndex
import Data.Group
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq)
import Data.Foldable.WithIndex
import Data.Monoid
import Data.IntMap
import qualified Data.IntMap as IM

-- -------------------------------------------------------------------- --
-- Graded semigroups

-- | A graded semigroup is a semigroup that can be decomposed into a
-- direct sum \( S = ⨁ᵢ Sᵢ \) where each \( Sᵢ \) is a semigroup itself.
--
class GradedSemigroup i f where
  -- | Append an element of a semegroup to the value
  -- at the provided index.
  --
  iupdate :: Semigroup g => i -> g -> f g -> f g
  default iupdate
    :: (Eq i, FunctorWithIndex i f, Semigroup g) => i -> g -> f g -> f g
  iupdate i h = imap go where
    go j g
      | i == j = g <> h
      | otherwise = g
  {-# inline iupdate #-}

  -- | Combine two graded semigroups, appending at common
  -- degrees in the sum
  --
  iappend :: Semigroup g => f g -> f g -> f g

  -- | Given an element of a semigroup, produce its corresponding
  -- degree in the graded semigroup.
  --
  -- /Note:/ In general, this is an \( O(n) \) operation.
  --
  degree :: Eq g => g -> f g -> Maybe i
  default degree
    :: (Eq g, FoldableWithIndex i f) => g -> f g -> Maybe i
  degree g = getFirst . ifoldMap go where
    go i h
      | g == h = First (Just i)
      | otherwise = mempty
  {-# inline degree #-}

instance Ord k => GradedSemigroup k (Map k) where
  iappend = M.unionWith (<>)
  iupdate = M.insertWith (<>)
  {-# inline iappend #-}

instance GradedSemigroup Int IntMap where
  iappend = IM.unionWith (<>)
  iupdate = IM.insertWith (<>)
  {-# inline iappend #-}

instance GradedSemigroup Int [] where
  iappend = go
    where
      go as [] = as
      go [] bs = bs
      go (a:as) (b:bs) = (a <> b):go as bs

instance GradedSemigroup Int Seq where
  iappend = undefined

-- -------------------------------------------------------------------- --
-- Graded monoids

-- | A graded monoid is a monoid that can be decomposed into a
-- direct sum \( M = ⨁ᵢ Mᵢ \) where each \( Mᵢ \) is a monoid itself.
--
class GradedSemigroup i f => GradedMonoid i f where
  -- | Insert a unital monoid value at the supplied index.
  --
  imempty :: Monoid g => i -> f g -> f g
  default imempty :: (Monoid g, Eq i) => i -> f g -> f g
  imempty _ = id
  {-# inline imempty #-}

instance GradedMonoid Int []
instance GradedMonoid Int Seq

instance Ord k => GradedMonoid k (Map k)

-- -------------------------------------------------------------------- --
-- Graded monoids

-- | A graded group is a group that can be decomposed into a
-- direct sum \( G = ⨁ᵢ Gᵢ \) where each \( Gᵢ \) is a group itself.
--
class GradedMonoid i f => GradedGroup i f where
  -- | Invert a group value at the supplied index
  --
  iinvert :: Group g => i -> f g -> f g
  default iinvert
    :: (Eq i, FunctorWithIndex i f, Group g) => i -> f g -> f g
  iinvert i = imap go where
    go j g
      | i == j = invert g
      | otherwise = g
  {-# inline iinvert #-}

instance GradedGroup Int []
instance GradedGroup Int Seq

instance Ord k => GradedGroup k (Map k) where
  iinvert = M.adjust invert
  {-# inline iinvert #-}

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

class GradedSemigroup i f where
  iupdate :: Semigroup g => i -> g -> f g -> f g
  default iupdate
    :: (Eq i, FunctorWithIndex i f, Semigroup g) => i -> g -> f g -> f g
  iupdate i h = imap go where
    go j g
      | i == j = g <> h
      | otherwise = g
  {-# inline iupdate #-}

  iappend :: Semigroup g => f g -> f g -> f g

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

class GradedSemigroup i f => GradedMonoid i f where
  imempty :: Monoid g => i -> f g -> f g
  default imempty :: (Monoid g, Eq i) => i -> f g -> f g
  imempty _ = id
  {-# inline imempty #-}

instance GradedMonoid Int []
instance GradedMonoid Int Seq

instance Ord k => GradedMonoid k (Map k)

-- -------------------------------------------------------------------- --
-- Graded monoids

class GradedMonoid i f => GradedGroup i f where
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

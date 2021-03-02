{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module       : Data.Functor.Graded
-- Copyright    : (c) 2021 Emily Pillmore
-- License      : BSD-style
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains definitions for grade semigroups, monoids,
-- and semigroups.
--
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

-- -------------------------------------------------------------------- --
-- Graded semigroups

class GradedSemigroup i f where
  iappend :: Semigroup g => i -> g -> f g -> f g
  default iappend :: (Eq i, FunctorWithIndex i f, Semigroup g) => i -> g -> f g -> f g
  iappend i h = imap go where
    go j g
      | i == j = g <> h
      | otherwise = g
  {-# inline iappend #-}

instance Ord k => GradedSemigroup k (Map k) where
  iappend = M.insertWith (<>)
  {-# inline iappend #-}

instance GradedSemigroup Int []
instance GradedSemigroup Int Seq

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
  default iinvert :: (Eq i, FunctorWithIndex i f, Group g) => i -> f g -> f g
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

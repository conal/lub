-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.Lazier
-- Copyright   :  (c) Conal Elliott 2010
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Some lazier operations.
-- See <http://conal.net/blog/posts/lazier-functional-programming-part-2/>
----------------------------------------------------------------------

module Data.Lazier (eitherL,condL) where

import Data.Lub
import Data.Glb

condL :: (HasLub a, HasGlb a) =>
         a -> a -> Bool -> a
condL a b = const (a `glb` b) `lub` (\ c -> if c then a else b)

eitherL :: (HasLub c, HasGlb c) =>
           (a -> c) -> (b -> c) -> (Either a b -> c)
eitherL f g = const (f undefined `glb` g undefined) `lub` either f g

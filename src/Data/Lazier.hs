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

{- -- Examples:

-- It takes care to check that some of these examples are computed
-- correctly, since printing stops at the first error.  For instance, ask
-- for t5!!1 .

q :: Int
q = undefined `unamb` 5

t0 :: Int
t0 = condL 3 4 True

t1,t2 :: Int
t1 = condL 6 8 undefined  -- _|_
t2 = condL 7 7 undefined  -- 7

t3,t4 :: (Int,Int)
t3 = condL (3,4) (4,5) undefined  -- (_|_,_|_)
t4 = condL (3,4) (3,5) undefined  -- (3,_|_)

t5 :: [Int]
t5 = condL [2,3,5] [1,3] undefined  -- _|_:3:_|_


fe1 :: Either Float Bool -> (Int,String)
fe1 = eitherL (\ x -> (3,"beetle " ++ show x)) (\ b -> (3,"battle " ++ show b))

s1 :: (Int,String)
s1 = fe1 undefined
-- (3,"b*** Exception: glb: bottom (flat & unequal)

s2 :: String
s2 = (tail.tail.tail.snd) (fe1 undefined)
-- "tle *** Exception: Prelude.undefined

s3 :: (Int,String)
s3 = fe1 (Left pi)

-}

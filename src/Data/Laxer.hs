-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.Laxer
-- Copyright   :  (c) Conal Elliott 2010
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Some laxer operations.
-- See <http://conal.net/blog/posts/lazier-functional-programming-part-2/>
----------------------------------------------------------------------

module Data.Laxer (eitherL,condL,foldrL,maybeL,fairZipWith, fairZip) where

import Data.Lub
import Data.Glb

-- | Laxer if-then-else, due to Luke Palmer.
--
-- @
-- condL a a undefined = a
-- condL (Left 3) (Left undefined) undefined = Left undefined
-- @
condL :: (HasLub a, HasGlb a) =>
         a -> a -> Bool -> a
condL a b = const (a `glb` b) `lub` (\ c -> if c then a else b)

-- | Laxer variant of 'either'
eitherL :: (HasLub c, HasGlb c) =>
           (a -> c) -> (b -> c) -> (Either a b -> c)
eitherL f g = const (f undefined `glb` g undefined) `lub` either f g

-- | Laxer variant of 'maybe'
maybeL :: (HasLub b, HasGlb b) =>
           b -> (a -> b) -> (Maybe a -> b)
maybeL n j = const (n `glb` j undefined) `lub` maybe n j

-- | Laxer variant of 'foldr' for lists
foldrL :: (HasLub b, HasGlb b) => (a -> b -> b) -> b -> [a] -> b
foldrL c n = const fallback `lub` go
  where
    fallback = n `glb` c undefined undefined
    go [] = n
    go (x : xs) = c x $ fallback `lub` go xs

-- | A version of 'zipWith' that succeeds if either list
-- ends at the same time the other one bottoms out.
--
-- Laws:
--
-- > fairZipWith >= zipWith
-- > flip . fairZipWith = fairZipWith . flip
fairZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
fairZipWith f as bs =
  zipWith3
    (const f)
    (lub (zipWith discard as bs) (zipWith discard bs as))
    as
    bs
  where
    discard _ _ = ()

-- | @fairZip = fairZipWith (,)@
fairZip :: [a] -> [b] -> [(a, b)]
fairZip = fairZipWith (,)

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

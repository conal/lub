-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.Glb
-- Copyright   :  (c) Conal Elliott 2010
-- License     :  GPL-3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Greatest lower bound
----------------------------------------------------------------------

module Data.Glb (HasGlb(..),glbBottom,flatGlb) where

import Control.Applicative (liftA2)

import Data.Repr

-- | Types that support information intersection ('glb')
class HasGlb a where
  -- | Greatest lower information bound.  Intersects information available
  -- from each argument.
  glb  :: a -> a -> a
  -- | n-ary 'lub'.  Defaults to @foldr lub undefined@
  glbs :: [a] -> a
  glbs = foldr glb undefined

-- | Bottom for a 'glb'.  In the form of @error \"glb: bottom (\<reason\>)\"@,
-- though not really an error.
glbBottom :: String -> a
glbBottom msg = error $ "glb: bottom (" ++ msg ++ ")"

-- | 'glb' on flat types with equality.  Gives bottom for unequal
-- arguments.
flatGlb :: Eq a => a -> a -> a
flatGlb a b | a == b    = a
            | otherwise = glbBottom "flat & unequal"

-- Flat types:
instance HasGlb ()      where glb = flatGlb
instance HasGlb Bool    where glb = flatGlb
instance HasGlb Char    where glb = flatGlb
instance HasGlb Int     where glb = flatGlb
instance HasGlb Integer where glb = flatGlb
instance HasGlb Float   where glb = flatGlb
instance HasGlb Double  where glb = flatGlb
-- ...

instance (HasGlb a, HasGlb b) => HasGlb (a,b) where
  (a,b) `glb` (a',b') = (a `glb` a', b `glb` b')

instance HasGlb b => HasGlb (a -> b) where
  glb = liftA2 glb

instance (HasGlb a, HasGlb b) => HasGlb (Either a b) where
  Left  a `glb` Left  a' = Left  (a `glb` a')
  Right b `glb` Right b' = Right (b `glb` b')
  _ `glb` _ = glbBottom "glb: bottom (Left/Right mismatch)"


-- 'glb' on representations
repGlb :: (HasRepr a v, HasGlb v) => a -> a -> a
repGlb = onRepr2 glb

-- instance (HasRepr t v, HasGlb v) => HasGlb t where
--   glb = repGlb

-- For instance,
instance HasGlb a => HasGlb (Maybe a) where glb = repGlb
instance HasGlb a => HasGlb [a]       where glb = repGlb

{- -- Examples

-- It takes care to check that some of these examples are computed
-- correctly, since printing stops at the first error.  For instance, ask
-- for t5!!1 .

t1,t2 :: Int
t1 = 6 `glb` 8  -- _|_
t2 = 7 `glb` 7  -- 7

t3,t4 :: (Int,Int)
t3 = (3,4) `glb` (4,5)  -- (_|_,_|_)
t4 = (3,4) `glb` (3,5)  -- (3,_|_)

t5 :: [Int]
t5 = [2,3,5] `glb` [1,3]  -- _|_:3:_|_

-}

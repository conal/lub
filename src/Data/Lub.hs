{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.Lub
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Compute least upper bound ('lub') of two values, with respect to
-- information content.  I.e., merge the information available in each.
-- For flat types (in which all values are either bottom or fully
-- defined), 'lub' is equivalent to 'unamb'.
----------------------------------------------------------------------

module Data.Lub
  ( 
  -- * Least upper bounds
    HasLub(..), bottom, flatLub
  -- * Some useful special applications of 'lub'
  , parCommute, pand, por, ptimes
  ) where

import Control.Applicative (liftA2)

import Data.Unamb hiding (parCommute)
import qualified Data.Unamb as Unamb

import Data.Repr

-- | Types that support information merging ('lub')
class HasLub a where
  -- | Least upper information bound.  Combines information available from
  -- each argument.  The arguments must be consistent, i.e., must have a
  -- common upper bound.
  lub :: a -> a -> a

instance HasLub ()   where _ `lub` _ = ()

-- | A 'lub' for flat domains.  Equivalent to 'unamb'.  Handy for defining
-- 'HasLub' instances, e.g.,
-- 
-- @
--   instance HasLub Integer where lub = flatLub
-- @
flatLub :: a -> a -> a
flatLub = unamb

-- Flat types:
instance HasLub Bool    where lub = flatLub
instance HasLub Char    where lub = flatLub
instance HasLub Int     where lub = flatLub
instance HasLub Integer where lub = flatLub
instance HasLub Float   where lub = flatLub
instance HasLub Double  where lub = flatLub
-- ...


-- Lub on pairs
-- pairLub :: (HasLub a, HasLub b) =>
--            (a,b) -> (a,b) -> (a,b)

-- Too strict.  Bottom if one pair is bottom
-- 
-- (a,b) `pairLub` (a',b') = (a `lub` a', b `lub` b')

-- Too lazy.  Non-bottom even if both pairs are bottom
-- 
-- ~(a,b) `pairLub` ~(a',b') = (a `lub` a', b `lub` b')

-- Probably correct, but more clever than necessary, and less efficient.
-- 
--   p `pairLub` p' = assuming (isP p `por` isP p')
--                      (a `lub` a', b `lub` b')
--     where
--       ~(a ,b ) = p
--       ~(a',b') = p'
--   
--   isP :: (a,b) -> Bool
--   isP (_,_) = True


instance (HasLub a, HasLub b) => HasLub (a,b) where
  p `lub` p' = (p `unamb` p') `seq`
                   (a `lub` a', b `lub` b')
    where
      ~(a ,b ) = p
      ~(a',b') = p'

instance HasLub b => HasLub (a -> b) where
  lub = liftA2 lub


instance (HasLub a, HasLub b) => HasLub (Either a b) where
  u `lub` v = if isL u `unamb` isL v then
                Left  (outL u `lub` outL v)
              else
                Right (outR u `lub` outR v)

isL :: Either a b -> Bool
isL = either (const True) (const False)

outL :: Either a b -> a
outL = either id (error "outL on Right")

outR :: Either a b -> b
outR = either (error "outR on Left") id

-- Generic case
--   instance (HasRepr t v, HasLub v) => HasLub t where lub = repLub

-- For instance,
instance HasLub a => HasLub (Maybe a) where lub = repLub
instance HasLub a => HasLub [a]       where lub = repLub


-- 'lub' on representations
repLub :: (HasRepr a v, HasLub v) => a -> a -> a
repLub = onRepr2 lub


{-

-- Examples:

(bottom,False) `lub` (True,bottom)

(bottom,(bottom,False)) `lub` ((),(bottom,bottom)) `lub` (bottom,(True,bottom))

Left () `lub` bottom :: Either () Bool

[1,bottom,2] `lub` [bottom,3,2]

-}

-- | Turn a binary commutative operation into that tries both orders in
-- parallel, 'lub'-merging the results.  Useful when there are special
-- cases that don't require evaluating both arguments.
-- 
-- Similar to 'Unamb.parCommute', but uses 'lub' instead of 'unamb'.
parCommute :: HasLub b => (a -> a -> b) -> (a -> a -> b)
parCommute op x y = (x `op` y) `lub` (y `op` x)

-- | Multiplication optimized for either argument being zero or one, where
-- the other might be expensive/delayed.
ptimes :: (HasLub a, Num a) => a -> a -> a
ptimes = parCommute times
 where
   0 `times` _ = 0
   1 `times` b = b
   a `times` b = a*b

-- I don't think this pplus is useful, since both arguments have to get
-- evaluated anyway.
-- 
-- -- | Addition optimized for either argument being zero, where the other
-- -- might be expensive/delayed.
-- pplus :: (HasLub a, Num a) => a -> a -> a
-- pplus = parCommute plus
--  where
--    0 `plus` b = b
--    a `plus` b = a+b


{-

-- Examples:

0     *    bottom :: Integer
0 `ptimes` bottom :: Integer

bottom `ptimes` 0 :: Integer

-}

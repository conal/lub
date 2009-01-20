{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
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
    HasLub(..), flatLub
  -- * Some useful special applications of 'lub'
  , parCommute, ptimes
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
  lub  :: a -> a -> a
  -- | n-ary 'lub'.  Defaults to @foldr lub undefined@
  lubs :: [a] -> a
  lubs = foldr lub undefined

-- The following instance is wrong, since it lubs two undefineds to ().
-- 
-- instance HasLub () where _ `lub` _ = ()

-- | A 'lub' for flat domains.  Equivalent to 'unamb'.  Handy for defining
-- 'HasLub' instances, e.g.,
-- 
-- @
--   instance HasLub Integer where lub = flatLub
-- @
flatLub :: a -> a -> a
flatLub = unamb

-- Flat types:
instance HasLub ()      where lub = flatLub
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

-- instance (HasLub a, HasLub b) => HasLub (a,b) where
--   (a,b) `lub` (a',b') = (a `lub` a', b `lub` b')

-- Too lazy.  Non-bottom even if both pairs are bottom

-- instance (HasLub a, HasLub b) => HasLub (a,b) where
--   ~(a,b) `lub` ~(a',b') = (a `lub` a', b `lub` b')


instance (HasLub a, HasLub b) => HasLub (a,b) where
  ~p@(a,b) `lub` ~p'@(a',b') =
     (definedP p `unamb` definedP p') `seq` (a `lub` a', b `lub` b')

definedP :: (a,b) -> Bool
definedP (_,_) = True

instance HasLub b => HasLub (a -> b) where
  lub = liftA2 lub

-- f `lub` g = \ a -> f a `lub` g a

instance (HasLub a, HasLub b) => HasLub (Either a b) where
  s `lub` s' = if isL s `unamb` isL s' then
                 Left  (outL s `lub` outL s')
               else
                 Right (outR s `lub` outR s')

isL :: Either a b -> Bool
isL = either (const True) (const False)

outL :: Either a b -> a
outL = either id (error "outL on Right")

outR :: Either a b -> b
outR = either (error "outR on Left") id

-- Generic case
--   instance (HasRepr t v, HasLub v) => HasLub t where lub = repLub

-- 'lub' on representations
repLub :: (HasRepr a v, HasLub v) => a -> a -> a
repLub = onRepr2 lub

-- instance (HasRepr t v, HasLub v) => HasLub t where
--   lub = repLub

-- For instance,
instance HasLub a => HasLub (Maybe a) where lub = repLub
instance HasLub a => HasLub [a]       where lub = repLub



-- a `repLub` a' = unrepr (repr a `lub` repr a')



{-  -- Examples:

(undefined,False) `lub` (True,undefined)

(undefined,(undefined,False)) `lub` ((),(undefined,undefined)) `lub` (undefined,(True,undefined))

Left () `lub` undefined :: Either () Bool

[1,undefined,2] `lub` [undefined,3,2]

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


{-  -- Examples:

0     *    undefined :: Integer
0 `ptimes` undefined :: Integer
undefined `ptimes` 0 :: Integer

-}

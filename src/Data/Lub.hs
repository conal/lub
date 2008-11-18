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
-- Compute least upper bounds (lub / join) of two values
----------------------------------------------------------------------

module Data.Lub
  ( HasLub, bottom
  -- * Some useful special applications of 'lub'
  , parCommute, por, pand, ptimes
  ) where

import Data.Unamb

import Data.AssocView

class HasLub a where lub :: a -> a -> a

instance HasLub ()   where _ `lub` _ = ()

-- | A 'lub' for flat domains.  Equivalent to 'unamb'.
lub0 :: a -> a -> a
lub0 = unamb

-- Flat types:
instance HasLub Bool    where lub = lub0
instance HasLub Char    where lub = lub0
instance HasLub Int     where lub = lub0
instance HasLub Integer where lub = lub0
instance HasLub Float   where lub = lub0
instance HasLub Double  where lub = lub0
-- ...

-- Lub on pairs
pairLub :: (HasLub a, HasLub b) =>
           (a,b) -> (a,b) -> (a,b)

-- p `pairLub` p' = (a `lub` a', b `lub` b')
--   where
--     ~(a ,b ) = p
--     ~(a',b') = p'

-- ~(a,b) `pairLub` ~(a',b') = (a `lub` a', b `lub` b')

-- Too lazy.  Makes bottom be more defined.  Can it be fixed?

p `pairLub` p' = (p `unamb` p') `seq`
                 (a `lub` a', b `lub` b')
  where
    ~(a ,b ) = p
    ~(a',b') = p'

-- p `pairLub` p' = assuming (isP p `por` isP p')
--                    (a `lub` a', b `lub` b')
--   where
--     ~(a ,b ) = p
--     ~(a',b') = p'

-- isP :: (a,b) -> Bool
-- isP (_,_) = True


instance (HasLub a, HasLub b) => HasLub (a,b) where
  lub = pairLub

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
--   instance (HasView t, HasLub (View t)) => HasLub t where lub = vlub

-- For instance,
instance HasLub a => HasLub (Maybe a) where lub = vlub
instance HasLub a => HasLub [a]       where lub = vlub


-- 'lub' on views
vlub :: (HasView a, HasLub (View a)) => a -> a -> a
vlub = onView2 lub


{-

-- Examples:

(bottom,False) `lub` (True,bottom)

(bottom,(bottom,False)) `lub` ((),(bottom,bottom)) `lub` (bottom,(True,bottom))

Left () `lub` bottom :: Either () Bool

[1,bottom,2] `lub` [bottom,3,2]

-}


{--------------------------------------------------------------------
    Some useful special applications of 'unamb'
--------------------------------------------------------------------}

-- | Turn a binary commutative operation into that tries both orders in
-- parallel, 'lub'-merging the results.
parCommute :: HasLub a => (a -> a -> a) -> (a -> a -> a)
parCommute op a b = (a `op` b) `lub` (b `op` a)

-- | Parallel or
por :: Bool -> Bool -> Bool
por = parCommute (||)

-- | Parallel and
pand :: Bool -> Bool -> Bool
pand = parCommute (&&)

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

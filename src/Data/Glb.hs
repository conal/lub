{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE PolyKinds #-}
#endif

{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.Glb
-- Copyright   :  (c) Conal Elliott 2010
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Greatest lower bound
----------------------------------------------------------------------

module Data.Glb (HasGlb(..),glbBottom,flatGlb
  , GHasGlb
  , genericGlb
  ) where

import Control.Applicative (liftA2, Const, ZipList)

import GHC.Generics
import qualified Data.Typeable as Typeable
#if MIN_VERSION_base(4,7,0)
import Data.Type.Equality ((:~:))
import qualified Data.Proxy as Proxy
#endif
#if MIN_VERSION_base(4,8,0)
import qualified Data.Functor.Identity as Identity
import qualified Data.Void as Void
#endif
#if MIN_VERSION_base(4,9,0)
import qualified Data.Functor.Compose as Compose
import qualified Data.Functor.Product as Product
import qualified Data.Functor.Sum as Sum
#endif
#if MIN_VERSION_base(4,10,0)
import Data.Type.Equality ((:~~:))
import qualified Type.Reflection as TR
#endif


-- | Types that support information intersection ('glb')
class HasGlb a where
  -- | Greatest lower information bound.  Intersects information available
  -- from each argument.
  glb  :: a -> a -> a
  default glb :: (Generic a, GHasGlb (Rep a)) => a -> a -> a
  glb = genericGlb

  -- | n-ary 'glb' for n > 0.  Defaults to @foldr1 glb@.  Unlike @lub@, we
  -- have no unit for 'glb'.
  glbs1 :: [a] -> a
  glbs1 = foldr1 glb

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
instance HasGlb Char    where glb = flatGlb
instance HasGlb Int     where glb = flatGlb
instance HasGlb Integer where glb = flatGlb
instance HasGlb Float   where glb = flatGlb
instance HasGlb Double  where glb = flatGlb
instance HasGlb Typeable.TypeRep where glb = flatGlb
#if MIN_VERSION_base(4,7,0)
instance HasGlb (a :~: b) where glb = flatGlb
#endif
#if MIN_VERSION_base(4,10,0)
instance HasGlb (a :~~: b) where glb = flatGlb
instance HasGlb (TR.TypeRep a) where glb = flatGlb
#endif

-- ...

-- Generic-derived instances
instance HasGlb ()
#if MIN_VERSION_base(4,7,0)
instance HasGlb (Proxy.Proxy a)
#endif
instance HasGlb Bool
instance HasGlb Ordering
instance (HasGlb a, HasGlb b) => HasGlb (Either a b)
instance HasGlb a => HasGlb (Maybe a)
instance HasGlb a => HasGlb [a]
instance HasGlb a => HasGlb (ZipList a)

instance (HasGlb a, HasGlb b) => HasGlb (a, b)
instance (HasGlb a, HasGlb b, HasGlb c) => HasGlb (a, b, c)
instance (HasGlb a, HasGlb b, HasGlb c, HasGlb d) => HasGlb (a, b, c, d)
instance (HasGlb a, HasGlb b, HasGlb c, HasGlb d, HasGlb e) => HasGlb (a, b, c, d, e)

instance HasGlb a => HasGlb (Const a b)

#if MIN_VERSION_base(4,8,0)
instance HasGlb a => HasGlb (Identity.Identity a)
instance HasGlb Void.Void
#endif

-- People often use :+: and :*: rather than Sum and Product
-- even outside of a Generic context.
instance (HasGlb (f a), HasGlb (g a)) => HasGlb ((f :*: g) a)
instance (HasGlb (f a), HasGlb (g a)) => HasGlb ((f :+: g) a)

#if MIN_VERSION_base(4,9,0)
instance HasGlb (f (g a)) => HasGlb (Compose.Compose f g a)
instance (HasGlb (f a), HasGlb (g a)) => HasGlb (Product.Product f g a)
instance (HasGlb (f a), HasGlb (g a)) => HasGlb (Sum.Sum f g a)
#endif


-- Functions
instance HasGlb b => HasGlb (a -> b) where
  glb = liftA2 glb

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

-- | Used for generic deriving of 'HasGlb'
class GHasGlb f where
  gglb :: (Generic a, Rep a ~ f) => a -> a -> a

-- | A suitable definition of 'glb' for instances of 'Generic'.
genericGlb :: (Generic a, GHasGlb (Rep a)) => a -> a -> a
-- What makes genericGlb different from gglb? When using
-- TypeApplications, the first type argument of gglb is
-- the representation type; that's not very friendly.
genericGlb a b = gglb a b

-- Newtypes don't want their outsides forced/checked, because they don't have any.
instance HasGlb x => GHasGlb (D1 ('MetaData _q _r _s 'True) (C1 _t (S1 _u (K1 _v x)))) where
  gglb a b
    | M1 (M1 (M1 (K1 x))) <- from a
    , M1 (M1 (M1 (K1 y))) <- from b
    = to (M1 (M1 (M1 (K1 (glb x y)))))

-- Not a newtype, but possibly a lifted unary tuple.
-- We force the outsides first in case that is so.
instance GHasGlb' f => GHasGlb (D1 ('MetaData _q _r _s 'False) f) where
  gglb !a !b
    = to (M1 (gglb' ar br))
    where
      M1 ar = from a
      M1 br = from b

-- | Used for non-newtype 'Generic' deriving.
class GHasGlb' f where
  gglb' :: f p -> f p -> f p

instance GHasGlb' f => GHasGlb' (M1 i c f) where
  gglb' (M1 l) (M1 r) = M1 (gglb' l r)

instance (GHasGlb' f, GHasGlb' g, HasCon f, HasCon g) => GHasGlb' (f :+: g) where
  gglb' (L1 l1) (L1 l2) = L1 (gglb' l1 l2)
  gglb' (R1 r1) (R1 r2) = R1 (gglb' r1 r2)

  gglb' (L1 l1) (R1 r2) = mismatchedCons (getConName l1) (getConName r2)
  gglb' (R1 r1) (L1 l2) = mismatchedCons (getConName r1) (getConName l2)

class HasCon f where
  getConName :: f p -> String
instance Constructor i => HasCon (C1 i f) where
  getConName = conName
instance (HasCon f, HasCon g) => HasCon (f :+: g) where
  getConName (L1 x) = getConName x
  getConName (R1 x) = getConName x

mismatchedCons :: String -> String -> a
mismatchedCons l r = glbBottom $
    "Mismatched constructors.\nThe left argument was built with "
    ++ l ++ ",\nbut the right one was built with " ++ r ++ "."

instance (GHasGlb' f, GHasGlb' g) => GHasGlb' (f :*: g) where
  gglb' (l1 :*: l2) (r1 :*: r2) =
    gglb' l1 r1 :*: gglb' l2 r2

instance GHasGlb' U1 where
  -- We pattern match strictly so we don't get
  --
  -- glb @() () undefined = ()
  gglb' U1 U1 = U1

instance GHasGlb' V1 where
#if __GLASGOW_HASKELL__ >= 708
  gglb' v _ = case v of
#else
  gglb' !_ _ = error "Can't happen"
#endif

instance HasGlb c => GHasGlb' (K1 i c) where
  gglb' (K1 l) (K1 r) = K1 $ glb l r

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
#endif
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
  -- * Generic deriving
  , GHasLub
  , genericLub
  ) where

import Control.Applicative (liftA2, Const)

import Data.Unamb hiding (parCommute)
-- import qualified Data.Unamb as Unamb

import GHC.Generics
#if MIN_VERSION_base(4,8,0)
import qualified Data.Functor.Identity as Identity
import qualified Data.Void as Void
#endif
#if MIN_VERSION_base(4,9,0)
import qualified Data.Functor.Compose as Compose
import qualified Data.Functor.Product as Product
import qualified Data.Functor.Sum as Sum
#endif

-- | Types that support information merging ('lub')
class HasLub a where
  -- | Least upper information bound.  Combines information available from
  -- each argument.  The arguments must be consistent, i.e., must have a
  -- common upper bound.
  lub  :: a -> a -> a
  default lub :: (Generic a, GHasLub (Rep a)) => a -> a -> a
  lub = genericLub
  -- | n-ary 'lub'.  Defaults to @foldr lub undefined@
  lubs :: [a] -> a
  lubs = foldr lub undefined

-- | A 'lub' for flat domains.  Equivalent to 'unamb'.  Handy for defining
-- 'HasLub' instances, e.g.,
-- 
-- @
--   instance HasLub Integer where lub = flatLub
-- @
flatLub :: a -> a -> a
flatLub = unamb

-- Flat types:
instance HasLub Char    where lub = flatLub
instance HasLub Int     where lub = flatLub
instance HasLub Integer where lub = flatLub
instance HasLub Float   where lub = flatLub
instance HasLub Double  where lub = flatLub
-- ...

-- Generic-derived types:
instance HasLub ()
instance HasLub Bool
instance (HasLub a, HasLub b) => HasLub (Either a b)
instance HasLub a => HasLub (Maybe a)
instance HasLub a => HasLub [a]

instance (HasLub a, HasLub b) => HasLub (a,b)
instance (HasLub a, HasLub b, HasLub c) => HasLub (a,b,c)
instance (HasLub a, HasLub b, HasLub c, HasLub d) => HasLub (a,b,c,d)
instance (HasLub a, HasLub b, HasLub c, HasLub d, HasLub e) => HasLub (a,b,c,d,e)

instance HasLub a => HasLub (Const a b)

#if MIN_VERSION_base(4,8,0)
instance HasLub a => HasLub (Identity.Identity a)
instance HasLub Void.Void
#endif

-- People often use :+: and :*: rather than Sum and Product
-- even outside of a Generic context.
instance (HasLub (f a), HasLub (g a)) => HasLub ((f :*: g) a)
instance (HasLub (f a), HasLub (g a)) => HasLub ((f :+: g) a)

#if MIN_VERSION_base(4,9,0)
instance HasLub (f (g a)) => HasLub (Compose.Compose f g a)
instance (HasLub (f a), HasLub (g a)) => HasLub (Product.Product f g a)
instance (HasLub (f a), HasLub (g a)) => HasLub (Sum.Sum f g a)
#endif

-- Functions. This is not *strictly* correct, because it converts `undefined`
-- into `const undefined`, but anyone who cares is doing something fishy
-- anyway.
instance HasLub b => HasLub (a -> b) where
  lub = liftA2 lub
  -- f `lub` g = \ a -> f a `lub` g a

-- | Turn a binary commutative operation into that tries both orders in
-- parallel, 'lub'-merging the results.  Useful when there are special
-- cases that don't require evaluating both arguments.
-- 
-- Similar to parCommute from Unamb, but uses 'lub' instead of 'unamb'.
parCommute :: HasLub b => (a -> a -> b) -> (a -> a -> b)
parCommute op x y = (x `op` y) `lub` (y `op` x)

-- | Multiplication optimized for either argument being zero or one, where
-- the other might be expensive/delayed.
ptimes :: (HasLub a, Eq a, Num a) => a -> a -> a
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

zip' :: (HasLub a, HasLub b) => [a] -> [b] -> [(a,b)]
zip' = lubs [p1,p2,p3]
 where
   p1 []     _      = []
   p2 _      []     = []
   p3 (x:xs) (y:ys) = (x,y) : zip' xs ys

zip' [] (error "boom") :: [(Int,Int)]
zip' (error "boom") [] :: [(Int,Int)]

zip' [10,20] (1 : 2 : error "boom")
zip' (1 : 2 : error "boom") [10,20]

-}


-- ------------------------
-- Generic deriving

-- | Used for generic deriving of 'HasLub'
class GHasLub f where
  -- Yes, this is an unusual type for the method of a class of Generic
  -- representations. But we need to make decisions about what we do with `a`
  -- itself based on what its representation looks like, and this seems
  -- to be the simplest way to achieve that by far.
  glub :: (Generic a, Rep a ~ f) => a -> a -> a

-- | A suitable definition of 'lub' for instances of 'Generic'.
genericLub :: (Generic a, GHasLub (Rep a)) => a -> a -> a
-- What makes genericLub different from glub? When using
-- TypeApplications, the first type argument of glub is
-- the representation type; that's not very friendly.
genericLub a b = glub a b

-- Newtypes don't want their outsides forced/checked, because they don't have any.
instance HasLub x => GHasLub (D1 ('MetaData _q _r _s 'True) (C1 _t (S1 _u (K1 _v x)))) where
  glub a b
    | M1 (M1 (M1 (K1 x))) <- from a
    , M1 (M1 (M1 (K1 y))) <- from b
    = to (M1 (M1 (M1 (K1 (lub x y)))))

-- Not a newtype. First, we use 'unamb' to get the value in WHNF.
-- We can then walk the generic representation of that WHNF value,
-- setting up 'lub' computations using the actual values stored
-- in the (generic representations of) the two argument values.
instance GHasLub' f => GHasLub (D1 ('MetaData _q _r _s 'False) f) where
  -- It turns out to be *really* helpful to use `unamb a b` here rather than
  -- unamb (from a) (from b). Doing so gets us really clean Core without a
  -- bunch of unnecessary generic conversions. Basically, we want to avoid
  -- computing any generic representations within `unamb`, because nothing can
  -- inline through that. An extra side benefit is that we can use the same
  -- GHasLub instance for lifted unary tuples as for other non-newtype types,
  -- which avoids a lot of mess.
  glub a b
    = to (M1 (glub' (unM1 (from ab)) ar br))
    where
      M1 ar = from a
      M1 br = from b
      -- We force ab here in case the type is a lifted unary tuple, in which case
      -- its outside *won't* be forced by glub'.
      !ab = a `unamb` b

-- | Used for non-newtype 'Generic' deriving.
class GHasLub' f where
  -- | The first argument is used to get constructor
  -- info. We are free to pattern match
  -- on it all we like.
  glub' :: f p -> f p -> f p -> f p

instance GHasLub' f => GHasLub' (M1 i c f) where
  glub' (M1 outer) (M1 l) (M1 r) = M1 (glub' outer l r)

instance (GHasLub' f, GHasLub' g) => GHasLub' (f :+: g) where
  glub' (L1 o) ~(L1 l1) ~(L1 l2) = L1 (glub' o l1 l2)
  glub' (R1 o) ~(R1 r1) ~(R1 r2) = R1 (glub' o r1 r2)

instance (GHasLub' f, GHasLub' g) => GHasLub' (f :*: g) where
  -- We must pattern match strictly on the first argument, because
  -- otherwise we'll end up with things like
  --
  --   lub @(a,b) undefined undefined = (undefined, undefined)
  glub' (o1 :*: o2) ~(l1 :*: l2) ~(r1 :*: r2) =
    glub' o1 l1 r1 :*: glub' o2 l2 r2

instance GHasLub' U1 where
  -- We pattern match strictly so we don't get
  --
  -- lub @() undefined undefined = ()
  glub' U1 _ _ = U1

instance GHasLub' V1 where
#if __GLASGOW_HASKELL__ >= 708
  glub' v _ _ = case v of
#else
  glub' !_ _ _ = error "Can't happen"
#endif

instance HasLub c => GHasLub' (K1 i c) where
  glub' _ (K1 l) (K1 r) = K1 $ lub l r

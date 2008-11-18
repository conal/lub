{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.AssocRepr
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Compute least upper bounds (lub / join) of two values
-- 
-- This version uses associated types for HasRepr
----------------------------------------------------------------------

module Data.AssocRepr (HasRepr(..), onRepr, onRepr2) where

-- Reprs.  TODO: find & use a simple, standard generic programming framework.

class HasRepr t where
  type Repr t
  repr   :: t -> Repr t
  unrepr :: Repr t -> t

-- | Apply a binary function on a repr
onRepr :: (HasRepr a, HasRepr b) =>
          (Repr a -> Repr b)
       -> (a -> b)
onRepr h = unrepr . h . repr

-- | Apply a binary function on a repr
onRepr2 :: (HasRepr a, HasRepr b, HasRepr c) =>
           (Repr a -> Repr b -> Repr c)
        -> (a -> b -> c)
onRepr2 op a b = unrepr (repr a `op` repr b)


-- Repr instances

instance HasRepr (Maybe a) where
  type Repr (Maybe a) = Either () a
  
  repr   Nothing      = (Left ())
  repr   (Just a)     = (Right a)
  
  unrepr (Left ())    = Nothing
  unrepr (Right a)    = (Just a)
  

instance HasRepr [a] where
  type Repr [a]         = Either () (a,[a])
  
  repr   []             = (Left  ())
  repr   (a:as)         = (Right (a,as))
  
  unrepr (Left  ())     = []
  unrepr (Right (a,as)) = (a:as)

-- ...

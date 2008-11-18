{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances
           , FunctionalDependencies
  #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.View
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Compute least upper bounds (lub / join) of two values
-- 
-- This version uses associated types for HasView
----------------------------------------------------------------------

module Data.View (HasView(..), onView, onView2) where

-- Views.  TODO: find & use a simple, standard generic programming framework.

class HasView t v | t -> v where
  view   :: t -> v
  unview :: v -> t

-- | Apply a binary function on a view
onView :: (HasView a va, HasView b vb) =>
          (va -> vb)
       -> ( a ->  b)
onView h = unview . h . view

-- | Apply a binary function on a view
onView2 :: (HasView a va, HasView b vb, HasView c vc) =>
           (va -> vb -> vc)
        -> ( a ->  b ->  c)
onView2 op a b = unview (view a `op` view b)


-- View instances

instance HasView (Maybe a) (Either () a) where
  view   Nothing      = (Left ())
  view   (Just a)     = (Right a)
  
  unview (Left ())    = Nothing
  unview (Right a)    = (Just a)
  

instance HasView [a] (Either () (a,[a])) where
  view   []             = (Left  ())
  view   (a:as)         = (Right (a,as))
  
  unview (Left  ())     = []
  unview (Right (a,as)) = (a:as)

-- ...

{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.AssocView
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

module Data.AssocView (HasView(..), onView, onView2) where

-- Views.  TODO: find & use a simple, standard generic programming framework.

class HasView t where
  type View t
  view   :: t -> View t
  unview :: View t -> t

-- | Apply a binary function on a view
onView :: (HasView a, HasView b) =>
          (View a -> View b)
       -> (a -> b)
onView h = unview . h . view

-- | Apply a binary function on a view
onView2 :: (HasView a, HasView b, HasView c) =>
           (View a -> View b -> View c)
        -> (a -> b -> c)
onView2 op a b = unview (view a `op` view b)


-- View instances

instance HasView (Maybe a) where
  type View (Maybe a) = Either () a
  
  view   Nothing      = (Left ())
  view   (Just a)     = (Right a)
  
  unview (Left ())    = Nothing
  unview (Right a)    = (Just a)
  

instance HasView [a] where
  type View [a]         = Either () (a,[a])
  
  view   []             = (Left  ())
  view   (a:as)         = (Right (a,as))
  
  unview (Left  ())     = []
  unview (Right (a,as)) = (a:as)

-- ...

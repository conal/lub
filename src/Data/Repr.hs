{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances
           , FunctionalDependencies
  #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.Repr
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

module Data.Repr (HasRepr(..), onRepr, onRepr2) where

-- Reprs.  TODO: find & use a simple, standard generic programming framework.

-- | A data type representation, in terms of standard data types.
-- Requires that @'unrepr' . 'repr' == 'id'@.
class HasRepr t r | t -> r where
  repr   :: t -> r  -- ^  to  representation
  unrepr :: r -> t  -- ^ from representation

-- | Apply a binary function on a repr
onRepr :: (HasRepr a ra, HasRepr b rb) =>
          (ra -> rb) -> (a -> b)
onRepr h = unrepr . h . repr

-- | Apply a binary function on a repr
onRepr2 :: (HasRepr a ra, HasRepr b rb, HasRepr c rc) =>
           (ra -> rb -> rc) -> (a -> b -> c)
onRepr2 h a b = unrepr (h (repr a) (repr b))

-- Equivalently:
-- 
--   onRepr2 h a = unrepr . h (repr a) . repr
--   
--   onRepr2 h a = onRepr (h (repr a))
--   
--   onRepr2 h = onRepr . h . repr



-- Some HasRepr instances

instance HasRepr (Maybe a) (Either () a) where
  repr   Nothing   = (Left ())
  repr   (Just a)  = (Right a)
  
  unrepr (Left ()) = Nothing
  unrepr (Right a) = (Just a)
  

instance HasRepr [a] (Either () (a,[a])) where
  repr   []             = (Left  ())
  repr   (a:as)         = (Right (a,as))
  
  unrepr (Left  ())     = []
  unrepr (Right (a,as)) = (a:as)

-- ...

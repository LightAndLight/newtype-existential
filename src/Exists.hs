{-# language RankNTypes #-}
{-# language ConstraintKinds, PolyKinds, KindSignatures #-}
{-# language FlexibleInstances #-}
module Exists where

class Vacuous (a :: k) where
instance Vacuous (a :: k) where

newtype Exists (f :: k -> *)
  = Exists
  { unExists
    :: forall r
    . (forall a. f a -> r) -> r
  }

withExists
  :: Exists f
  -> (forall a. f a -> r) -> r
withExists = unExists

exists :: f a -> Exists f
exists a = Exists $ \f -> f a

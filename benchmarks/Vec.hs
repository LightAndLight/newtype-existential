{-# language ConstraintKinds, PolyKinds, KindSignatures, DataKinds, GADTs #-}
module Vec where

import Prelude hiding (replicate)
import Exists

data Nat = Z | S Nat
data Vec :: Nat -> a -> * where
  Nil :: Vec 'Z a
  Cons :: a -> Vec s a -> Vec ('S n) a

newtype Flip f a b = Flip { unFlip :: f b a }

replicate
  :: Int
  -> a
  -> Exists (Flip Vec a)
replicate 0 _ = exists $ Flip Nil
replicate n a =
  withExists
    (replicate (n-1) a) $
    \rest -> exists . Flip $ Cons a (unFlip rest)

fromVec :: Exists (Flip Vec a) -> [a]
fromVec = go
  where
    go :: Exists (Flip Vec a) -> [a]
    go e =
      withExists e $ \v ->
      case unFlip v of
        Nil -> []
        Cons a as -> a : go (exists (Flip as))

toVec
  :: [a]
  -> Exists (Flip Vec a)
toVec [] = exists (Flip Nil)
toVec (a:as) =
  withExists
    (toVec as) $
    \(Flip rest) -> exists (Flip $ Cons a rest)

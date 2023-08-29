{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Data.Functor.Foldable.Mutu (mutu) where

import Data.Functor.Foldable (Base, Recursive, cata)
import Data.HVect (HVect (..))
import Data.Kind (Type)

data FVect :: (Type -> Type) -> [Type] -> Type where
  FNil :: FVect f '[]
  (:$:) :: !(f t) -> !(FVect f ts) -> FVect f (t ': ts)

mapAlg :: (forall a. f a -> a) -> FVect f ts -> HVect ts
mapAlg _ FNil = HNil
mapAlg alg (x :$: xs) = alg x :&: mapAlg alg xs

mutu :: (Recursive t) => FVect ((->) (Base t (HVect ts))) ts -> t -> HVect ts
mutu algs = cata $ \x -> mapAlg ($ x) algs

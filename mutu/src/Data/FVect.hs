{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Data.FVect where

import Data.HVect (HVect (..))
import Data.Kind (Type)

data FVect :: (Type -> Type) -> [Type] -> Type where
  FNil :: FVect f '[]
  (:$:) :: !(f t) -> !(FVect f ts) -> FVect f (t ': ts)

mapAlg :: (forall a. f a -> a) -> FVect f ts -> HVect ts
mapAlg _ FNil = HNil
mapAlg alg (x :$: xs) = alg x :&: mapAlg alg xs

mapNT :: (forall t. f t -> g t) -> FVect f ts -> FVect g ts
mapNT _ FNil = FNil
mapNT nt (x :$: xs) = nt x :$: mapNT nt xs

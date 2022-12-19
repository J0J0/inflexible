-- |
-- Module      : HFMext
-- Description : Extension of the @HaskellForMaths@ package
-- 
-- This module contains some functionality that is directly
-- related to the @HaskellForMaths(-excerpt)@ package,
-- but does not seem to exist there.

module HFMext
    (   basisElems
    ,   coeffs
    ,   changeBaseRing
    ) where

import Math.Algebras.VectorSpace


basisElems :: Vect k b -> [b]
basisElems = map fst . terms

coeffs :: Vect k b -> [k]
coeffs = map snd . terms

changeBaseRing :: (Eq k', Num k')
               => (k -> k') -> Vect k a -> Vect k' a
changeBaseRing phi (V xs) =
    V [(b,k') | (b,k) <- xs, let k' = phi k, k' /= 0]

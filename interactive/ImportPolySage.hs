module ImportPolySage
    (   sageEliminate
    ,   sageHasFiniteProjection
    ) where

import Data.Coerce (coerce)

import Math.CommutativeAlgebra.Polynomial as Poly
import Math.Core.Field (Q)
import Extern.Sage as Sage

import HFMext

import Var

-- | A drop-in for 'Math.CommutativeAlgebra.GroebnerBasis.eliminate',
-- modulo IO of course.
sageEliminate :: [GrevlexPoly Q T]     -- ^ variables to eliminate
              -> [GrevlexPoly Q T]     -- ^ generators of an ideal
              -> IO [GrevlexPoly Q T]  -- ^ generators of the elemination ideal
sageEliminate vs ps = fmap coerce $
    Sage.eliminationIdeal' (coerce ps) (coerce vs')
  where
    vs' = map extract_var vs
 
sageHasFiniteProjection :: [GrevlexPoly Q T]  -- ^ generators of an ideal
                        -> GrevlexPoly Q T    -- ^ variable to project on
                        -> IO Bool
sageHasFiniteProjection ps v =
    Sage.hasFiniteProjection' (coerce ps) (coerce v')
  where
    v' = extract_var v

extract_var :: GrevlexPoly Q T -> T
extract_var = fst . one_or_err . Poly.mindices . one_or_err . basisElems
  where
    one_or_err [x] = x
    one_or_err _   =
        error $ "argument that should contain only variables" <>
                "contains non-variable polynomial"

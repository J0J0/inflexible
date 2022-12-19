{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving #-}

import Prelude hiding ( (*>), (<*) )
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Ratio
import qualified Data.Map.Strict as M

import Math.Core.Field (Q(..), numeratorQ, denominatorQ)
import Math.Algebras.VectorSpace
import Math.Algebras.Structures
import Math.Algebras.TensorAlgebra
import Math.Algebras.TensorProduct
import qualified Math.CommutativeAlgebra.Polynomial as Poly
import Math.CommutativeAlgebra.Polynomial (GrevlexPoly, Grevlex(Grevlex), MonImpl(M))

import qualified Numeric.Matrix as Mat

import FreeGradComAlg
import FracClear
import Util


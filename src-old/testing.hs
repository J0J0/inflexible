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


data G = X1 | X2 | Y1 | Y2 | Y3 | Z deriving (Eq, Ord, Show, Enum, Bounded)

-- A1
gs = M.fromList [(X1,2), (X2,4), (Y1,9), (Y2,11), (Y3,13), (Z,35)]
glist@[x1,x2, y1,y2,y3, z] = (fromJust . injectFGCA' gs) <$> M.keys gs   -- :: [Lam Rational G]

w = x2^2*y1*y2 - x1*x2*y1*y3 + x1^2*y2*y3
d0 = M.fromList [(X1,0), (X2,0), (Y1, x1^3*x2), (Y2, x1^2*x2^2), (Y3, x1*x2^3), (Z, x2^2*w + x1^18+x2^9) ]
d = extendDiff (d0 M.!)

newtype T = T { getT :: Int } deriving (Eq, Ord, Enum, Bounded)
instance Show T where show (T k) = "α" ++ show k
                      
maple_show :: (GrevlexPoly Q T) -> String
maple_show (V xs) = intercalate "  +  " (s_coeffmultinom <$> xs)
    where
        s_coeffmultinom (b,q) = "(" ++ show q ++ ") * " ++ s_multinom b
        s_multinom (Grevlex (M _ []))  = "1"
        s_multinom (Grevlex (M _ xis)) = intercalate "*" (s_varpow <$> xis)
        s_varpow (x,1) = s_var x
        s_varpow (x,i) = s_var x ++ "^" ++ show i
        s_var (T k) = "T__" ++ show k

                      
f = genericGAlgEndo gs :: Lam (GrevlexPoly Q T) G -> Lam (GrevlexPoly Q T) G

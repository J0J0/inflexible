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


glist@[x1,x2, y1,y2,y3, z] = (injS <$> [X1,X2]) ++ (injE <$> [Y1,Y2,Y3,Z])
    where
        injS = injectFGCA_unsafe . Left
        injE = injectFGCA_unsafe . Right

-- A2
gs2 = M.fromList $ zip [X1,X2,Y1,Y2,Y3,Z] [4,6,17,19,21,59]

d2 = extendDiff $ (M.!) $ M.fromList [
        (X1,0),
        (X2,0),
        (Y1, x1^3*x2  ),
        (Y2, x1^2*x2^2),
        (Y3, x1*x2^3  ),
        (Z,  x2^4*y1*y2 - x1*x2^3*y1*y3 + x1^2*x2^2*y2*y3 + x1^15 + x2^10)
                                     ]
                      
f2 = genericGAlgEndo gs2 :: Lam (GrevlexPoly Q T) G -> Lam (GrevlexPoly Q T) G

-- A3
gs3 = M.fromList $ zip [X1,X2,Y1,Y2,Y3,Z] [8,10,33,35,37,119]

d3 = extendDiff $ (M.!) $ M.fromList [
        (X1,0),
        (X2,0),
        (Y1, x1^3*x2  ),
        (Y2, x1^2*x2^2),
        (Y3, x1*x2^3  ),
        (Z,  x1^4*x2^2*y1*y2 - x1^5*y1*y3 + x1^6*y2*y3 + x1^15 + x2^12)
                                     ]
                      
f3 = genericGAlgEndo gs3 :: Lam (GrevlexPoly Q T) G -> Lam (GrevlexPoly Q T) G

-- A4
gs4 = M.fromList $ zip [X1,X2,Y1,Y2,Y3,Z] [10,12,41,43,45,119]

d4 = extendDiff $ (M.!) $ M.fromList [
        (X1,0),
        (X2,0),
        (Y1, x1^3*x2  ),
        (Y2, x1^2*x2^2),
        (Y3, x1*x2^3  ),
        (Z,  x2^3*y1*y2 - x1*x2^2*y1*y3 + x1^2*x2*y2*y3 + x1^12 + x2^10)
                                     ]
                      
f4 = genericGAlgEndo gs4 :: Lam (GrevlexPoly Q T) G -> Lam (GrevlexPoly Q T) G

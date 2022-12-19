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

import Extern.Maple



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


data G = X1 | X2 | Y1 | Y2 | Y3 | Z deriving (Eq, Ord, Show, Enum, Bounded)
                      
-- A1
gs = M.fromList [(X1,2), (X2,2), (Y1,7), (Y2,7), (Y3,7), (Z,19)]
glist@[x1,x2, y1,y2,y3, z] = (fromJust . injectFGCA' gs) <$> M.keys gs
glistQ = glist :: [Lam Q G]

w = x2^2*y1*y2 - x1*x2*y1*y3 + x1^2*y2*y3
d0 = M.fromList [(X1,0), (X2,0), (Y1, x1^3*x2), (Y2, x1^2*x2^2), (Y3, x1*x2^3), (Z, x1*w + x1^10+x2^10) ]
d = extendDiff (d0 M.!)

f  = genericGAlgEndo gs :: Lam (GrevlexPoly Q T) G -> Lam (GrevlexPoly Q T) G
cs = genericConstraints gs d :: [GrevlexPoly Q T]


maple_solve :: T -> IO [String]
maple_solve (T k) = do
    let ps = intercalate ", " (map maple_show cs)
        i  = "T__" ++ show k
    m <- newMaple
    putMaple m "read \"/tmp/ps.mpl\":"
    putMaple m $ "solve_constr({" ++ ps ++ "}, " ++ i ++ "):"
    resp <- getMaple m
    closeMaple m
    return resp

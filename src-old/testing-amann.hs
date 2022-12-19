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

maple_solve :: [GrevlexPoly Q T] -> T -> IO [String]
maple_solve cs (T k) = do
    let ps = intercalate ", " (map maple_show cs)
        i  = "T__" ++ show k
    m <- newMaple
    putMaple m "read \"/tmp/ps.mpl\":"
    putMaple m $ "solve_constr({" ++ ps ++ "}, " ++ i ++ "):"
    resp <- getMaple m
    closeMaple m
    return resp


data G = X1 | X2 | Y1 | Y2 | Y3 | Z | Z' deriving (Eq, Ord, Show, Enum, Bounded)

glist@[x1,x2, y1,y2,y3, z,z'] = (injS <$> [X1,X2]) ++ (injE <$> [Y1,Y2,Y3,Z,Z'])
    where
        injS = injectFGCA_unsafe . Left
        injE = injectFGCA_unsafe . Right

-- A_0
gs0 = M.fromList $ zip [X1,X2,Y1,Y2,Y3,Z,Z'] [4,6, 27,29,31, 77,75] -- 75+4i

d0 = extendDiff $ (M.!) $ M.fromList [
        (X1,0),
        (X2,0),
        (Y1, x1^4*x2^2),
        (Y2, x1^3*x2^3),
        (Y3, x1^2*x2^4),
        (Z,  x1*x2^3*y1*y2 - x1^2*x2^2*y1*y3 + x1^3*x2*y2*y3 + x2*x1^18 + x2^13),
        (Z', x1^19) -- 19+i
                                     ]

f0  = genericGAlgEndo gs0 :: Lam (GrevlexPoly Q T) G -> Lam (GrevlexPoly Q T) G
cs0 = genericConstraints gs0 d0 :: [GrevlexPoly Q T]

vol0 = x2^26*z' - x1^15*x2^24*y1  -- 15+i

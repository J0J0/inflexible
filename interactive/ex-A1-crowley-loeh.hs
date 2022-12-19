{-# LANGUAGE NoMonomorphismRestriction #-}

import Prelude hiding ( (*>), (<*) )
import ImportAll
import qualified ImportPoly as Poly
import Var (T(T))

data G = X1 | X2 | Y1 | Y2 | Y3 | Z deriving (Eq, Ord, Show, Enum, Bounded)

[x1,x2, y1,y2,y3, z] =
    (injectFGCA_even <$> [X1,X2]) ++ (injectFGCA_odd <$> [Y1,Y2,Y3,Z])

-- A1
a1 = mkDga [ (X1, 2,  0)
           , (X2, 4,  0)
           , (Y1, 9,  x1^3*x2  )
           , (Y2,11,  x1^2*x2^2)
           , (Y3,13,  x1*x2^3  )
           , ( Z,35,  x2^4*y1*y2 - x1*x2^3*y1*y3 + x1^2*x2^2*y2*y3 + x1^18 + x2^9)
           ]

w = x2^2*y1*y2 - x1*x2*y1*y3 + x1^2*y2*y3

f  = genericAlgMor a1 a1 :: Lam (GrevlexPoly Q T) G -> Lam (GrevlexPoly Q T) G
cs = genericDgaEndoConstraints a1 :: [GrevlexPoly Q T]

vol = x2^16
[bx2] = basisElems x2
t_for_x2 = coeff bx2 (f x2)

elim_vars = delete t_for_x2 $ nub $ concat $ map Poly.vars cs

elim_ideal_gens = Poly.eliminate elim_vars cs

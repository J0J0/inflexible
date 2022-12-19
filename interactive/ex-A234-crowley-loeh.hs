{-# LANGUAGE NoMonomorphismRestriction #-}

import Prelude hiding ( (*>), (<*) )
import ImportAll
import qualified ImportPoly as Poly
import Var (T(T))

data G = X1 | X2 | Y1 | Y2 | Y3 | Z deriving (Eq, Ord, Show, Enum, Bounded)

glist@[x1,x2, y1,y2,y3, z] = (injS <$> [X1,X2]) ++ (injE <$> [Y1,Y2,Y3,Z])
    where
        injS = injectFGCA_unsafe . Left
        injE = injectFGCA_unsafe . Right

d_y1 = x1^3*x2
d_y2 = x1^2*x2^2
d_y3 = x1*x2^3

-- A2
a2 = mkDga [ (X1, 4,  0)
           , (X2, 6,  0)
           , (Y1,17,  d_y1)
           , (Y2,19,  d_y2)
           , (Y3,21,  d_y3)
           , ( Z,59,  x2^4*y1*y2 - x1*x2^3*y1*y3 + x1^2*x2^2*y2*y3 + x1^15 + x2^10)
           ]
-- A3
a3 = mkDga [ (X1,  8,  0)
           , (X2, 10,  0)
           , (Y1, 33,  d_y1)
           , (Y2, 35,  d_y2)
           , (Y3, 37,  d_y3)
           , ( Z,119,  x1^4*x2^2*y1*y2 - x1^5*x2*y1*y3 + x1^6*y2*y3 + x1^15 + x2^12)
           ]

-- A4
a4 = mkDga [ (X1, 10,  0)
           , (X2, 12,  0)
           , (Y1, 41,  d_y1)
           , (Y2, 43,  d_y2)
           , (Y3, 45,  d_y3)
           , ( Z,119,  x2^3*y1*y2 - x1*x2^2*y1*y3 + x1^2*x2*y2*y3 + x1^12 + x2^10)
           ]

{-
my_dga  = a0
vol_gen = x1
vol_gen_power = 19

f  = genericAlgMor my_dga my_dga :: Lam (GrevlexPoly Q T) G -> Lam (GrevlexPoly Q T) G
cs = genericDgaEndoConstraints my_dga :: [GrevlexPoly Q T]

vol = vol_gen^vol_gen_power
[bvol] = basisElems vol_gen
t_for_vol_gen = coeff bvol (f vol_gen)

elim_vars = delete t_for_vol_gen $ nub $ concat $ map Poly.vars cs

elim_ideal_gens = Poly.eliminate elim_vars cs
-}

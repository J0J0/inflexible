{-# LANGUAGE NoMonomorphismRestriction #-}

import Prelude hiding ( (*>), (<*) )
import ImportAll
import qualified ImportPoly as Poly
import qualified Extern.Sage as Sage
import Var (T(T))

data G = X1 | X2 | Y1 | Y2 | Y3 | Z deriving (Eq, Ord, Show, Enum, Bounded)

glist@[x1,x2, y1,y2,y3, z] = (injS <$> [X1,X2]) ++ (injE <$> [Y1,Y2,Y3,Z])
    where
        injS = injectFGCA_unsafe . Left
        injE = injectFGCA_unsafe . Right

-- A0mod. A variant of A0, which is a variant of A1 to A4 from Crowley/Löh
a0mod = mkDga [ (X1, 2,  0)
              , (X2, 2,  0)
              , (Y1, 7,  x1^3*x2)
              , (Y2, 7,  x1^2*x2^2)
              , (Y3, 7,  x1*x2^3)
              , ( Z,17,  w + x1^9+x2^9)
              ]
  where
    w = x2^2*y1*y2 - x1*x2*y1*y3 + x1^2*y2*y3

my_dga  = a0mod
vol_gen = x1
vol_gen_power = 18

f  = genericAlgMor my_dga my_dga :: Lam (GrevlexPoly Q T) G -> Lam (GrevlexPoly Q T) G
cs = genericDgaEndoConstraints my_dga :: [GrevlexPoly Q T]

vol = vol_gen^vol_gen_power
[bvol] = basisElems vol_gen
t_for_vol_gen = coeff bvol (f vol_gen)
t_int         = let [(T v,_)] = (basisElems t_for_vol_gen >>= Poly.mindices) in v

elim_vars = delete t_for_vol_gen $ nub $ concat $ map Poly.vars cs

elim_ideal_finite = Sage.hasFiniteProjection' (coerce cs) t_int

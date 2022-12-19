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

-- A1, slightly modified
a1mod = mkDga [ (X1, 2,  0)
              , (X2, 4,  0)
              , (Y1, 9,  x1^3*x2)
              , (Y2,11,  x1^2*x2^2)
              , (Y3,13,  x1*x2^3)
              , ( Z,31,  x2*w + x1^16+x2^8)
              ]
  where
    w = x2^2*y1*y2 - x1*x2*y1*y3 + x1^2*y2*y3

my_dga  = a1mod
vol_gen = x2
vol_gen_power = 15

f  = genericAlgMor my_dga my_dga :: Lam (GrevlexPoly Q T) G -> Lam (GrevlexPoly Q T) G
cs = genericDgaEndoConstraints my_dga :: [GrevlexPoly Q T]

vol = vol_gen^vol_gen_power
[bvol] = basisElems vol_gen
t_for_vol_gen = coeff bvol (f vol_gen)

elim_vars = delete t_for_vol_gen $ nub $ concat $ map Poly.vars cs

elim_ideal_gens = Poly.eliminate elim_vars cs

{-# LANGUAGE NoMonomorphismRestriction #-}

import Prelude hiding ( (*>), (<*) )
import ImportAll
import qualified ImportPoly as Poly
import Var (T(T))

data G = X1 | X2 | Y1 | Y2 | Y3 | Z | U  deriving (Eq, Ord, Show, Enum, Bounded)
-- U = \bar{x}_2

glist@[x1,x2, y1,y2,y3, z, u] = (injS <$> [X1,X2]) ++ (injE <$> [Y1,Y2,Y3,Z]) ++ [injS U]
    where
        injS = injectFGCA_unsafe . Left
        injE = injectFGCA_unsafe . Right

fibr = mkDga [ (X1, 2,  0)
             , ( U, 2,  0)
             , (Y1, 9,  x1^3*u^2)
             , (Y2,11,  x1^2*u^4)
             , (Y3,13,  x1*u^6)
             , ( Z,35,  u^8*y1*y2 - x1*u^6*y1*y3 + x1^2*u^4*y2*y3 + x1^18 + u^18)
             ]

my_dga  = fibr
vol_gen = u
vol_gen_power = 33

f  = genericAlgMor my_dga my_dga :: Lam (GrevlexPoly Q T) G -> Lam (GrevlexPoly Q T) G
cs = genericDgaEndoConstraints my_dga :: [GrevlexPoly Q T]

vol = vol_gen^vol_gen_power
[bvol] = basisElems vol_gen
t_for_vol_gen = coeff bvol (f vol_gen)

elim_vars = delete t_for_vol_gen $ nub $ concat $ map Poly.vars cs

elim_ideal_gens = Poly.eliminate elim_vars cs

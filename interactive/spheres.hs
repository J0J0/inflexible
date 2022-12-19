{-# LANGUAGE NoMonomorphismRestriction #-}

import Prelude hiding ( (*>), (<*) )
import ImportAll
import qualified ImportPoly as Poly
import Var (T(T))

data G = X | Y | Z deriving (Eq, Ord, Show, Enum, Bounded)

(x,z, y) = (injE X, injE Z, injS Y)
  where
    injS = injectFGCA_unsafe . Left
    injE = injectFGCA_unsafe . Right

-- odd dimensional sphere, S^{2k+1}
n = 41
s_odd = mkDga [ (X,n, 0) ]

f_odd  = genericAlgMor s_odd s_odd :: Lam (GrevlexPoly Q T) G -> Lam (GrevlexPoly Q T) G
cs_odd = genericDgaEndoConstraints s_odd :: [GrevlexPoly Q T]

vol_odd = x
[bx] = basisElems x
t_for_x = coeff bx (f_odd x)

elim_x = delete t_for_x $ nub $ concat $ map Poly.vars cs_odd

elim_ideal_gens_odd = Poly.eliminate elim_x cs_odd

-- even dimensional sphere, S^{2k}
m = 42
s_even = mkDga [ (Y,m, 0)
               , (Z,2*m-1, y^2)
               ]

f_even  = genericAlgMor s_even s_even :: Lam (GrevlexPoly Q T) G -> Lam (GrevlexPoly Q T) G
cs_even = genericDgaEndoConstraints s_even :: [GrevlexPoly Q T]

vol_even = y
[by] = basisElems y
t_for_y = coeff by (f_even y)

elim_y = delete t_for_y $ nub $ concat $ map Poly.vars cs_even

elim_ideal_gens_even = Poly.eliminate elim_y cs_even

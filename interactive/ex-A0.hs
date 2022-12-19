{-# LANGUAGE NoMonomorphismRestriction, TypeApplications #-}

import Prelude hiding ( (*>), (<*) )
import ImportAll
import qualified ImportPoly     as Poly
import qualified ImportPolySage as Poly
import Var (T(T))

data G = X1 | X2 | Y1 | Y2 | Y3 | Z  deriving (Eq, Ord, Show, Enum, Bounded)

[x1,x2, y1,y2,y3, z] =
    (injectFGCA_even <$> [X1,X2]) ++ (injectFGCA_odd <$> [Y1,Y2,Y3,Z])

-- A0. A variant of A1 to A4 from Crowley/Löh
a0 = mkDga [ (X1, 2,  0)
           , (X2, 2,  0)
           , (Y1, 7,  x1^3*x2)
           , (Y2, 7,  x1^2*x2^2)
           , (Y3, 7,  x1*x2^3)
           , ( Z,19,  x1*w + x1^10+x2^10)
           ]
  where
    w = x2^2*y1*y2 - x1*x2*y1*y3 + x1^2*y2*y3

vol_gen = x1
vol_gen_power = 19
vol = vol_gen^vol_gen_power

a0Q     = a0 :: DgaSpec Q G
my_dga  = a0
my_dgaQ = a0Q

is_elliptic = maybe False id $ isElliptic' my_dgaQ

have_fundamental_class = dropCache $ do
    let q0 = degree my_dgaQ vol == Just (formalDimension my_dgaQ)
    q1 <- isCocycle my_dgaQ vol
    q2 <- not <$> isCoboundary my_dgaQ vol
    return $ q0 && q1 && q2

basic_info = do
    print my_dga
    putStrLn $ "Of dimension: " <> show (formalDimension my_dga)
    putStrLn $ "Elliptic: " <> show is_elliptic
    putStrLn $ "Fundamental class: " <>
        if have_fundamental_class then show vol else "None"

f  = genericAlgMor @(GrevlexPoly Q T) my_dga my_dga
cs = genericDgaEndoConstraints @(GrevlexPoly Q T) my_dga

[bvol] = basisElems vol_gen
t_for_vol_gen = coeff bvol (f vol_gen)
elim_vars = delete t_for_vol_gen $ nub $ concat $ map Poly.vars cs

elim_ideal_gens = Poly.eliminate elim_vars cs
elim_ideal_gens_via_sage = Poly.sageEliminate elim_vars cs

elim_ideal_finite = Poly.sageHasFiniteProjection cs t_for_vol_gen

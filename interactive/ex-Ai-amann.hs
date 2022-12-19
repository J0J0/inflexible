{-# LANGUAGE NoMonomorphismRestriction #-}

import Prelude hiding ( (*>), (<*) )
import ImportAll
import qualified ImportPoly as Poly
import Var (T(T))

data G = X1 | X2 | Y1 | Y2 | Y3 | Z | Z' deriving (Eq, Ord, Show, Enum, Bounded)

glist@[x1,x2, y1,y2,y3, z,z'] = (injS <$> [X1,X2]) ++ (injE <$> [Y1,Y2,Y3,Z,Z'])
    where
        injS = injectFGCA_unsafe . Left
        injE = injectFGCA_unsafe . Right

d_y1 = x1^4*x2^2
d_y2 = x1^3*x2^3
d_y3 = x1^2*x2^4
d_z  = x1*x2^3*y1*y2 - x1^2*x2^2*y1*y3 + x1^3*x2*y2*y3 + x2*x1^18 + x2^13

a i = mkDga [ (X1, 4,  0)
            , (X2, 6,  0)
            , (Y1,27,  d_y1)
            , (Y2,29,  d_y2)
            , (Y3,31,  d_y3)
            , (Z ,77,  d_z)
            -- ^ independent of i
            -- v varies with i
            , (Z',75+4*i,  x1^(19+i))
            ]

vol i = x2^26*z' - x1^(15+i)*x2^24*y1

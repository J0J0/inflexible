{-# LANGUAGE NoMonomorphismRestriction, TypeApplications  #-}

import Prelude hiding ( (*>), (<*) )
import ImportAll

data G2 = T1 | T2 | Y1 | Y2  deriving (Eq, Ord, Show)

([t1,t2,y1,y2], apl2) =
    mkDgaWithGenerators [ (T1,0, y1)
                        , (T2,0, y2)
                        , (Y1,1, 0)
                        , (Y2,1, 0) ]


data G = T Int | Y Int  deriving (Eq, Ord, Show)

(t, y) = (injectFGCA_even . T, injectFGCA_odd . Y)

apl :: Int -> DgaSpec Q G
apl m = mkDga $ ts ++ ys
  where
    ts = map (\ j -> (T j, 0, y j)) [1..m]
    ys = map (\ j -> (Y j, 1, 0))   [1..m]

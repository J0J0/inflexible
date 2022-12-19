{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Alg.Classes
-- Description : Eq, Ord, Show instances for 'DgaSpec'

module Alg.Classes where

import Data.List (group, intercalate)
import Data.Function (on)
import qualified Data.Map.Lazy as M
import Data.Semigroup (Max(..))
import qualified Optics.Core as O

import Math.Algebras.TensorAlgebra (SymmetricAlgebra(Sym), ExteriorAlgebra(Ext))

import Types
import Alg
import Util (toSupscript)

-----------------
-- Eq and Ord instances for DgaSpec
instance ENO k a => Eq (DgaSpec k a) where
    (==) = (==) `on` (O.view finiteRepr)

instance (ENO k a, Ord k) => Ord (DgaSpec k a) where
    compare = compare `on` (O.view finiteRepr)

-----------------
-- Show instances

instance {-# OVERLAPPING #-} (Show a, Eq a) => Show (FGCA a) where
    show (Sym _ xs, Ext _ ys) = concat varpows --intercalate " " varpows
        where
            varpows = s <$> group (xs ++ ys)
            --s as@(a:_) = show a ++ "^" ++ show (length as)
            s as@(a:_) = show a ++ show_sup (length as)
            show_sup = fmap toSupscript . show

instance (Show k, Show a, ENO k a) => Show (DgaSpec k a) where
    show dga = "The dga specified by generators (with degrees)\n"
                ++ l_gens ++ "\n" ++ l_degs ++ "\n"
                ++ "and differential (given on generators):"
                ++ ls_diff
        where
            gens  = _gens dga
            gsS   = show <$> M.keys  gens
            degsS = show <$> M.elems gens
            
            width     = getMax . foldMap (Max . length)
            widthGens = width gsS
            widthAll  = widthGens `max` width degsS
            padAll  s = replicate (widthAll - length s) ' '  ++  s
            padGens s = s  ++ replicate (widthGens - length s) ' '
            
            both f (x,y) = (f x, f y)   -- TODO: replace!
            (l_gens, l_degs) =
                both (intercalate "  " . fmap padAll) (gsS, degsS)
            
            ls_diff = flip foldMap (M.keys gens `zip` gsS) $
                \ (z, zS) -> "\n" ++ padGens zS
                             ++ " |--> "
                             ++ show (_diff' dga z)

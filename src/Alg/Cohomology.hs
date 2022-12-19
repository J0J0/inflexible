{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module      : Alg.Cohomology
-- Description : Cohomological aspects
-- 
-- A cochain algebra is, in particular, a cochain complex.
-- Here we provide functionality to reason about the cohomology of the
-- latter. Throughout this module, we assume, that all algebras are connected
-- (that is, they have no generators of degree 0).
-- 
-- Most functions in this module expect /homogeneous/ algebra elements
-- as input, and __this is not checked__. It is the caller's responsibity
-- to pass only such elements. When in doubt, see if 'degree' yields
-- @Just@ some degree.
--
-- For brevity, we use the term /cochain/ as an abbreviation for homogeneous
-- element. (This coincides with the terminology of cochain complexes in
-- homological algebra where non-homogeneous elements do usually not appear.)
-- 
-- As an additional convenience (mainly useful for interactive sessions),
-- we provide "primed" variants of the main functions of this module, which
-- discard the cache ('Cache.dropCache') after they are done.

module Alg.Cohomology where

import           Prelude hiding ( (*>), (<*) )
import qualified Data.Map.Lazy as M
import           Data.Maybe (isJust)
import qualified Data.Set      as S

import qualified Numeric.Matrix as LA

import Math.Algebras.VectorSpace
import HFMext (coeffs, basisElems)

import Types
import Alg
import Lens
import Cache
import FracClear
import Util

-- | Check whether an algebra element is a cocycle, i.e. whether it is in the kernel
-- of the differential. (This also works on non-homogeneous elements.)
isCocycle :: (ENO k a) => DgaSpec k a -> Lam k a -> Cached k a Bool
isCocycle = return .: isCocycle'

-- | See 'isCocycle'.
isCocycle' :: (ENO k a) => DgaSpec k a -> Lam k a -> Bool
isCocycle' dga ch = _diff dga ch == 0

-- | Check whether a cochain is a coboundary, i.e. whether it is in the image
-- of the differential.
isCoboundary :: (ENO' k a, Elim' k, Eq (UnFrac k))
           => DgaSpec k a -> Lam k a -> Cached k a Bool
isCoboundary dga ch =
    fillCochain dga ch >>= \ mb -> return $ isCocycle' dga ch && isJust mb

-- | See 'isCoboundary'.
isCoboundary' :: (ENO' k a, Elim' k, Eq (UnFrac k))
            => DgaSpec k a -> Lam k a -> Bool
isCoboundary' = dropCache .: isCoboundary

-- | Given a cochain, compute @Just@ a preimage of it under the differential;
-- or @Nothing@ if it isn't a coboundary.
fillCochain :: (ENO' k a, Elim' k, Eq (UnFrac k))
          => DgaSpec k a
          -> Lam k a                       -- ^ cochain @c@ to be filled
          -> Cached k a (Maybe (Lam k a))  -- ^ a cochain @c'@ such that @d c' = c@,
                                           --   or Nothing if @c@ wasn't a coboundary.
fillCochain dga ch = do
    let deg = degree_unsafe dga ch
    
    hasOnlyBdyMonos dga deg ch >>= \case
        False -> return Nothing
        True  -> do
            monoms   <- bdyMonomials dga deg
            diff_ech <- llDifferential dga (deg-1)
            diff_fs  <- viewCachedErr "differentialFs unset" (DgaKey differentialFs) dga (deg-1)
            
            let (m, n) = LA.bounds (LA.echGetMat diff_ech)
                (c_raw, c_f) = unfracs [coeff mo ch | mo <- S.toAscList monoms]
                c_mat = LA.fromRows (map (:[]) (zipWith (*) diff_fs c_raw))
                mult  = maybe (error "dimension mismatch (How?!?)") id .: LA.mult
                c_ech = (LA.echGetTransform diff_ech) `mult` c_mat
            
            case all (\ i -> c_ech `LA.at` (i,1) == 0) [(LA.echGetRank diff_ech + 1)..m] of
                False -> return Nothing
                True  -> do
                    bs     <- findBasis dga (deg-1)
                    bdy_bs <- bdyBasis dga deg
                    let c i   = fromUnFrac $ c_ech `LA.at` (i,1)
                        a i j = fromUnFrac $ c_f * (LA.echGetMat diff_ech) `LA.at` (i,j)
                        rank  = LA.echGetRank diff_ech
                        pivs  = LA.echGetPivotCols diff_ech
                        
                        xs    = [ (c j - sum cs) / (a j kj)
                                | (j,kj) <- zip [rank,rank-1..1] pivs,
                                  let cs = zipWith (*)
                                            [a j kt | kt <- take (rank - j) pivs]
                                            xs
                                ]
                        preim = M.fromList [(_diff dga b, b) | b <- bs]
                        bs'   = (preim M.!) <$> bdy_bs
                    
                    return $ Just $ sum $ zipWith (<*) bs' xs

-- | See 'fillCochain'.
fillCochain' :: (ENO' k a, Elim' k, Eq (UnFrac k))
           => DgaSpec k a
           -> Lam k a          -- ^ cochain @c@ to be filled
           -> Maybe (Lam k a)  -- ^ a cochain @c'@ such that @d c' = c@,
                               --   or Nothing if @c@ wasn't a coboundary.
fillCochain' = dropCache .: fillCochain

-- | Given a dga, we try to show whether it is /elliptic/;
-- because our cochain algebras are finitely generated by design,
-- ellipticity is equivalent to finite dimensional cohomology.
-- 
-- The return value @Nothing@ signifies unknown ellipticity status.
-- 
-- Currently, this function can only determine the latter, if all
-- generators of even degree are cocycles. This includes the case
-- of pure Sullivan algebras.
isElliptic :: (ENO' k a, Elim' k, Eq (UnFrac k))
           => DgaSpec k a -> Cached k a (Maybe Bool)
isElliptic dga = do
    allM (isCocycle dga) even_gens >>= \case
        False -> return Nothing
        True  -> Just <$> allM vanish_eventually even_gens_with_deg
  where
    even_gens_with_deg =
        fmap (\ (x,k) -> (injectFGCA_even x, k)) $
            M.toList $ M.filter even (_gens dga)
    even_gens = fmap fst even_gens_with_deg
    
    -- if dga /is/ elliptic, it has the following dimension
    dim = formalDimension dga
    
    vanish_eventually (gen,degree) =
        let check_dim = (dim `div` degree) + 1
         in isCoboundary dga $ gen^check_dim

-- | See 'isElliptic'.
isElliptic' :: (ENO' k a, Elim' k, Eq (UnFrac k))
            => DgaSpec k a -> Maybe Bool
isElliptic' = dropCache . isElliptic

-- | Suppose @k@ is a field. Let \(f\) be an endomorphism of the given dga
-- \(A\) and let \(c\) be a cochain in \(A\) of degree \(n\),
-- such that
-- 
-- * the cohomology \( H^n(A) \) in degree \(n\) is one-dimensional over @k@, and
-- * the cochain \(c\) represents its non-trivial class, i.e. \(c\) is a cocycle
--   but /not/ a coboundary.
--
-- Then this function computes the /mapping degree of \(f\) with respect to \(c\)/,
-- i.e. the element \( \lambda \) from @k@, such that
-- \( H(f)[c] = \lambda \cdot [c] \;\in H^n(A) \) holds.
-- 
-- In the case of a Poincaré dga, e.g. an elliptic dga, of dimension \(n\),
-- the cochain \(c\) is a representative of the fundamental class and
-- this function simply returns the /mapping degree of \(f\)/.
--
mappingDegreeAt :: (ENO' k a, Elim' k)
                => DgaSpec k a
                -> Lam k a      -- ^ representative of fundamental class
                -> DgaEndo k a  -- ^ endomorphism
                -> Cached k a k -- ^ mapping degree of the endomorphism
mappingDegreeAt dga ch f = do
    let deg = degree_unsafe dga ch
    
    diff_ech <- llDifferential dga (deg-1)
    diff_fs  <- viewCachedErr "differentialFs unset" (DgaKey differentialFs) dga (deg-1)
    monoms   <- bdyMonomials dga deg
    
    let f_ch = f ch
        (fc_raw, fc_fac) = unfracs [coeff mo f_ch | mo <- S.toAscList monoms]
        (c_raw,  c_fac ) = unfracs [coeff mo ch   | mo <- S.toAscList monoms]
        
        mult     = maybe (error "dimension mismatch (How?!?)") id .: LA.mult
        ap_trans = ((LA.echGetTransform diff_ech) `mult`) . LA.fromRows . map (:[]) . zipWith (*) diff_fs
        
        rank = LA.echGetRank diff_ech
        
        fc_el = fromUnFrac $ ap_trans fc_raw `LA.at` (rank+1,1)
        c_el  = fromUnFrac $ ap_trans c_raw  `LA.at` (rank+1,1)
    
    return $ (fromUnFrac fc_fac * fc_el) / (fromUnFrac c_fac * c_el)

-- | See 'mappingDegreeAt'.
mappingDegreeAt' :: (ENO' k a, Elim' k)
                 => DgaSpec k a
                 -> Lam k a      -- ^ representative of fundamental class
                 -> DgaEndo k a  -- ^ endomorphism
                 -> k            -- ^ mapping degree of the endomorphism
mappingDegreeAt' dga ch f = dropCache $ mappingDegreeAt dga ch f

-- * Utilities

-- | Cached version of 'Alg.findBasis_'.
findBasis :: ENO' k a => DgaSpec k a -> Deg -> Cached k a (Basis k a)
findBasis = cachePureResult (GenKey chainsBasis) findBasis_

-- | Compute a basis of the submodule of coboundaries of a given 'DgaSpec' in a
-- specified degree.
bdyBasis :: (ENO' k a, Elim k) => DgaSpec k a -> Deg -> Cached k a (Basis k a)
bdyBasis dga deg = do
    llDifferential dga (deg-1)
    viewCachedErr "boundariesBasis unset" (DgaKey boundariesBasis) dga deg

-- | Compute the 'S.Set' of all monomials that appear in the submodule of
-- coboundaries in a specified degree.
bdyMonomials :: (ENO' k a, Elim k) => DgaSpec k a -> Deg -> Cached k a (S.Set (FGCA a))
bdyMonomials dga deg = do
    llDifferential dga (deg-1)
    viewCachedErr "monomialsBdy unset" (DgaKey monomialsBdy) dga deg

-- | Check whether an algebra element contains only monomials that appear in
-- the submodule of coboundaries of a given degree.
-- 
-- This is mainly useful as a first (and quick to check) necessary condition on
-- an element to be a coboundary.
hasOnlyBdyMonos :: (ENO' k a, Elim k)
                => DgaSpec k a
                -> Deg      -- degree of the next argument's cochain
                -> Lam k a
                -> Cached k a Bool
hasOnlyBdyMonos dga deg c = do
    ms <- bdyMonomials dga deg
    return $ (basisElems c) `allSatisfy` (`S.member` ms)


-- * Low-level access

-- | Reduce a matrix representation of the differential in the specified degree
-- to row echelon form and additionally cache other information that is
-- obtained on the way.
-- 
-- This function is used internally by the rest of this module.
llDifferential :: (ENO' k a, Elim k) => DgaSpec k a -> Deg -> Cached k a (LA.EchelonInfo (Mat k))
llDifferential = cacheResult (DgaKey differentialUF) llDifferential_

llDifferential_ :: (ENO' k a, Elim k) => DgaSpec k a -> Deg -> Cached k a (LA.EchelonInfo (Mat k))
llDifferential_ dga deg = do
    bs <- findBasis dga deg
    let imgGens = _diff dga <$> bs
        monoms  = foldMap (S.fromList . basisElems) imgGens
        (matl, rowdenoms)  = unzip $ map unfracs [coeff m <$> imgGens | m <- S.toAscList monoms]
        ech     = LA.echelonInfo (LA.fromRows matl)
        pivotCs = LA.echGetPivotCols ech
    
    setCached (DgaKey monomialsBdy) dga (deg+1) monoms
    setCached (DgaKey differentialFs) dga deg rowdenoms
    setCached (DgaKey boundariesBasis) dga (deg+1) $ (0:imgGens) `multiAt` pivotCs
    
    return ech

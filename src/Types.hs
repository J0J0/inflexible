{-# LANGUAGE ConstraintKinds, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- |
-- Module      : Types
-- Description : Type definitions and synonyms
-- 
-- This module collects all data types and type synonyms that
-- are used in the main library.
-- 
-- Note, that we use the terminology that we introduce in the beginning
-- of the "Alg" module.

module Types where

import           Control.Monad.State
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import qualified GHC.Generics
import qualified Generics.SOP
import           Optics.Core (Lens')

import           Math.Algebras.VectorSpace
import           Math.Algebras.Structures
import           Math.Algebras.TensorAlgebra
import           Math.Algebras.TensorProduct
import qualified Math.CommutativeAlgebra.Polynomial as Poly
import qualified Numeric.Matrix as LA

import FracClear

-- | A @'DgaSpec' k a@ contains the specification of a dga over @k@,
-- in terms of algebra generators with degrees and a differential.
-- 
-- Here, @k@ is the base ring and @a@ is the type of the generators.
-- (Note, that /not all/ inhabitants of @a@ need be generators. For
-- example, @a@ might be @String@ but only @"x"@ and @"y"@ be used as
-- generators.)
data DgaSpec k a = DgaSpec
    { _gens :: Generators a
    , _diff :: Differential k a
    }
    deriving (GHC.Generics.Generic, Generics.SOP.Generic)

-- | Type synonym for degree in the sense of gradings.
type Deg = Int
-- | A collection of algebra generators is represented as a 'M.Map'
-- whose keys are the generators with value the respective degree.
type Generators a = M.Map a Deg
-- | A differential is represented by an actual Haskell function.
-- Note, that this means that a 'DgaSpec' itself is usually not a "finite"
-- object, but see 'Alg.finiteRepr'.
type Differential k a = (Lam k a -> Lam k a)

-- | @'Lam' k a@ types the elements of algebras
-- specified by instances of @'DgaSpec' k a@.
type Lam k a = Vect k (FGCA a)
-- | Following the @HaskellForMaths@ convention (e.g. in "Math.Algebras.TensorAlgebra"),
-- the type (synonym) 'FGCA', which is short for @FreeGradedCommutativeAlgebra@,
-- types the /basis elements/ of the free module structure of such an algebra.
type FGCA a  = Tensor (SymmetricAlgebra a) (ExteriorAlgebra a)

-- | The type constraint synonym @'PolyRing' k p m v@ says that we want thy type @p@
-- to represent a polynomial ring over @k@ in variables coming from the
-- type @v@, using the monomial constructor @m@
-- (see "Math.CommutativeAlgebra.Polynomial" of the @HaskellForMaths@ package).
type PolyRing k p m v = ( Poly.MonomialConstructor m
                        , Ord (m v)
                        , Algebra k (m v)
                        , p ~ Vect k (m v) )


-- * Type synonyms for various types of maps
--
-- $TypeSynonymsForMaps
-- Of course, the following type synonyms do /not/ provide any type safty, e.g.
-- an 'AlgMor' is not guaranteed to preserve the algebra structure.
-- However, it makes type signatures self-contained and clearly states
-- the assumptions on the input.

-- | Short for a /set map/ of algebras.
type AlgMap k a b = (Lam k a -> Lam k b)
-- | Short for @LinearHomomorphism@, i.e. a graded @k@-homomorphism of
-- underlying graded modules of algebras (possibly of non-zero degree,
-- i.e. grade-shifting)
type LinHom k a b = AlgMap k a b
-- | Short for @AlgebraMorphism@, i.e. a morphism in the category of graded
-- algebras, i.e. a graded @k@-homomorphism of degree 0 that is compatible with
-- multiplication and unit.
type AlgMor k a b = LinHom k a b
-- | Short for a morphism of cochain algebras, i.e. an 'AlgMor' that commutes with the differentials
type DgaMor k a b = AlgMor k a b
-- | Short for a @k@-linear endomorphism of the underlying graded module of an algebra
type LinEndo k a = LinHom k a a
-- | Short for an endomorphism of a graded algebra
type AlgEndo k a = AlgMor k a a
-- | Short for an endomorphism of a cochain algebra
type DgaEndo k a = DgaMor k a a


-- * Constraint abbreviations
-- 
-- $ConstraintAbbreviations
-- Most functions need some 'Eq', 'Num' and 'Ord' instances.
-- The following synonyms are used for convenience.

type ENO  k a   = (Eq k, Num k, Ord a)
type ENOO k a b = (ENO k a, Ord b)
type ENO' k a   = (ENO k a, Ord k)

-- ** Row echelon reduction
-- 
-- $Elim
-- Additionally, computations in "Alg.Cohomology" use row echelon reduction of
-- matrices, for which we require the base ring @k@ to satisfy the following
-- type constraints. (\"Elim\" as in "Gaussian elimination".)
type Elim k = (FracClear k, LA.MatrixReprFor (UnFrac k), LA.MatrixEchelonFor (UnFrac k))
type Elim' k = (Elim k, Fractional k, LA.MatrixAccessFor (UnFrac k))


-- * Types for the cached operations

-- | A @'Cache' k a@ caches computational data for cochain algebras specified by
-- instances of @'DgaSpec' k a@.
data Cache k a = Cache
    { _genProp :: M.Map (Generators a) (GenPropCache k a)
    , _dgaProp :: M.Map (DgaSpec k a) (DgaPropCache k a)
    }
    deriving (GHC.Generics.Generic, Generics.SOP.Generic)

type DegMap x = M.Map Deg x
-- | Here, 'Basis' means: basis of some submodule.
type Basis k a = [Lam k a]
type Mat k = LA.Matrix (UnFrac k)

-- | Cached data that only depends on the algebra generators.
data GenPropCache k a = GenPropCache
    { _chainsBasis :: DegMap (Basis k a)
    }
    deriving (GHC.Generics.Generic, Generics.SOP.Generic)

-- | Cached data that also depends on the differental.
data DgaPropCache k a = DgaPropCache
    { _boundariesBasis :: DegMap (Basis k a)
    , _monomialsBdy    :: DegMap (S.Set (FGCA a))
    , _differentialUF  :: DegMap (LA.EchelonInfo (Mat k))  -- UF = UnFrac
    , _differentialFs  :: DegMap [UnFrac k]                -- Fs = factors
    }
    deriving (GHC.Generics.Generic, Generics.SOP.Generic)

type Cached k a x = State (Cache k a) x

data Key k a y = GenKey (PropKey GenPropCache k a y)
               | DgaKey (PropKey DgaPropCache k a y)
type PropKey t k a y = Lens' (t k a) (DegMap y)

{-# LANGUAGE ScopedTypeVariables, NamedFieldPuns #-}
{-# LANGUAGE ConstraintKinds, TypeFamilies       #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

{-# OPTIONS_HADDOCK prune #-}
-- The undocumented "…_unsafe" functions should only be
-- used by someone who looked at the implementation and
-- understands when their usage is appropriate.

{- |
Module      : Alg
Description : Algebra basics

Here we provide basic functionality for constructing
semi-free differential graded-commutative algebras,
i.e. free graded-commutative algebras with a differential
that satisfies the graded Leibniz rule.

As far as this library is concerned, we use the following
conventions and terminology:

* Semi-free differential graded-commutative algebras are called
  /cochain algebras/ and we also use the abbreviation /dga/.

    Note, that both terms often denote broader classes of objects in
    the literature, but since this library does not treat other than
    the mentioned objects, there should arise no confusion.

* Free graded-commutative algebras (without differential)
  will simply be called /algebras/. They are represented by a dga
  with zero differential, if necessary.

* Differentials raise the degree by 1, i.e. we use "upper grading".

* A /monomial/ is a product of algebra generators.
-}

module Alg where

import Prelude hiding ( (*>), (<*) )
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Control.Applicative (liftA2)
import Control.Monad (guard)
import Data.Traversable (mapAccumL)
import qualified Optics.Core as O
import Optics.Iso (Iso')
import Optics.Operators ((^.))

import Math.Algebras.VectorSpace
import Math.Algebras.Structures
import Math.Algebras.TensorAlgebra
import Math.Algebras.TensorProduct
import qualified Math.CommutativeAlgebra.Polynomial as Poly
import HFMext (coeffs, changeBaseRing)

import Types
import Lens
import Util


-- * Dga construction

-- | Make a 'DgaSpec' from a @[(generator, degree)]@ list,
-- together with the zero differential.
mkAlg :: ENO k a => [(a,Deg)] -> DgaSpec k a
mkAlg gs = DgaSpec { _gens = M.fromList gs
                   , _diff = const 0 }

-- | Make a 'DgaSpec' from a @[(generator, degree, image under differential)]@
-- list.
-- 
-- This function will throw an error if the given input does not
-- describe a valid differential.
mkDga :: ENO k a => [(a, Deg, Lam k a)] -> DgaSpec k a
mkDga = snd . mkDgaWithGenerators

-- | Like 'mkDga', but additionally, return the algebra generators
-- /in the same order/ as the input list.
-- 
-- Thanks to laziness, we can then use the following construct:
-- 
-- @
-- ([t,dt], interval) =
--     'mkDgaWithGenerators' [ ("t",0, dt)
--                         , ("dt",1, 0) ]
-- @
-- 
-- In case of a lot of manually typed generators,
-- 'mkDgaWithGeneratorsParanoid' is an even safer variant.
mkDgaWithGenerators :: ENO k a
                    => [(a, Deg, Lam k a)]
                    -> ([Lam k a], DgaSpec k a)
mkDgaWithGenerators l = (gens, dga)
  where
    sth  = mkDga_unsafe l
    gens = fmap (\ (x,_,_) -> injectFGCA'' sth x) l
    dga  = if isDifferential sth (_diff sth)
           then sth else error "mkDga(WithGenerators): invalid differential"

-- | Similar to 'mkDgaWithGenerators', but the input list must be of the form
-- @[(generator, generator as an algebra element, degree, image under differential)]@
-- (see also 'algGenerators' and 'injectFGCA'').
-- 
-- We can then write
-- 
-- @
-- ([t,dt], interval) =
--     'mkDgaWithGeneratorsParanoid' [ ("t",t, 0, dt)
--                                 , ("dt",dt, 1, 0) ]
-- @
-- 
-- and it is checked that @t@ really corresponds to the generator @"t"@.
-- If we had accidentally written @([__dt,t__], interval) = …@, an error
-- would have been thrown.
mkDgaWithGeneratorsParanoid :: ENO k a
                            => [(a, Lam k a, Deg, Lam k a)]
                            -> ([Lam k a], DgaSpec k a)
mkDgaWithGeneratorsParanoid l = (gens, if ok then dga else err)
  where
    (gens,dga) = mkDgaWithGenerators $ fmap (\ (a,b,c,d) -> (a,c,d)) l
    gens_input = fmap (\ (a,b,c,d) -> b) l
    ok         = gens_input == gens
    err        = error "mkDgaWithGeneratorsParanoid: generators don't match"

mkDga_unsafe :: ENO k a => [(a, Deg, Lam k a)] -> DgaSpec k a
mkDga_unsafe l =
    (M.fromList $ fmap (\ (x,y,z) -> (x,(y,z))) l) ^. O.re finiteRepr

-- | Isomorphism (in terms of the @optics@ package) between 'DgaSpec' and
-- a finite representation that can be used e.g. for 'Eq' and 'Ord' instances.
finiteRepr :: ENO k a => Iso' (DgaSpec k a) (M.Map a (Deg, Lam k a))
finiteRepr = O.iso toFin fromFin
  where
    toFin dga = let d = _diff' dga
                in M.mapWithKey (\ z deg -> (deg, d z)) (dga^.gens)
    fromFin m = DgaSpec { _gens = M.map fst m
                        , _diff = extendToDiff (M.map snd m M.!) }


-- * Generators of an algebra

-- | Given an element of type @a@, return the corresponding algebra generator.
-- Returns 'Nothing' if the element is not a generator in the given 'DgaSpec'.
-- 
-- Mathematical analogy: If \(M\) is a free module on the basis \(X\), this
-- function corresponds to the canonical injection
-- \( X \hookrightarrow M \hookrightarrow \wedge(M) \).
injectFGCA' :: ENO k a => DgaSpec k a -> a -> Maybe (Lam k a)
injectFGCA' = flip M.lookup . algGenerators

-- | The same as 'injectFGCA'', but generates an error instead of returning
-- 'Nothing'.
injectFGCA'' :: ENO k a => DgaSpec k a -> a -> Lam k a
injectFGCA'' = maybe err id .: injectFGCA'
    where
        err = error "injectFGCA'': trying to inject a non-generator"

-- | Algebra generators of a 'DgaSpec' in the form a 'M.Map' with keys the
-- generators as elements of @a@ and values the corresponding algebra element.
algGenerators :: ENO k a
              => DgaSpec k a -> M.Map a (Lam k a)
algGenerators = polymorphic_generators . _gens

-- | Algebra generators of a 'DgaSpec' as a list.
algGenerators' :: ENO k a
               => DgaSpec k a -> [Lam k a]
algGenerators' = M.elems . algGenerators

-- | The same as 'algGenerators' for any 'DgaSpec' with the given 'Generators'.
polymorphic_generators :: ENO k a
                       => Generators a -> M.Map a (Lam k a)
polymorphic_generators = M.mapWithKey injectGen
    where
        injectGen z i = let lr = if even i then Left else Right
                         in injectFGCA_unsafe (lr z)

injectFGCA_unsafe :: ENO k a => Either a a -> Lam k a
injectFGCA_unsafe (Left x)  = injectSym' x `te` (unit 1)
injectFGCA_unsafe (Right y) = (unit 1) `te` injectExt' y

-- | For defining dgas algorithmically, in particular for speciying the
-- differential in 'mkDga', it is often necessary to have access
-- to the generators as algebra elements in advance.
-- 
-- The functions 'injectFGCA_even' and 'injectFGCA_odd' make this possible,
-- but it must be known whether the generator will have even or odd degree.
-- 
-- Note, that this is a potentially unsafe operation: the parity used here
-- __must__ match the parity of the degree that is later specified in 'mkDga'
-- (or otherwise constructed @'Generators' a@).
injectFGCA_even :: ENO k a => a -> Lam k a
injectFGCA_even = injectFGCA_unsafe . Left

-- | See 'injectFGCA_even'.
injectFGCA_odd :: ENO k a => a -> Lam k a
injectFGCA_odd = injectFGCA_unsafe . Right

-- | Convenience function for applying the differential of a 'DgaSpec'
-- directly to an element of type @a@. If the latter is not an algebra
-- generator, an error is generated.
_diff' :: ENO k a => DgaSpec k a -> a -> Lam k a
_diff' dga = _diff dga . injectFGCA'' dga


-- * Degree

-- | Degree of a monomial: the degree of \(x_1\cdots x_r\)
-- is the sum of the degrees of the \(x_j\).
homDegree :: (Ord a) => DgaSpec k a -> FGCA a -> Deg
homDegree (DgaSpec{_gens}) (Sym _ xs, Ext _ ys) = s xs + s ys
    where
        s = sum . map (_gens M.!)

-- | Degree of an arbitrary algebra element. Returns 'Nothing' if
-- the given element is not homogeneous.
degree :: (Ord a) => DgaSpec k a -> Lam k a -> Maybe Deg
degree _  (V []) = Nothing
degree alg (V (((z,_):zs))) =
    if all sameDeg zs then Just k else Nothing
        where
            k = homDegree alg z
            sameDeg = ((== k) . homDegree alg . fst)

degree_unsafe :: (Ord a) => DgaSpec k a -> Lam k a -> Deg
degree_unsafe _   (V [])        = 0
degree_unsafe alg (V ((z,_):_)) = homDegree alg z

-- | Compute a @k@-basis of monomials for the given /connected/ algebra
-- in the given degree.
--
-- Connectedness means, that the input algebra __must not__ have generators
-- of degree 0.
--
-- (The cached variant of this function is 'Alg.Cohomology.findBasis'.)
findBasis_ :: ENO k a => DgaSpec k a -> Deg -> Basis k a
findBasis_ alg n = [product $ zipWith (^) gs' p | p <- ps]
    where
        gs' = algGenerators' alg
        ps  = solveDegreeEquation (M.elems . _gens $ alg) n


-- * Maps of algebras

-- | Universal property of free graded-commutative algebras:
-- Given a map on generators, it uniquely extends to a morphism
-- of algebras.
-- 
-- Note, that it depends on 'DgaSpec's whether the resulting 'AlgMor'
-- respects the grading. The function 'isMapOfDeg' can be used
-- to check this.
--
-- It is valid to pass a partial function as long as it is
-- defined on all actually used algebra generators.
extendToAlg :: forall k a b. (ENOO k a b)
          => (a -> Lam k b) -> AlgMor k a b
extendToAlg f0 = linear f1
    where
        f1 :: FGCA a -> Lam k b
        f1 g = case decompose g of
                 Nothing       -> 1
                 Just (lr, g') -> f0 (unpack lr) * f1 g'
        unpack = either id id

-- | Extend a map on generators to a differential.
-- By the Leibniz rule, this extension is unique.
--
-- Note, that it depends on a 'DgaSpec' whether the resulting
-- 'Differential' is sound (i.e. whether it respects the grading and
-- composition with itself is zero). The function 'isDifferential'
-- can be used to check this.
--
-- It is valid to pass a partial function as long as it is
-- defined on all actually used algebra generators.
extendToDiff :: forall k a. ENO k a
           => (a -> Lam k a) -> Differential k a
extendToDiff d0 = linear d1
    where
        d1 :: FGCA a -> Lam k a
        d1 g = case decompose g of
                 Nothing       -> zerov
                 Just (lr, g') -> d0 (unpack lr) * pure g'
                                  + sign lr *> (injectFGCA_unsafe lr * d1 g')
        unpack = either id id
        sign = either (const 1) (const (-1))

-- | Check if a given given 'LinHom' is graded of a specified degree.
-- 
-- For example, given 'DgaSpec's @algA@ and @algB@, use
-- "@'isMapOfDeg' algA algB 0 …@ " to check if a 'LinHom' is
-- of degree 0, i.e. a morphism of graded modules.
isMapOfDeg :: ENOO k a b
           => DgaSpec k a -> DgaSpec k b -> Deg -> LinHom k a b -> Bool
isMapOfDeg algA algB k f =
    all mapsToRightDeg (algGenerators algA)
        where
            mapsToRightDeg z = let fz = f z
                                in fz == 0 || cmpDeg z fz
            cmpDeg z z' = maybe False id $
                liftA2 (==) ((k+) <$> degree algA z) (degree algB z')

-- | Check if a given 'LinEndo' is a differential for the given algebra,
-- i.e. if it raises degree by 1 and composition with itself is zero.
-- 
-- (The differential of the given 'DgaSpec' is ignored.)
isDifferential :: ENO k a
               => DgaSpec k a -> LinEndo k a -> Bool
isDifferential alg f = isMapOfDeg alg alg 1 f &&
    all ((== 0) . f . f) (algGenerators alg)


-- * Miscellaneous

-- Proposition 38.3 in: Félix, Halpering, Thomas. "Rational Homotopy Theory". Springer, 2001
-- | The formal dimension of an elliptic Sullivan algebra.
formalDimension :: (Ord a) => DgaSpec k a -> Int
formalDimension = M.foldl' accum 0 . _gens
    where
        accum s k | even k    = s - (k-1)
                  | otherwise = s + k

-- | For a @k@-algebra @p@, extend @'DgaSpec' k a@ to @'DgaSpec' p a@.
extendScalars :: (ENOO k a b, Algebra k b, p ~ Vect k b)
              => DgaSpec k a -> DgaSpec p a
extendScalars dga =
    DgaSpec { _gens = _gens dga
            , _diff = extendToDiff
                (changeBaseRing unit . _diff dga . injectFGCA'' dga)
            }

-- | Decompose a monomial
-- \( x_1\cdot x_2 \cdots x_r \)
-- into a pair, consisting of \(x_1\) and \(x_2\cdots x_r\).
-- 
-- Returns 'Nothing' on the empty product,
-- @('Left' x_1, ...)@ if \(x_1\) is of even degree, and
-- @('Right' x_1, ...)@ if \(x_2\) is of odd degree.
decompose :: FGCA a
          -> Maybe (Either a a, FGCA a)
decompose ((Sym i (x:xs)), e) = Just (Left x,  (Sym (i-1) xs, e))
decompose (s, (Ext j (y:ys))) = Just (Right y, (s, Ext (j-1) ys))
decompose (_, _)              = Nothing

{-# LANGUAGE ExtendedDefaultRules, DerivingVia, OverloadedStrings, StandaloneDeriving, TupleSections, TypeApplications #-}

-- |
-- Module      : Extern.Sage
-- Description : Interface to Sage functions
-- 
-- This module provides an interface to some functionality of
-- [SageMath](https://www.sagemath.org/). More specifically,
-- we use the module for "[Ideals in multivariate polynomial
-- rings](https://doc.sagemath.org/html/en/reference/polynomial_rings/sage/rings/polynomial/multi_polynomial_ideal.html)".
-- 
-- An accompanying Python module
-- @pybits\/Extern\/sageglue.py@
-- is responsible for calling the actual sage functions.
-- We use the
-- [@cpython@ package](https://hackage.haskell.org/package/cpython)
-- to call this Python module directly from Haskell.
-- 
-- == Usage notes
-- Everything in this module happens over the base field
-- \(\mathbb{Q}\), i.e. the rational numbers.
-- 
-- For simplicity, this interface imposes the following restrictions:
--
-- * A 'Polynomial' in the sense of this interface has a fixed
--   variable type of @'VarNum' = Int@ and by convention, we use only
--   non-negative variables.
-- * No variable gaps are possible: if the variable @2 :: VarNum@
--   is used in some polynomial, we automatically work over a
--   polynomial ring in three variables, namely @0@, @1@, and @2@,
--   whether @0@ and @1@ are used or not.
--   (See also 'numberOfVariables'.)
-- 
-- This means that, in order to use this interface, you have to
-- enumerate your variables by @0,1,…@ and convert your polynomials to
-- 'Polynomial's first.
-- In the case of a @newtype@ around @Int@ as variable type
-- (as used in the @interactive\/@ examples)
-- the conversion is easily handled by 'Data.Coerce.coerce'.

-- TODO: mention that getModule is only present in our fork of
-- the @cpython@ package.

-- TODO: add note somewhere, that marshalling of polynomials
-- between Haskell and Python is /not/ symmetric

module Extern.Sage
    (
    -- * Initialization
      initialize
    -- * Exposed Interface
    , VarNum
    , Polynomial
    , IdealGenerators
    , idealDimension
    , idealDimension'
    , hasFiniteProjection
    , hasFiniteProjection'
    , eliminationIdeal
    , eliminationIdeal'
    , numberOfVariables
    -- * Python exceptions
    , onPyException
    , printPyException
    )
  where


import           Prelude hiding (Integer, toInteger, (*>), (<*))
import qualified Prelude as HS (Int, Integer, toInteger)

import Control.Monad (void, (>=>))
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import System.FilePath (takeDirectory)
import Paths_inflexible (getDataFileName)

import Math.Algebras.VectorSpace (Vect(V), (<*))
import Math.Core.Field (Q(..), numeratorQ, denominatorQ)
import Math.CommutativeAlgebra.Polynomial (GrevlexPoly, Grevlex(Grevlex), MonImpl(M))
import qualified Math.CommutativeAlgebra.Polynomial as Poly

import qualified CPython            as Py  (initialize)
import qualified CPython.Simple     as PyS (arg, call, getAttribute, setAttribute, ToPy(toPy), FromPy(fromPy))
import           CPython.Simple            (ToPy(), FromPy())
import qualified CPython.Types      as Py

import qualified CPython.Protocols.Object   as Py    (callArgs, cast, Object(toObject), SomeObject)
import qualified CPython.Protocols.Object   as PyObj (getAttribute)
import qualified CPython.Protocols.Sequence as PySeq (toTuple)
import qualified CPython.Types.Dictionary as PyDict (mergeFromSeq2, new)
import qualified CPython.Types.List       as Py     (fromList, iterableToList)
import qualified CPython.Types.Module     as Py     (getModule, importModule)

import qualified Control.Exception        as E  (handle)
import qualified CPython.Types.Exception  as Py (exceptionTraceback, exceptionType, exceptionValue)
import qualified CPython.Protocols.Object as Py (print)
import           System.IO                      (hPutStr, stderr)

import Util ((<&>))

default (Text, String)

-- ----------------------------
-- Python setup and marshalling

instance ToPy HS.Int where
    toPy = PyS.toPy . HS.toInteger

instance ToPy Py.SomeObject where
    toPy = return

newtype PyObjWrapper o = PyObjWrapper { unwrapPyObj :: o }
instance (Py.Object o) => ToPy (PyObjWrapper o) where
    toPy = return . Py.toObject . unwrapPyObj

deriving via (PyObjWrapper Py.Dictionary) instance ToPy Py.Dictionary
deriving via (PyObjWrapper Py.Integer   ) instance ToPy Py.Integer
deriving via (PyObjWrapper Py.List      ) instance ToPy Py.List
deriving via (PyObjWrapper Py.Tuple     ) instance ToPy Py.Tuple


pyModuleName :: Text
pyModuleName = "sageglue"

amendPythonsPath :: IO ()
amendPythonsPath = do
    pypath <- PyS.getAttribute "sys" "path" :: IO [Text]
    file   <- getDataFileName ("pybits/Extern/" <> T.unpack pyModuleName <> ".py")
    let dir = takeDirectory file
    PyS.setAttribute "sys" "path" (T.pack dir : pypath)

-- | Initialize the Python and Sage bits.
-- __Calling this is mandatory before using the interface below__.
initialize :: IO ()
initialize = E.handle onPyException $ do
    Py.initialize
    amendPythonsPath
    void $ Py.importModule pyModuleName

-- | An exception handler for Python exceptions,
-- printing type, value, and traceback (if available)
-- to standard error.
onPyException :: Py.Exception -> IO ()
onPyException exc = do
    put_err "Python exception of type: "
    py_print_err (Py.exceptionType exc)
    put_err "with value: "
    py_print_err (Py.exceptionValue exc)
    case Py.exceptionTraceback exc of
        Nothing -> return ()
        Just tb -> do
            put_err "and traceback:\n"
            PyS.call "traceback" "print_tb" [PyS.arg tb] []
  where
    put_err = hPutStr stderr
    py_print_err = flip Py.print stderr

-- | This function is useful for debugging a Python exception:
-- When you get a non-informative
-- 
-- > *** Exception: <CPython exception>
--
-- message, feeding the same action to this function will make Python
-- print more details about the exception.
printPyException :: IO a -> IO ()
printPyException io = E.handle onPyException (io >> return ())

getPyModule :: IO Py.Module
getPyModule = Py.getModule pyModuleName

getPyAttribtute :: Text -> IO Py.SomeObject
getPyAttribtute name = do
    module_ <- getPyModule
    PyObj.getAttribute module_ =<< Py.toUnicode name

-- | No check whether the requested attribute is callable is made.
getPyFunction :: Text -> IO Py.SomeObject
getPyFunction = getPyAttribtute

-- --------------------------------------------
-- Haskell to Python marshalling of polynomials

type VarNum     = Int
type Exponent   = Int
-- | Polynomials as used by this interface, based on the module
-- "Math.CommutativeAlgebra.Polynomial" from the @HaskellForMaths@
-- package.
type Polynomial = GrevlexPoly Q VarNum
type Monom      = Grevlex VarNum

monomsWithCoeff :: Polynomial -> [(Monom,Q)]
monomsWithCoeff (V monoms) = monoms

-- | /Note:/ this function assumes that in the monom's internal representation
-- the variables appear in ascending order (or rather that @mindices@ returns
-- them in such a way). As far as i can see, this is guaranteed if one
-- constructs only variables directly and then builds monomials via arithmetic
-- operations. However, as the monomial constructor is exposed, care must be
-- taken not to construct monomials violating the above condition.
--
monomExponents :: Monom -> [Exponent]
monomExponents = go 0 . Poly.mindices
  where
    go :: VarNum -> [(VarNum,Exponent)] -> [Exponent]
    go _ [] = []
    go pos all@((var,exp):rest)
        | pos == var  = exp : go (pos+1) rest
        | otherwise   = 0   : go (pos+1) all

monomToPython :: Int -- ^ total number of desired variables
              -> Monom
              -> IO Py.Tuple
monomToPython nvar monom = do
    let exps = monomExponents monom
    exp_args <- mapM PyS.toPy exps
    py_zero  <- PyS.toPy @HS.Integer 0
    Py.toTuple (exp_args ++ replicate (nvar - length exps) py_zero)

polyToPython :: Int -- ^ total number of desired variables
             -> Polynomial
             -> IO Py.Dictionary
polyToPython nvar p = do
    p' <- PyS.toPy =<< mapM to_py_repr (monomsWithCoeff p)
    dict <- PyDict.new
    PyDict.mergeFromSeq2 dict p' True
    return dict
  where
    to_py_repr :: (Monom,Q) -> IO (Py.Tuple, (HS.Integer,HS.Integer))
    to_py_repr (m,q) = do
        m' <- monomToPython nvar m
        return (m',(numeratorQ q, denominatorQ q))

-- --------------------------------------------
-- Python to Haskell marshalling of polynomials

iterableToHsList :: Py.Object iter => iter -> IO [Py.SomeObject]
iterableToHsList = Py.iterableToList >=> Py.fromList

instance FromPy Py.SomeObject where
    fromPy = return
instance FromPy Int where
    fromPy = fmap fromIntegral . (PyS.fromPy @HS.Integer)

-- | Caveat: In addition to the expected form, we require that 'VarNum'
-- appear in increasing order. (This seems to be guaranteed by the
-- implementation of the 'sparse_iter' method of Sage's ETuple, yet
-- it is not documented.)
py2hsMonom :: Py.SomeObject  -- ^ expected form: tuple[Degree=int, Iterable[tuple[VarNum=int,Exponent=int]]]
           -> IO Monom
py2hsMonom o = do
    (degree, o') <- PyS.fromPy @(Int, Py.SomeObject) o
    repr <- iterableToHsList o' >>= mapM PyS.fromPy :: IO [(VarNum,Exponent)]
    return $ Grevlex (M degree repr)

py2hsPoly :: Py.SomeObject  -- ^ expected form: Iterable[tuple[Py2HsMonom,Coeff=(Int,Int)]]
          -> IO Polynomial
py2hsPoly o = do
    repr <- iterableToHsList o >>=
        mapM PyS.fromPy :: IO [(Py.SomeObject,(HS.Integer,HS.Integer))]
    summands <- mapM (fmap mk_poly . to_hs_repr) repr
    return $ sum summands
  where
    to_hs_repr :: (Py.SomeObject, (HS.Integer,HS.Integer)) -> IO (Monom, Q)
    to_hs_repr (o, (nomi,denom)) =
        py2hsMonom o <&> (, Q (nomi % denom))
    mk_poly :: (Monom, Q) -> Polynomial
    mk_poly (m,c) = pure m <* c


-- ---------------------------------------------
-- The interface that we export from this module

type IdealGenerators = [Polynomial]


-- | Dimension of the generated ideal within a polynomial ring
-- with specified number of variables.
idealDimension :: Int  -- ^ total number of variables
               -> IdealGenerators
               -> IO HS.Integer
idealDimension nvar ps = PyS.fromPy =<< do
    py_func   <- getPyFunction "idealDimension"
    arg_nvar  <- PyS.toPy nvar
    arg_polys <- PyS.toPy =<< mapM (polyToPython nvar) ps
    Py.callArgs py_func [arg_nvar, arg_polys]

-- | Convenience function that calls 'idealDimension'
-- with the number of variables as computed by
-- 'numberOfVariables'.
idealDimension' :: IdealGenerators -> IO HS.Integer
idealDimension' ps = idealDimension nvars ps
  where
    nvars = numberOfVariables ps


-- | Let @V@ be the zero locus of the given ideal (generators)
-- in @n@-dimensional affine space, where @n@ is the first argument.
-- Then this function computes whether the projection of @V@ onto
-- the affine line that is given by the third argument has a finite
-- image.
hasFiniteProjection :: Int  -- ^ total number of variables
                    -> IdealGenerators
                    -> VarNum  -- ^ projection target
                    -> IO Bool
hasFiniteProjection nvar ps var_id = PyS.fromPy =<< do
    py_func   <- getPyFunction "hasFiniteProjection"
    arg_nvar  <- PyS.toPy nvar
    arg_polys <- PyS.toPy =<< mapM (polyToPython nvar) ps
    arg_varid <- PyS.toPy var_id
    Py.callArgs py_func [arg_nvar, arg_polys, arg_varid]

-- | Convenience function that calls 'hasFiniteProjection'
-- with the number of variables as computed by
-- 'numberOfVariables'.
hasFiniteProjection' :: IdealGenerators -> VarNum -> IO Bool
hasFiniteProjection' ps var_id = hasFiniteProjection nvars ps var_id
  where
    nvars = numberOfVariables ps


-- | Consider the polynomial ring in a given number of variables
-- and let @I@ be the ideal given by the specified generators.
-- This function computes the elimination ideal resulting from
-- eliminating the given variables from @I@.
eliminationIdeal :: Int  -- ^ total number of variables
                 -> IdealGenerators
                 -> [VarNum]  -- ^ variables to eliminate
                 -> IO IdealGenerators
eliminationIdeal nvar ps vs = do
    py_func    <- getPyFunction "eliminationIdealGens"
    arg_nvar   <- PyS.toPy nvar
    arg_polys  <- PyS.toPy =<< mapM (polyToPython nvar) ps
    arg_varids <- PyS.toPy =<< mapM PyS.toPy vs
    py_gens_i  <- Py.callArgs py_func [arg_nvar, arg_polys, arg_varids]
    gens       <- mapM py2hsPoly =<< iterableToHsList py_gens_i
    return gens

-- | Convenience function that calls 'eliminationIdeal'
-- with the number of variables as computed by
-- 'numberOfVariables'.
eliminationIdeal' :: IdealGenerators
                  -> [VarNum]  -- ^ variables to eliminate
                  -> IO IdealGenerators
eliminationIdeal' ps vs = eliminationIdeal nvars ps vs
  where
    nvars = numberOfVariables ps

-- | Computes the number of variables "used" in the given polynomials.
-- Here, a variable @j@ counts as used if there exists a variable @k@
-- in the given polynomials with @j ≤ k@. In other words, it simply
-- returns @1 + maximal variable that actually appears@.
numberOfVariables :: [Polynomial] -> Int
numberOfVariables ps =
    let vars = [v |     p <- ps
                  , (m,_) <- monomsWithCoeff p
                  , (v,_) <- Poly.mindices m]
    in case vars of
        [] -> 0
        vs -> 1 + maximum vs

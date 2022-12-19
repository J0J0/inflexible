{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Cache
-- Description : Caching of recurring computations
-- 
-- This module provides a simple cache framework for storing and retrieving
-- results of functions that expect a 'DgaSpec' and a particular degree
-- as input. For 'Key's we use the lens from the "Lens" module, which
-- facilitates easy access to the values.

module Cache where

import Control.Monad.State
import qualified Data.Map.Lazy as M
import qualified Generic.Data (gmappend, gmempty)
import qualified Optics.Core as O
import Optics.Core ((%))
import Optics.Operators ((^.))

import Types
import Lens
import Alg.Classes ()
import Util ((.:))


instance Semigroup (GenPropCache k a) where
    (<>)   = Generic.Data.gmappend
instance Semigroup (DgaPropCache k a) where
    (<>)   = Generic.Data.gmappend
instance Monoid    (GenPropCache k a) where
    mempty = Generic.Data.gmempty
instance Monoid    (DgaPropCache k a) where
    mempty = Generic.Data.gmempty


cacheResult :: ENO' k a
            => Key k a y
            -> (DgaSpec k a -> Deg -> Cached k a y)
            -> (DgaSpec k a -> Deg -> Cached k a y)
cacheResult key f = \ dga deg ->
    viewCached key dga deg >>= \case
        Just val -> return val
        Nothing  -> do
            val <- f dga deg
            setCached key dga deg val
            return val

cachePureResult :: ENO' k a
                => Key k a y
                -> (DgaSpec k a -> Deg -> y)
                -> (DgaSpec k a -> Deg -> Cached k a y)
cachePureResult key f = cacheResult key (pure .: f)

viewCache :: ENO' k a => Key k a y -> DgaSpec k a -> Deg -> Cache k a -> Maybe y
viewCache key dga deg = case key of
    GenKey l -> mkView genProp (dga^.gens) l
    DgaKey l -> mkView dgaProp dga l
  where
    mkView l1 x l2 = O.preview (l1 % at' x % l2 % at' deg)

setCache :: ENO' k a => Key k a y -> DgaSpec k a -> Deg -> y -> Cache k a -> Cache k a
setCache key dga deg = case key of
    GenKey l -> mkSetter genProp (dga^.gens) l
    DgaKey l -> mkSetter dgaProp dga l
  where
    mkSetter l1 x l2 = deepSetDef mempty (l1 % at x) (l2 % at deg) . Just

viewCached :: ENO' k a => Key k a y -> DgaSpec k a -> Deg -> Cached k a (Maybe y)
viewCached key dga deg = gets (viewCache key dga deg)

setCached :: ENO' k a => Key k a y -> DgaSpec k a -> Deg -> y -> Cached k a ()
setCached key dga deg val = modify (setCache key dga deg val)


viewCachedErr :: ENO' k a => String -> Key k a y -> DgaSpec k a -> Deg -> Cached k a y
viewCachedErr errstr key dga deg =
    gets (viewCache key dga deg) >>= \case
        Just y  -> return y
        Nothing -> error errstr


dropCache :: Cached k a y -> y
dropCache cval = evalState cval emptyCache

emptyCache :: Cache k a
emptyCache = Cache M.empty M.empty

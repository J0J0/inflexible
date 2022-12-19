{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# OPTIONS_HADDOCK prune #-}
-- (Unfortunately, the lens definitions in this module
-- are not picked up by Haddock correctly.)

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}


-- |
-- Module      : Lens
-- Description : Lens for caches (and 'DgaSpec')
-- 
-- This module provides 'Lens'' in the sense of the @optics@ package family
-- (mainly) for the "Cache" machinery.
-- 
-- Lens are generically derived via the packages @generics-sop@ and @optics-sop@
-- for the record fields of the types 'Cache', 'GenPropCache', 'DgaPropCache',
-- and 'DgaSpec'; following the usual convention that the lenses' names are the
-- same as the record field names without the initial underscore.

module Lens where

import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP
import qualified Optics.Core  as O
import qualified Optics.SOP   as O (mkLenses, LensesFor(Lenses))

import           Control.Applicative (Alternative, (<|>))
import qualified Data.Map.Lazy as Map
import           Data.Map.Lazy (Map)

import Types


O.Lenses (gens, diff) =
    O.mkLenses :: O.LensesFor (DgaSpec k a)

O.Lenses (genProp, dgaProp) =
    O.mkLenses :: O.LensesFor (Cache k a)

O.Lenses (chainsBasis) =
    O.mkLenses :: O.LensesFor (GenPropCache k a)

O.Lenses ( boundariesBasis
         , monomialsBdy
         , differentialUF
         , differentialFs
         ) =
    O.mkLenses :: O.LensesFor (DgaPropCache k a)


at :: (Ord k) => k -> O.Lens' (Map k a) (Maybe a)
at = O.at

-- note: the following differs from O.at', which is a strict O.at version!
at' :: (Ord k) => k -> O.AffineTraversal' (Map k a) a
at' k = at k O.% O._Just

altDef :: Alternative f => a -> f a -> f a
altDef a x = x <|> pure a

-- | Given
-- 
-- @
-- o1 :: 'O.Setter'' s (f a)
-- @
-- 
-- and
-- 
-- @
-- o2 :: 'O.Setter'' a x
-- @
-- 
-- where @f@ is an 'Alternative' and we have some default value for @a@,
-- combine @o1@ and @o2@ in the following way:
-- if @o1@'s target is empty, replace it by @pure@ the given default;
-- then use @o2@ to set the inner target.
deepSetDef :: (Alternative f, O.Is k O.A_Setter, O.Is l O.A_Setter)
     => a -> O.Optic' k is s (f a) -> O.Optic' l is' a x -> x -> s -> s
deepSetDef empty o1 o2 = pseudo_setter . const
  where
    pseudo_setter f = O.over o1 (fmap (O.over o2 f) . altDef empty)
    -- Note that @O.sets pseudo_setter@ is NOT valid, see below.


-- -- FIXME: The result of the following (|%) combinator is generally
-- --        NOT a well-formed Setter'! :(
-- --        It violates functoriality: usually @O.over … id ≠ id@
-- --        as passing an overall empty input shows.
-- 
-- -- | Compose two setters with targets in an 'Alternative' such
-- -- that the lack of an inner structure is compensated by an (empty)
-- -- default.
-- -- 
-- -- An example (and our sole use case) might be helpful.
-- -- 
-- -- > (|%) :: Monoid a => Setter' s (Maybe a) -> Setter' a (Maybe x) -> Setter' s (Maybe x)
-- --
-- -- works as follows: if the first setter targets a @Nothing@,
-- -- the latter gets "replaced" by @Just mempty :: Maybe a@ (over which
-- -- the second setter can then be mapped).
-- (|%) :: (Alternative f, Monoid a, O.Is k O.A_Setter, O.Is l O.A_Setter)
--      => O.Optic' k is s (f a) -> O.Optic' l is' a (f x) -> O.Setter' s (f x)
-- o1 |% o2 = O.sets $ \ f -> O.over o1 (fmap (O.over o2 f) . altDef mempty)
-- 
-- infixl 8 |%

{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

-- |
-- Module      : FracClear
-- Description : Abstract interface for clearing fractions
-- 
-- See 'FracClear' type class.
-- 
-- /Background:/ This module exists, because exact calculations
-- with @'Ratio' k@ elements can be terribly slow, possibly
-- in contrast to direct computations with elements of @k@.
-- So if, for a particular task, denominators can be "put outside
-- of the calculation", using 'FracClear' might give a performance
-- increase.

module FracClear where

import Data.Ratio (Ratio, (%), numerator, denominator)
import GHC.Real (Ratio((:%)))

import Math.Core.Field (Q(..), numeratorQ, denominatorQ)


-- | Making a commutative ring @k@ an instance of 'FracClear'
-- encapsules the abstract concept of "clearing fractions".
-- 
-- Note, that this does /not/ require the presence of actual
-- fractions: for example, the integers also fit this concept
-- (with all "denominators" equal to @1@).
-- In this sense, the terminology "fraction", "numerator" and
-- "denominator" is used abstractly here.
class (Num k, Num (UnFrac k)) => FracClear k where
    -- | @'UnFrac' k@ is the type of numerator and denominator
    -- of an element of @k@.
    type UnFrac k
    -- | Inclusion of @'UnFrac' k@ into @k@, usually by putting
    -- @1@ as denominator.
    fromUnFrac :: UnFrac k -> k
    
    -- | Split an element into numerator and denominator, i.e.
    -- we want
    -- 
    -- @
    --   let (x,y) = 'unfrac' val
    --    in val * y == x
    -- @
    -- 
    -- to be @True@.
    unfrac :: k
           -> (UnFrac k, UnFrac k) -- ^ (numerator, denominator)
    
    -- | Given a list of elements, clear all denominators
    -- simultaneously, i.e. we want
    --
    -- @
    --   let ([xs],y) = 'unfracs' vals
    --    in 'map' (* y) vals == xs
    -- @
    -- 
    -- to be @True@.
    -- 
    -- A default implementation in terms of 'unfrac' is provided
    -- – however, a better method might be available. For example,
    -- for rational numbers, the least common multiple of all present
    -- denominators provides a suitable @y@.
    unfracs :: [k] -> ([UnFrac k], UnFrac k)
    unfracs xs = (zipWith (*) coeffs rs, sall)
        where
            (rs, ss)      = unzip $ map unfrac xs
            (sall:coeffs) = prods 1 (1:ss)
            
            -- prods t [y1,y2,..] == map (t*) [y2*y3*.., y1*y3*..]
            prods y' (y:ys) = (y' * product ys) : prods (y'*y) ys
            prods _  []     = []

instance (Integral i) => FracClear (Ratio i) where
    type UnFrac (Ratio i) = i
    fromUnFrac = (:% 1)
    unfrac x   = (numerator x, denominator x)
    unfracs [] = ([], 1)
    unfracs xs = (xs', s)
        where
            s   = foldr1 lcm $ map denominator xs
            xs' = map (\ x -> (s `quot` denominator x) * numerator x) xs

instance FracClear Q where
    type UnFrac Q = Integer
    fromUnFrac = Q . fromUnFrac
    unfrac x   = (numeratorQ x, denominatorQ x)
    unfracs    = unfracs . map (\ (Q x) -> x)

instance FracClear Int where
    type UnFrac Int = Int
    fromUnFrac = id
    unfrac x   = (x, 1)
    unfracs xs = (xs, 1)

instance FracClear Integer where
    type UnFrac Integer = Integer
    fromUnFrac = id
    unfrac x   = (x, 1)
    unfracs xs = (xs, 1)

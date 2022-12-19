-- |
-- Module      : Util
-- Description : Generic utility functions

module Util
    (   (.:)
    ,   (<&>)
    ,   allSatisfy
    ,   solveDegreeEquation
    ,   multiAt
    ,   toSupscript
    ,   allM
    ) where

import Data.Char (chr)
import Data.List (genericIndex)

-- | Composition with "two arguments":
-- @(f .: g) x y = f (g x y)@.
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)

-- | Flipped '(<$>)'.
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

-- | Flipped 'all' for infix use.
allSatisfy :: Foldable t => t a -> (a -> Bool) -> Bool
allSatisfy = flip all

-- | A naive implementation to solve the following Diophantine problem:
-- Given positive integers \(d_1,\ldots,d_r ,\, n\),
-- find all solutions \(x \in(\mathbb{Z}_{\geq 0})^r\) to the equation
-- \[ \sum_{j=1}^r d_j\cdot x_j = n \]
-- with the additional property that \(x_j\in\{0,1\}\) if \(d_j\) is odd.
solveDegreeEquation :: (Integral i)
    => [i]   -- ^ input coefficients
    -> i     -- ^ target value
    -> [[i]] -- ^ solutions
solveDegreeEquation ds n =
    filter ((== n) . sum . zipWith (*) ds) xss
        where
            xss = mapM (enumFromTo 0) bs
            bs = map calcBound ds
            calcBound k
              | k <= 0    = error "solveDegreeEquation: input coefficients must be positive"
              | even k    = n `div` k
              | otherwise = if k <= n then 1 else 0

-- | Like 'genericIndex' but for many indices.
multiAt :: (Integral i) => [a] -> [i] -> [a]
multiAt xs = map (xs `genericIndex`)

-- | Transform digits to supscript characters.
-- Produces an error on non-digit input. Example:
-- 
-- > map toSupscript "42" = "⁴²"
toSupscript :: Char -> Char
toSupscript '0' = chr 0x2070 -- '⁰'
toSupscript '1' = chr 0x00B9 -- '¹' (part of latin1)
toSupscript '2' = chr 0x00B2 -- '²' (part of latin1)
toSupscript '3' = chr 0x00B3 -- '³' (part of latin1)
toSupscript '4' = chr 0x2074 -- '⁴'
toSupscript '5' = chr 0x2075 -- '⁵'
toSupscript '6' = chr 0x2076 -- '⁶'
toSupscript '7' = chr 0x2077 -- '⁷'
toSupscript '8' = chr 0x2078 -- '⁸'
toSupscript '9' = chr 0x2079 -- '⁹'
toSupscript  _  = undefined

-- | From @GHC.Utils.Monad@:
-- Monad version of 'all', aborts the computation at the first @False@ value
allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM f bs = go bs
  where
    go []     = return True
    go (b:bs) = (f b) >>= (\bv -> if bv then go bs else return False)

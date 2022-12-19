{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies #-}

module FreeGradComAlg 
    where
        
import Prelude hiding ( (*>), (<*) )
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List.Ordered (nubSort)
import Control.Monad (guard)
import Data.Traversable (mapAccumL)

import Math.Algebras.VectorSpace
import Math.Algebras.Structures
import Math.Algebras.TensorAlgebra
import Math.Algebras.TensorProduct
import qualified Math.CommutativeAlgebra.Polynomial as Poly

import qualified Numeric.Matrix as Mat
import Numeric.Matrix (Matrix, MatrixElement, EchelonInfo(..))

import FracClear
import Util

--import Debug.Trace

------------
basisElems :: Vect k b -> [b]
basisElems = map fst . terms

coeffs :: Vect k b -> [k]
coeffs = map snd . terms

------------

type Generators a = M.Map a Int
type Lam k a = Vect k (FGCA a)

type FGCA a = Tensor (SymmetricAlgebra a) (ExteriorAlgebra a)

type Differential k a = (Lam k a -> Lam k a)

homDegree :: (Ord a) => Generators a -> FGCA a -> Int
homDegree gs (Sym _ xs, Ext _ ys) = s xs + s ys
    where
        s = sum . map (gs M.!)
        
degree :: (Ord a) => Generators a -> Lam k a -> Maybe Int
degree _  (V []) = Nothing
degree gs (V (((z,_):zs))) = if all sameDeg zs then Just k else Nothing
    where
        k = homDegree gs z
        sameDeg = ((== k) . homDegree gs . fst) 
        
degree_unsafe :: (Ord a) => Generators a -> Lam k a -> Int
degree_unsafe _  (V []) = 0
degree_unsafe gs (V ((z,_):_)) = homDegree gs z
        

decompose :: FGCA a -> Maybe (Either a a, FGCA a)
decompose ((Sym i (x:xs)), e) = Just (Left x,  (Sym (i-1) xs, e))
decompose (s, (Ext j (y:ys))) = Just (Right y, (s, Ext (j-1) ys))
decompose (_, _)              = Nothing

injectFGCA' :: (Eq k, Num k, Ord a) => Generators a -> a -> Maybe (Lam k a)
injectFGCA' = flip M.lookup . M.mapWithKey injectGen
    where
        injectGen z i = let lr = if even i then Left else Right
                         in injectFGCA_unsafe (lr z)
                         
injectFGCA'' :: (Eq k, Num k, Ord a) => Generators a -> a -> Lam k a
injectFGCA'' = maybe err id .: injectFGCA'
    where
        err = error "injectFGCA'': trying to inject a non-generator"

injectFGCA_unsafe :: (Eq k, Num k, Ord a) => Either a a -> Lam k a
injectFGCA_unsafe (Left x)  = injectSym' x `te` (unit 1)
injectFGCA_unsafe (Right y) = (unit 1) `te` injectExt' y
                    
extendDiff :: forall k a. (Eq k, Num k, Ord a)
           => (a -> Lam k a) -> Differential k a
extendDiff d0 = linear d1
    where
        d1 :: FGCA a -> Lam k a
        d1 g = case decompose g of
                 Nothing       -> zerov
                 Just (lr, g') -> d0 (unpack lr) * pure g' + sign lr *> (injectFGCA_unsafe lr * d1 g')
        unpack = either id id
        sign = either (const 1) (const (-1))
        
findBasis :: (Eq k, Num k, Ord a)
          => Generators a -> Int -> [Lam k a]
findBasis gs n = [product $ zipWith (^) gs' p | p <- ps]
    where
        gs' = injectFGCA'' gs <$> M.keys gs
        ps  = findPartitions (M.elems gs) n
        
indTrivialClass :: forall k a. (FracClear k, Ord k, MatrixElement (UnFrac k), Ord a)
                => Generators a -> Differential k a -> Lam k a -> Maybe Bool
-- returns Nothing if chain is not even a cycle
-- or if chain == 0 (for the moment)
indTrivialClass gs d chain = do
    guard (chain /= 0)
    guard (d chain == 0)
    
    let dim       = degree_unsafe gs chain
        imageGens = nubSort (d <$> findBasis gs (dim-1))
        bs        = foldMap (S.fromList . basisElems) imageGens
        
    if all (`S.member` bs) (basisElems chain)
    then do
        let vecs = imageGens ++ [chain]
            matl = map (fst . unfracs) [coeff b <$> vecs | b <- S.toList bs]
            mat  = Mat.fromList matl :: Matrix (UnFrac k)
            n    = Mat.numCols mat
            EchelonInfo { echIpivots = pivs } = Mat.echelonInfo mat
            
        return $ head pivs /= n
    else
        return False
        
fillChain :: forall k a. (Fractional k, FracClear k, Ord k, MatrixElement (UnFrac k), Ord a)
          => Generators a -> Differential k a -> Lam k a -> Maybe (Lam k a)
fillChain gs d chain = do
    guard (chain /= 0)
    guard (d chain == 0)
    
    let dim       = degree_unsafe gs chain
        imgGenAss = M.fromList [(d b, b) | b <- findBasis gs (dim-1)]
        imageGens = M.keys imgGenAss
        bs        = foldMap (S.fromList . basisElems) imageGens
        
    guard $ all (`S.member` bs) (basisElems chain)
    
    let vecs   = imageGens ++ [chain]
        matl   = map (fst . unfracs) [coeff b <$> vecs | b <- S.toList bs]
        mat    = Mat.fromList matl :: Matrix (UnFrac k)
        (m, n) = Mat.dimensions mat
        EchelonInfo { echIform   = ech
                    , echIrank   = rank
                    , echIpivots = pivs
                    } = Mat.echelonInfo mat
        a i j = fromUnFrac $ ech `Mat.at` (i,j)
            
    guard $ head pivs /= n
    
    let xs   = [ (a j n - sum cs) / (a j kj)
               | (j,kj) <- zip [rank,rank-1..1] pivs,
                 let cs = zipWith (*) [a j kt | kt <- take (rank - j) pivs] xs
               ]
        bs'  = (M.!) imgGenAss <$> multiIndex ([0] ++ imageGens) pivs
    
    return $ sum $ zipWith (<*) bs' xs
    
extendAlg :: forall k a. (Eq k, Num k, Ord a)
          => (a -> Lam k a) -> Lam k a -> Lam k a
extendAlg f0 = linear f1
    where
        f1 :: FGCA a -> Lam k a
        f1 g = case decompose g of
                 Nothing       -> 1
                 Just (lr, g') -> f0 (unpack lr) * f1 g'
        unpack = either id id

genericGAlgEndo :: forall k m v p a.
    (Eq k, Num k, Poly.MonomialConstructor m, Enum v, Ord (m v), Algebra k (m v), p ~ Vect k (m v), Ord a)
                => Generators a -> Lam p a -> Lam p a
genericGAlgEndo gs = extendAlg (f M.!)
    where
        degs    = foldr S.insert S.empty gs
        bases   = M.fromSet (findBasis gs) degs :: M.Map Int [Lam p a]
        (_, f)  = mapAccumL go (toEnum 1) gs
        go v' i = foldl (\ (v,a) g -> (succ v, a + var v *> g)) (v',0) $ bases M.! i
        var     = Poly.var
        
genericConstraints :: forall k m v p a.
    (Eq k, Num k, Poly.MonomialConstructor m, Enum v, Ord (m v), Algebra k (m v), p ~ Vect k (m v), Ord a)
      => Generators a -> Differential p a -> [p]
genericConstraints gs d = foldMap (coeffs . comm) gs'
    where
        f      = genericGAlgEndo gs
        comm x = d (f x) - f (d x)
        gs'    = (injectFGCA'' gs <$> M.keys gs)
        
mappingDegree :: forall k a.
    (Fractional k, FracClear k, Ord k, MatrixElement (UnFrac k), Ord a)
              => Generators a
              -> Differential k a
              -> Lam k a  -- ^ representative of fundamental class
              -> (Lam k a -> Lam k a)  -- ^ endomorphism
              -> k  -- ^ mapping degree of the endomorphism
mappingDegree gs d cycle f = maybe 0 id $ do
    guard (cycle /= 0)
    
    let dim       = degree_unsafe gs cycle
        imageGens = nubSort (d <$> findBasis gs (dim-1))
        vecs      = imageGens ++ [f cycle, cycle]
        bs        = foldMap (S.fromList . basisElems) vecs
        
        matl = map (fst . unfracs) [coeff b <$> vecs | b <- S.toList bs]
        mat  = Mat.fromList matl :: Matrix (UnFrac k)
        
        n = Mat.numCols mat
        EchelonInfo { echIform   = ech
                    , echIrank   = rank
                    , echIpivots = pivs
                    } = Mat.echelonInfo mat
        a i j = fromUnFrac $ ech `Mat.at` (i,j)
        
    guard $ head pivs == n-1
    guard $ a rank n /= 0
    
    return $ a rank (n-1) / a rank n
    
ellipticDimension :: (Ord a) => Generators a -> Int
ellipticDimension = M.foldl' accum 0
    where
        accum s k | even k    = s - (k-1)
                  | otherwise = s + k

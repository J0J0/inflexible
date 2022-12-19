{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Var where

newtype T = T { getT :: Int } deriving (Eq, Ord, Enum, Bounded)
instance Show T where show (T k) = "τ" ++ show k

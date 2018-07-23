{-# LANGUAGE DeriveFunctor #-}
module Markov where

newtype Probability a = Prob
    { getProb :: a
    } deriving (Show, Functor)

-- | A Markov is a matrix of probabilities.
newtype Markov a = Markov
    { getMarkov :: [[Probability a]]
    } deriving (Show)

instance Functor Markov where
    fmap f (Markov m) = Markov $ fmap (fmap (fmap f)) m

instance (Num a, Ord a) => Monoid (Markov a) where
    mempty = empty
    (Markov m) `mappend` (Markov n) = undefined

prob :: (Num a, Ord a) => a -> Probability a
prob x = if x < 0 || x > 1
    then error "Probability must be between 0 and 1, inclusive."
    else Prob x

empty :: (Num a, Ord a) => Markov a
empty = Markov [[prob 1]]

-- insert :: (Num a, Ord a) => Probability a -> Markov a -> Markov a
-- insert p (Markov m) =
--     let
--         withNewRow = m `mappend` (m !! 0)
--         withNewCol = fmap (++ [p]) withNewRow
--     in
--         undefined


{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib where

import           Control.Monad          (replicateM, void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Random   (MonadRandom, RandT, getRandom,
                                         runRandT)
import           Control.Monad.Reader   (MonadReader, ReaderT, asks, runReaderT)
import           Control.Monad.State    (MonadState, StateT, get, put,
                                         runStateT)
import           Data.List              (findIndex)
import           Data.Maybe             (fromMaybe)
import           System.Random          (StdGen, newStdGen)

-- | A Probability is a number n, where 0 <= n < 1.
type Probability = Double

-- | A Markov is a matrix of probabilities.
newtype Markov = Markov
    { getMarkov :: [[Probability]]
    } deriving (Show)

-- | An MState (Markov state) is an index in the matrix.
type MState = Int


-- | An App is a Markov chain with a current state and a Random Gen.
newtype App a = App
    { runApp :: ReaderT Markov (StateT MState (RandT StdGen IO)) a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadReader Markov
               , MonadState MState
               , MonadRandom
               , MonadIO
               )

run :: Markov -> MState -> StdGen -> App a -> IO ((a, MState), StdGen)
run markov mstate gen app = do
    runRandT (runStateT (runReaderT (runApp app) markov) mstate) gen

-- | Get the next state.
nextState :: App MState
nextState = do
    markov <- asks getMarkov
    currentState <- get
    rand <- getRandom :: App Double

    let
        -- Get the current "state" in the Markov chain
        row = markov !! currentState
        -- Create a list of probability ranges
        accumulatedSum = scanl1 (+) row
        -- The random number's index is the next state
        nextState' = fromMaybe
            (-1)
            (findIndex (> rand) accumulatedSum)

    -- Update and return the new state
    put nextState'
    return nextState'

exampleMarkov :: Markov
exampleMarkov = Markov
    [ [0.0, 1.0, 0.0]
    , [0.0, 0.0, 1.0]
    , [1.0, 0.0, 0.0]
    ]

printStates :: Int -> App ()
printStates n = void $ replicateM n (nextState >>= liftIO . print)

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib where

import Control.Monad (replicateM, void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Random   (MonadRandom, RandT, getRandom, runRandT)
import           Control.Monad.Reader   (MonadReader, ReaderT, ask, runReaderT)
import           Control.Monad.State    (MonadState, StateT, get, put, runStateT)
import           Data.List              (findIndex)
import           System.Random          (StdGen, newStdGen)

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

type Probability = Double

data Markov = Markov [[Probability]]
    deriving (Show)

type MState = Int

nextState :: App MState
nextState = do
    Markov markov <- ask
    index <- get
    rand <- getRandom :: App Double

    let row = markov !! index
        accumProbs = scanl1 (+) row
        nextIndex =
            case findIndex (> rand) accumProbs of
                Nothing -> length row - 1
                Just i -> i

    put nextIndex
    return nextIndex

exampleMarkov :: Markov
exampleMarkov = Markov
    [ [1.0, 0.0, 0.0]
    , [1.0, 0.0, 0.0]
    , [1.0, 0.0, 0.0]
    ]

run :: Markov -> MState -> App a -> IO ((a, MState), StdGen)
run markov mstate app = do
    gen <- newStdGen
    runRandT (runStateT (runReaderT (runApp app) markov) mstate) gen

printStates :: Int -> App ()
printStates n = void $ replicateM n (nextState >>= liftIO . print)

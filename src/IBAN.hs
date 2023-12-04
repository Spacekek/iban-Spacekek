--
-- INFOB3CC Concurrency
-- Practical 1: IBAN calculator
--
-- http://ics.uu.nl/docs/vakken/b3cc/assessment.html
--
module IBAN (

  Mode(..), Config(..),
  count, list, search

) where

import Control.Concurrent
import Crypto.Hash.SHA1
import Data.Atomics                                       ( readForCAS, casIORef, peekTicket )
import Data.IORef
import Data.List                                          ( elemIndex )
import Data.Word
import Data.Maybe                                         ( fromJust )
import System.Environment
import System.IO
import Data.ByteString.Char8                              ( ByteString )
import qualified Data.ByteString                          as B
import qualified Data.ByteString.Char8                    as B8
import GHC.MVar (MVar(MVar))


-- -----------------------------------------------------------------------------
-- 0. m-test
-- -----------------------------------------------------------------------------

-- Perform the m-test on 'number'
mtest :: Int -> Int -> Bool
mtest m number =
  foldr (\(x,y) acc -> acc + x*y) 0 (zip (digits number) [1..]) `mod` m == 0
  where
    digits 0 = []
    digits n = n `mod` 10 : digits (n `div` 10)


-- -----------------------------------------------------------------------------
-- 1. Counting mode (3pt)
-- -----------------------------------------------------------------------------

count :: Config -> IO Int
count config = do
  -- count the number of ints that pass the m-test, shared counter
  -- Implement count mode here!
  undefined


-- -----------------------------------------------------------------------------
-- 2. List mode (3pt)
-- -----------------------------------------------------------------------------

list :: Handle -> Config -> IO ()
list handle config = do
  -- create mvars
  mvs <- mapM (const newEmptyMVar) numbers
  -- create threads
  forkThreads (cfgThreads config) (`work` mvs)
  -- wait for all threads to finish
  mapM_ takeMVar mvs
  where
    -- list of numbers to check
    numbers = [cfgLower config .. cfgUpper config]
    -- work function for each thread
    work :: Int -> [MVar ()] -> IO ()
    work index mvs = do
      -- check if number passes m-test
      let number = numbers !! index
      if mtest (cfgModulus config) number
        then do
          hPrint handle number
          putMVar (mvs !! index) ()
        else do
          putMVar (mvs !! index) ()


-- -----------------------------------------------------------------------------
-- 3. Search mode (4pt)
-- -----------------------------------------------------------------------------

search :: Config -> ByteString -> IO (Maybe Int)
search config query = do
  -- Implement search mode here!
  undefined


-- -----------------------------------------------------------------------------
-- Starting framework
-- -----------------------------------------------------------------------------

data Mode = Count | List | Search ByteString
  deriving Show

data Config = Config
  { cfgLower   :: !Int
  , cfgUpper   :: !Int
  , cfgModulus :: !Int
  , cfgThreads :: !Int
  }
  deriving Show

-- Evaluates a term, before continuing with the next IO operation.
--
evaluate :: a -> IO ()
evaluate x = x `seq` return ()

-- Forks 'n' threads. Waits until those threads have finished. Each thread
-- runs the supplied function given its thread ID in the range [0..n).
--
forkThreads :: Int -> (Int -> IO ()) -> IO ()
forkThreads n work = do
  -- Fork the threads and create a list of the MVars which
  -- per thread tell whether the work has finished.
  finishVars <- mapM work' [0 .. n - 1]
  -- Wait on all MVars
  mapM_ takeMVar finishVars
  where
    work' :: Int -> IO (MVar ())
    work' index = do
      var <- newEmptyMVar
      _   <- forkOn index (work index >> putMVar var ())
      return var

-- Checks whether 'value' has the expected hash.
--
checkHash :: ByteString -> String -> Bool
checkHash expected value = expected == hash (B8.pack value)

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
import Foreign (new)


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
  count <- newIORef 0
  -- increment counter using casIORef, lock the counter every time we want to increment it and unlock it after

  forkThreads (cfgThreads config) (`work` count)
  -- wait for all threads to finish
  readIORef count
  where
    numbers = [cfgLower config .. cfgUpper config]
    -- work function for each thread
    work :: Int -> IORef Int -> IO ()
    work index count = do
      -- check if number passes m-test
      let number = numbers !! index
      if mtest (cfgModulus config) number
        then do
          -- increment counter
          undefined
        else do
          return ()


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
  -- given SHA1 hash and range in which to search, find the number that hashes to the given hash and passes the m-test

  -- create mvars
  mvs <- mapM (const newEmptyMVar) numbers
  -- create threads
  forkThreads (cfgThreads config) (`work` mvs)
  -- wait for all threads to finish
  mapM_ takeMVar mvs
  -- check if any thread found the number
  return $ elemIndex True (map (fst . takeMVar) mvs)
  where
    -- list of numbers to check
    numbers = [cfgLower config .. cfgUpper config]
    -- work function for each thread
    work :: Int -> [MVar (Bool, Int)] -> IO ()
    work index mvs = do
      -- check if number passes m-test
      let number = numbers !! index
      if mtest (cfgModulus config) number
        then do
          -- check if number hashes to the given hash
          if checkHash query (show number)
            then do
              putMVar (mvs !! index) (True, number)
            else do
              putMVar (mvs !! index) (False, number)
        else do
          putMVar (mvs !! index) (False, number)


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

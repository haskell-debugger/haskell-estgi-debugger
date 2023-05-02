----------------------------------------------------------------------------
-- |
-- Module      :  DAP.Internal
-- Copyright   :  (C) 2023 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
-- Description :  Internal functions for consumption by other modules like Server.hs
----------------------------------------------------------------------------
module DAP.Internal
  ( withLock
  , withGlobalLock
  ) where
----------------------------------------------------------------------------
import           Control.Concurrent         ( modifyMVar_, newMVar, MVar )
import           System.IO.Unsafe           ( unsafePerformIO )
----------------------------------------------------------------------------
-- | Used for logging in the presence of multiple threads.
lock :: MVar ()
{-# NOINLINE lock #-}
lock = unsafePerformIO $ newMVar ()
----------------------------------------------------------------------------
-- | Used for performing actions (e.g. printing debug logs to stdout)
-- Also used for writing to each connections Handle.
-- Ensures operations occur one thread at a time.
--
-- Used internally only
--
withLock :: MVar () -> IO () -> IO ()
withLock mvar action = modifyMVar_ mvar $ \x -> x <$ action
----------------------------------------------------------------------------
-- | Used for performing actions (e.g. printing debug logs to stdout)
-- Ensures operations occur one thread at a time.
--
-- Used internally only
--
withGlobalLock :: IO () -> IO ()
withGlobalLock = withLock lock
----------------------------------------------------------------------------

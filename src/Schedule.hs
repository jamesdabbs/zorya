module Schedule
  ( runScheduler
  ) where

import Control.Concurrent     (ThreadId, forkIO, threadDelay)
import Control.Monad          (forM_, forever)

import Types

-- TODO: digest and implement
--   https://www.schoolofhaskell.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions
catchAndNotify :: IO a -> IO a
catchAndNotify = id

runScheduler :: (Z () -> IO ()) -> [ (Frequency, Z ()) ] -> IO ()
runScheduler runIO items = forM_ items (startWorker runIO)

startWorker :: (Z () -> IO ()) -> (Frequency, Z ()) -> IO ThreadId
startWorker run (delay, action) = forkIO . forever $ do
  catchAndNotify $ run action
  sleep delay

sleep :: Frequency -> IO ()
sleep (Hours   n) = sleep $ Minutes (60 * n)
sleep (Minutes n) = sleep $ Seconds (60 * n)
sleep (Seconds n) = threadDelay $ n * 1000000

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module lets you periodically supply metric deltas to an IO action.
-- Example usage:
--
-- > main = do
-- >     store <- newStore
-- >     forkPush defaultPushOptions store
--
-- You probably want to include some of the predefined metrics defined
-- in the ekg-core package, by calling e.g. the 'registerGcStats'
-- function defined in that package.
module System.Remote.Monitoring.Push
    (
      pushThreadId
    , forkPush
    , PushOptions(..)
    , defaultPushOptions
    ) where

import Control.Concurrent (ThreadId, myThreadId, threadDelay, throwTo)
import qualified Data.HashMap.Strict as M
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified System.Metrics as Metrics

#if __GLASGOW_HASKELL__ >= 706
import Control.Concurrent (forkFinally)
#else
import Control.Concurrent (forkIO)
import Control.Exception (SomeException, mask, try)
import Prelude hiding (catch)
#endif

-- | A handle that can be used to control the push sync thread.
-- Created by 'forkPush'.
data Push = Push
    { threadId :: {-# UNPACK #-} !ThreadId
    }

-- | The thread ID of the push sync thread. You can stop the sync by
-- killing this thread (i.e. by throwing it an asynchronous
-- exception.)
pushThreadId :: Push -> ThreadId
pushThreadId = threadId

-- | Options to control how to connect to the push server and how
-- often to flush metrics. The flush interval should be shorter than
-- the flush interval push itself uses to flush data to its
-- backends.
data PushOptions = PushOptions
    {
      -- | Data push interval, in ms.
      flushInterval :: !Int

      -- | Print debug output to stderr.
    , debug :: !Bool

      -- | Prefix to add to all metric names.
    , prefix :: !T.Text

      -- | Suffix to add to all metric names. This is particularly
      -- useful for sending per host stats by settings this value to:
      -- @takeWhile (/= \'.\') \<$\> getHostName@, using @getHostName@
      -- from the @Network.BSD@ module in the network package.
    , suffix :: !T.Text
    }

-- | Defaults:
--
-- * @flushInterval@ = @1000@
--
-- * @debug@ = @False@
defaultPushOptions :: PushOptions
defaultPushOptions = PushOptions
    { flushInterval = 1000
    , debug         = False
    , prefix        = ""
    , suffix        = ""
    }

-- | Create a thread that periodically flushes the metrics in the
-- store to push.
forkPush :: PushOptions  -- ^ Options
           -> (Metrics.Sample -> IO ()) -- ^ Callback
           -> Metrics.Store  -- ^ Metric store
           -> IO Push      -- ^ Push sync handle
forkPush opts cb store = do
    me <- myThreadId
    tid <- forkFinally (loop cb store emptySample opts) $ \ r -> do
        case r of
            Left e  -> throwTo me e
            Right _ -> return ()
    return $ Push tid
    where
        emptySample = M.empty

loop :: (Metrics.Sample -> IO ()) -- ^ Callback
     -> Metrics.Store   -- ^ Metric Store
     -> Metrics.Sample  -- ^ Last sampled metrics
     -> PushOptions  -- ^ Options
     -> IO ()
loop cb store lastSample opts = do
    start <- time
    sample <- Metrics.sampleAll store
    let !diff = diffSamples opts lastSample sample
    cb diff -- call cb with our difference between the last poll and our current poll
    end <- time
    threadDelay (flushInterval opts * 1000 - fromIntegral (end - start))
    loop cb store sample opts

-- | Microseconds since epoch.
time :: IO Int64
time = (round . (* 1000000.0) . toDouble) `fmap` getPOSIXTime
  where toDouble = realToFrac :: Real a => a -> Double

diffSamples :: PushOptions -> Metrics.Sample -> Metrics.Sample -> Metrics.Sample
diffSamples opts prev curr = M.foldlWithKey' combine M.empty curr
  where
    combine m name new = case M.lookup name prev of
        Just old -> case diffMetric old new of
            Just val -> M.insert name' val m
            Nothing  -> m
        _        -> M.insert name' new m
        where
            name' = T.append (prefix opts) (T.append name (suffix opts))

    diffMetric :: Metrics.Value -> Metrics.Value -> Maybe Metrics.Value
    diffMetric (Metrics.Counter n1) (Metrics.Counter n2)
        | n1 == n2  = Nothing
        | otherwise = Just $! Metrics.Counter $ n2 - n1
    diffMetric (Metrics.Gauge n1) (Metrics.Gauge n2)
        | n1 == n2  = Nothing
        | otherwise = Just $ Metrics.Gauge n2
    diffMetric (Metrics.Label n1) (Metrics.Label n2)
        | n1 == n2  = Nothing
        | otherwise = Just $ Metrics.Label n2
    -- Distributions are assumed to be non-equal.
    diffMetric _ _  = Nothing

------------------------------------------------------------------------
-- Backwards compatibility shims

#if __GLASGOW_HASKELL__ < 706
forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then
#endif
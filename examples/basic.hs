{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Control.Concurrent (threadDelay, forkIO)
import Control.Exception (evaluate)
import Control.Monad (forever)
import Data.List (foldl')
import System.Metrics (newStore, registerGcMetrics, createCounter)
import qualified System.Metrics.Counter as Counter (inc)
import System.Remote.Monitoring.Push (PushOptions (..), forkPush, defaultPushOptions, subscribe, consume)

-- 'sum' is using a non-strict lazy fold and will blow the stack.
sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

mean :: Fractional a => [a] -> a
mean xs = sum' xs / fromIntegral (length xs)

main :: IO ()
main = do
    store <- newStore
    registerGcMetrics store
    iters <- createCounter "iterations" store

    -- Register our main ekg push channel
    push <- forkPush defaultPushOptions { prefix = "pfx_", suffix = "_sfx" } store

    -- Create two different subscriptions (ch1 & ch2)
    ch1 <- subscribe push
    _ <-forkIO $ forever $ do
            msg <- consume ch1
            putStrLn $ "subscription #1: " ++ show  msg

    ch2 <- subscribe push
    _ <- forkIO $ forever $ do
            msg <- consume ch2
            putStrLn $ "subscription #2: " ++ show msg

    let 
        loop :: Double -> IO ()
        loop n = do
            _ <- evaluate $ mean [1..n]
            Counter.inc iters
            threadDelay 2000
            loop n

    loop 1000000

# Modifications of ekg-statsd by Johan Tibell to provide a generic "push" framework.

You will notice this is almost identical to ekg-statsd. It's just me trying to abstract away the basic functionality found in ekg-statsd, so not to have to duplicate this code anywhere else. Eventually I may try to clean up this "abstraction" and create a PR in ekg-core.

# Getting started

See examples/basic.hs

```
main :: IO ()
main = do
    store <- newStore
    registerGcMetrics store
    iters <- createCounter "iterations" store
    push <- forkPush defaultPushOptions { prefix = "pfx", suffix = "sfx" } store

    ch1 <- subscribe push
    _ <- forkIO $ forever $ do
            msg <- consume ch1
            putStrLn $ "subscription #1: " ++ show  msg

    let loop n = do
            evaluate $ mean [1..n]
            Counter.inc iters
            threadDelay 2000
            loop n

    loop 1000000
```

-- adarqui

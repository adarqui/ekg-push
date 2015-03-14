# Modifications of ekg-statsd by Johan Tibbel to provide a generic "push" framework.

Eventually I should perhaps write to a Chan instead of calling the IO action?

You will notice this is almost identical to ekg-statsd. It's just me trying to abstract away the basic functionality found in ekg-statsd, so not to have to duplicate this code anywhere else. Eventually I may try to clean up this "abstraction" and create a PR in ekg-core.

# Getting started

See examples/basic.hs

```
main :: IO ()
main = do
    store <- newStore
    registerGcMetrics store
    iters <- createCounter "iterations" store
    forkPush defaultPushOptions { prefix = "pfx", suffix = "sfx" } (\arg -> (putStrLn $ show arg) >> return ()) store
    let loop n = do
            evaluate $ mean [1..n]
            Counter.inc iters
            threadDelay 2000
            loop n
    loop 1000000
```

-- adarqui

[![Hackage](https://img.shields.io/hackage/v/debug-trace-var.svg)](https://hackage.haskell.org/package/debug-trace-var)
[![Stackage LTS](http://stackage.org/package/debug-trace-var/badge/lts)](http://stackage.org/lts/package/debug-trace-var)

# debug-trace-var

When writing print debug, we often write.

~~~hs
import           Debug.Trace

main :: IO ()
main = do
  let a = 1
  traceIO $ "a = " ++ show a
~~~

This is troublesome to describe the name of the variable twice.

With debug-trace-var you write variables only once.

~~~hs
{-# LANGUAGE QuasiQuotes #-}
import           Debug.Trace.Var

main :: IO ()
main = do
  let a = 1
  [traceVarIO|a|]
~~~

# Example

~~~hs
{-# LANGUAGE QuasiQuotes #-}

import           Debug.Trace.Var

-- > a
-- n = 1
-- s = "あ"
a :: IO ()
a = let n = 1
        s = "あ"
    in [traceVar|n|] ([traceVar|s|] return ())

-- > b
-- "n = 1
-- n = 1"
b :: String
b = let n = 1
    in [traceVarId|n|]

-- > c
-- n = 344
c :: IO ()
c = let n = 344
    in [traceStackVar|n|] (return ())

-- > d
-- n = 344
-- n = 344
d :: IO ()
d = do
  let n = 344
  [traceVarIO|n|]
  [traceVarIO|n|]

-- > e
-- n = 344
e :: IO ()
e = do
  let n = 344
  [traceVarM|n|]
~~~

# Motivation

I wanted to use Template Haskell.

# One point advice

You can use [ndmitchell/debug: Haskell library for debugging](https://github.com/ndmitchell/debug)

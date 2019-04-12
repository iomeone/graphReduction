module Main where

-- hackage
import           Control.Concurrent (threadDelay)
import           Control.Monad      (forever)






-- self defined


import Eval

import State
import Types
import WatchFile





main :: IO ()
main = do
  startTask

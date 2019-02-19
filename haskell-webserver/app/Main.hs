module Main where

import Control.Concurrent.STM
import Data.IntMap
import Yesod

import Dispatch ()
import Foundation

main :: IO ()
main = do
    tstore <- atomically $ newTVar empty
    tident <- atomically $ newTVar 0
    warpEnv $ App tident tstore

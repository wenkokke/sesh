module Main where


import Control.Monad (forM_)
import Control.Search (search)
import Language.EGV


main :: IO ()
main = do
  tms <- search 10 (checkTypeAndUsage Unit)
  forM_ tms print

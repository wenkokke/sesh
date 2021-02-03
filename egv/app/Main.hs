module Main where


import Control.Monad (forM_)
import Control.Search (search)
import Data.Coolean
import Language.EGV


main :: IO ()
main = do
  tms <- search 20 (\tm -> checkType emptyTCS Unit tm &&& checkUsage tm)
  forM_ tms print

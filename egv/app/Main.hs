module Main where

import Language.EGV
import Control.Search (search)


main :: IO ()
main = print =<< search 30 (checkClosed Unit)

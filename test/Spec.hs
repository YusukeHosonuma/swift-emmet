module Main where

import Test.HUnit
import SwiftEmmet.ParserSpec

main :: IO ()
main = do
    _ <- runTestTT $ TestList
        [ parseTest
        ]
    return ()

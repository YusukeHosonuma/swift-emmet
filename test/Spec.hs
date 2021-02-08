module Main where

import Test.HUnit
import ParserSpec

main :: IO ()
main = do
    _ <- runTestTT $ TestList
        [ parseExprTest
        , parseExprWithSpaceTest
        ]
    return ()

module Main where

import Test.HUnit
import ParserSpec
import GenerateSpec

main :: IO ()
main = do
    _ <- runTestTT $ TestList
        [ parseExprTest
        , parseExprWithSpaceTest
        , generateTest
        ]
    return ()

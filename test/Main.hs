module Main where

import Test.Tasty
import SwiftEmmet as SE

main :: IO ()
main = defaultMain allTests

allTests :: TestTree 
allTests = testGroup "Tests"
    [ SE.tests
    ]

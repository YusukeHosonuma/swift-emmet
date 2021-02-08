{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.SwiftEmmet
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           System.Environment
import           System.Exit
import           System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    case args of
        []       -> printHeader >> loop
        (expr:_) -> printResult . eval . T.pack $ expr
    where
        loop = do
            T.putStrLn ""
            T.putStr "swift-emmet> "
            expr <- getLine
            T.putStrLn ""
            if expr == "exit"
                then
                    exitWith ExitSuccess
                else
                    case eval (T.pack expr) of
                        Right s -> T.putStrLn s
                        Left  s -> T.putStrLn s
            loop

printHeader :: IO ()
printHeader = do
    T.putStrLn "--------------------------------------------------------------------"
    T.putStrLn "Welcome swift-emmet 🎉"
    T.putStrLn ""
    T.putStrLn "Usage (e.g.)"
    T.putStrLn "- S.Person>l.name:String,v.age:Int,v.weight:Double - generate struct"
    T.putStrLn "- C.Person>l.name:S,v.age:I,v.weight:Double        - generate class"
    T.putStrLn ""
    T.putStrLn "Type Alias:"
    T.putStrLn "- S = String"
    T.putStrLn "- I = Int"
    T.putStrLn "- D = Double"
    T.putStrLn "--------------------------------------------------------------------"

printResult :: Either T.Text T.Text -> IO ()
printResult (Right s) = T.putStrLn s
printResult (Left s) = do
    T.putStrLn s
    exitWith $ ExitFailure 1

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.SwiftEmmet
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Options.Applicative
import           System.Exit
import System.IO

data Option = Option
    { optExpr :: String
    } deriving Show

optionParser :: Parser Option
optionParser = Option
    <$> strArgument
        (metavar "EXPR" <> value "" <> help "Evaluate expression and print")

usageParser :: Parser (a -> a)
usageParser = infoOption (usageMessage <> "\n" <> helpMessage)
    $  long "usage"
    <> help "Show detail usage"

version :: Parser (a -> a)
version = infoOption "0.1.0.0"
    $  long "version"
    <> help "Show version"

main :: IO ()
main = run =<< execParser opts
    where
        opts = info
            (optionParser <**> version <**> usageParser <**> helper)
            (fullDesc
                <> progDesc "Generate struct or class from short syntax"
                <> header "swift-emmet")

run :: Option -> IO ()
run (Option ""  ) = runRepl
run (Option expr) = runEval expr

runEval :: String -> IO ()
runEval = printResult . eval . T.pack
    where
        printResult :: Either T.Text T.Text -> IO ()
        printResult (Right s) = T.putStrLn s
        printResult (Left s) = do
            T.putStrLn s
            exitWith $ ExitFailure 1

runRepl :: IO ()
runRepl = do 
    hSetBuffering stdout NoBuffering
    printHeader >> loop
    where
        loop = do
            T.putStrLn ""
            T.putStr "swift-emmet> "
            expr <- getLine
            T.putStrLn ""
            if expr == "exit"
                then
                    exitSuccess
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
    putStrLn helpMessage
    T.putStrLn "--------------------------------------------------------------------"

usageMessage :: String
usageMessage = unlines
    [ "Usage(e.g.):"
    , "$ swift-emmet 'S.Person>l.name:String,v.age:Int,v.weight:Double'"
    , "struct Person {"
    , "    let name: String"
    , "    var age: Int"
    , "    var weight: Double"
    , "}"
    ]

helpMessage :: String
helpMessage = unlines
    [ "Expression example:"
    , "- S.Person>l.name:String,v.age:Int,v.weight:Double - generate struct"
    , "- C.Person>l.name:S,v.age:I,v.weight:Double        - generate class"
    , ""
    , "Type aliases:"
    , "- S = String"
    , "- I = Int"
    , "- D = Double"
    ]

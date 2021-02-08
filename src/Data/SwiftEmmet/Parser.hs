{-# LANGUAGE OverloadedStrings #-}

module Data.SwiftEmmet.Parser
    ( parseExpr
    , Field
    , Type
    , VariableType (Var, Let)
    , Property (Property)
    , DataType (Struct, Class)
    , Expr (Expr)
    ) where

import Data.Char (toLower)
import Data.Text ( pack, Text, toUpper )
import Data.Attoparsec.Text
import Control.Applicative

type Field = Text
type Type = Text
data VariableType = Var | Let deriving (Show, Eq)
data Property = Property VariableType Field Type deriving (Show, Eq)
data DataType = Struct Text
              | Class Text
              deriving (Show, Eq)
data Expr = Expr DataType [Property] deriving (Show, Eq)

parseExpr :: Text -> Either Text Expr
parseExpr s = showParseResult
    $ parse exprParser s `feed` ""

exprParser :: Parser Expr
exprParser = Expr
    <$> dataTypeParser 
    <*> ((schar '>' *> propertiesParser <* endOfInput) <|> (endOfInput >> pure []))

dataTypeParser :: Parser DataType
dataTypeParser =
    (Struct <$> (ichar 'S' *> schar '.' *> word)) <|> 
    (Class  <$> (ichar 'C' *> schar '.' *> word))

propertiesParser :: Parser [Property]
propertiesParser = propertyParser `sepBy` schar ','

propertyParser :: Parser Property
propertyParser = Property <$> variableType <*> (schar '.' *> field) <*> (schar ':' *> typeName)
    where
        variableType :: Parser VariableType
        variableType = (char 'v' >> return Var) <|> (char 'l' >> return Let)

        field :: Parser Text
        field = pack <$> many1 letter

word :: Parser Text
word = pack <$> many1 (letter <|> digit)

typeName :: Parser Text
typeName = resolveAlias <$> word

resolveAlias :: Type -> Type
resolveAlias t = case toUpper t of
    "S" -> "String"
    "I" -> "Int"
    "D" -> "Double"
    _   -> t

-- ignore case-sensitive
ichar :: Char -> Parser Char
ichar c = schar c <|> schar (toLower c)

-- ignore enclose white-spaces
schar :: Char -> Parser Char
schar c = skipSpace *> char c <* skipSpace 

showParseResult :: Show a => Result a -> Either Text a
showParseResult (Done _ r) = Right r
showParseResult r = Left . pack $ show r

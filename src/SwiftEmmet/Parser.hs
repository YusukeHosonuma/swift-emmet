module SwiftEmmet.Parser where

type Field = String
type Type = String
data Property = Var Field Type 
              | Let Field Type
              deriving (Show, Eq)
data Expr = Struct String [Property] deriving (Show, Eq)

parse :: String -> Expr
parse _ = Struct "Person" [Var "name" "String"] 

{-# LANGUAGE OverloadedStrings #-}

module Data.SwiftEmmet.Generate (generate) where

import           Data.SwiftEmmet.Parser
import qualified Data.Text              as T (Text, intercalate, lines, unlines)

generate :: Expr -> T.Text
generate (Expr (Struct name) inherits []) = "struct " <> name <> inheritsToText inherits <> " {\n" <> "}"
generate (Expr (Class  name) inherits []) = "class "  <> name <> inheritsToText inherits <> " {\n" <> initializer [] <> "\n}"
generate (Expr (Struct name) inherits ps) = "struct " <> name <> inheritsToText inherits <> " {\n" <> properties ps <> "\n}"
generate (Expr (Class  name) inherits ps) = "class "  <> name <> inheritsToText inherits <> " {\n" <> properties ps <> "\n\n" <> initializer ps <> "\n}"

properties :: [Property] -> T.Text
properties = join . map (indent . property)

property :: Property -> T.Text
property (Property Var f t) = "var " <> f <> ": " <> t
property (Property Let f t) = "let " <> f <> ": " <> t

initializer :: [Property] -> T.Text
initializer [] = indent' "init() {}"
initializer ps = indent' $
    "init(" <> x <> ",\n" <> T.intercalate ",\n" (map ("     " <>) xs) <> ") {\n" <>
    T.unlines (map field ps) <>
    "}"
    where
        field (Property _ f _) = "    self." <> f <> " = " <> f
        (x : xs) = map arg ps
        arg (Property _ f t) = f <> ": " <> t

indent :: T.Text -> T.Text
indent = ("    " <>)

indent' :: T.Text -> T.Text
indent' xs = join $ map indent $ T.lines xs

join :: [T.Text] -> T.Text
join = T.intercalate "\n"

inheritsToText :: [Inherit] -> T.Text 
inheritsToText [] = ""
inheritsToText xs = ": " <> (T.intercalate ", " $ map toText xs)
    where
        toText :: Inherit -> T.Text
        toText (Inherit x) = x

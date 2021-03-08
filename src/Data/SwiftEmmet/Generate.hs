{-# LANGUAGE OverloadedStrings #-}

module Data.SwiftEmmet.Generate (generate) where

import           Data.SwiftEmmet.Parser
import qualified Data.Text              as T (Text, intercalate, lines, unlines)

generate :: Expr -> T.Text
generate (Expr (Struct name) inharits []) = "struct " <> name <> inharitsToText inharits <> " {\n" <> "}"
generate (Expr (Class  name) inharits []) = "class "  <> name <> inharitsToText inharits <> " {\n" <> initializer [] <> "\n}"
generate (Expr (Struct name) inharits ps) = "struct " <> name <> inharitsToText inharits <> " {\n" <> properties ps <> "\n}"
generate (Expr (Class  name) inharits ps)   "class "  <> name <> inharitsToText inharits <> " {\n" <> properties ps <> "\n\n" <> initializer ps <> "\n}"

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

inharitsToText :: [Inharit] -> T.Text 
inharitsToText [] = ""
inharitsToText xs = ": " <> (T.intercalate ", " $ map toText xs)
    where
        toText :: Inharit -> T.Text
        toText (Inharit x) = x

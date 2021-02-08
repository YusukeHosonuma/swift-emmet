module Data.SwiftEmmet where

import Data.Text
import Data.SwiftEmmet.Parser
import Data.SwiftEmmet.Generate

eval :: Text -> Either Text Text
eval expr = generate <$> parseExpr expr

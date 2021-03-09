module Data.SwiftEmmet
    ( eval
    , versionString
    ) where

import Data.Text
import Data.SwiftEmmet.Parser
import Data.SwiftEmmet.Generate

eval :: Text -> Either Text Text
eval expr = generate <$> parseExpr expr

versionString :: String 
versionString = "0.2.0"

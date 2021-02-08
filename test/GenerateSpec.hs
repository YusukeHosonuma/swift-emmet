{-# LANGUAGE OverloadedStrings #-}

module GenerateSpec where

import Test.HUnit
import SwiftEmmet.Parser
import SwiftEmmet.Generate
import Data.Text

generateTest :: Test 
generateTest = TestList
    [ "generate test 1" ~:
        generate (Expr (Struct "Person") [])
        ~?= text [ "struct Person {"
                 , "}" ]
    , "generate test 2" ~:
        generate (Expr (Class "Person") [])
        ~?= text [ "class Person {"
                 , "    init() {}"
                 , "}" ]
    , "generate test 3" ~:
        generate (Expr (Struct "Person") [Property Let "name" "String"])
        ~?= text [ "struct Person {"
                 , "    let name: String"
                 , "}" ]
    , "generate test 4" ~:
        generate (Expr (Struct "Person") [ Property Let "name"   "String"
                                         , Property Var "age"    "Int"
                                         , Property Var "weight" "Double"
                                         ])
        ~?= text [ "struct Person {"
                 , "    let name: String"
                 , "    var age: Int"
                 , "    var weight: Double"
                 , "}" ]
    , "generate test 5" ~:
        generate (Expr (Class "Person") [ Property Let "name"   "String"
                                        , Property Var "age"    "Int"
                                        , Property Var "weight" "Double"
                                        ])
        ~?= text [ "class Person {"
                 , "    let name: String"
                 , "    var age: Int"
                 , "    var weight: Double"
                 , ""
                 , "    init(name: String,"
                 , "         age: Int,"
                 , "         weight: Double) {"
                 , "        self.name = name"
                 , "        self.age = age"
                 , "        self.weight = weight"
                 , "    }"
                 , "}" ]
    ]

text :: [Text] -> Text
text = intercalate "\n"

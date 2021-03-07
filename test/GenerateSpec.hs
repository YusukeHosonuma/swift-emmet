{-# LANGUAGE OverloadedStrings #-}

module GenerateSpec where

import Test.HUnit
import Data.SwiftEmmet.Parser
import Data.SwiftEmmet.Generate
import Data.Text

generateTest :: Test 
generateTest = TestList
    [ "generate test 1" ~:
        generate (Expr (Struct "Person") [] [])
        ~?= text [ "struct Person {"
                 , "}" ]
    , "generate test 2" ~:
        generate (Expr (Class "Person") [] [])
        ~?= text [ "class Person {"
                 , "    init() {}"
                 , "}" ]
    , "generate test 3" ~:
        generate (Expr (Struct "Person") [] [Property Let "name" "String"])
        ~?= text [ "struct Person {"
                 , "    let name: String"
                 , "}" ]
    , "generate test 4" ~:
        generate (Expr (Struct "Person") [] [ Property Let "name"   "String"
                                         , Property Var "age"    "Int"
                                         , Property Var "weight" "Double"
                                         ])
        ~?= text [ "struct Person {"
                 , "    let name: String"
                 , "    var age: Int"
                 , "    var weight: Double"
                 , "}" ]
    , "generate test 5" ~:
        generate (Expr (Class "Person") [] [ Property Let "name"   "String"
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

generateWithInharitsTest :: Test
generateWithInharitsTest = TestList
    [ "generate inharits test 1" ~:
        generate (Expr (Struct "Person") [Inharit "Foo"] [])
        ~?= text [ "struct Person: Foo {"
                 , "}"]
    , "generate inharits test 2" ~:
        generate (Expr (Struct "Person") [Inharit "Foo", Inharit "Bar"] [])
        ~?= text [ "struct Person: Foo, Bar {"
                 , "}"]
    , "generate inharits test 3" ~:
        generate (Expr (Class "Person") [Inharit "Foo"] [])
        ~?= text [ "class Person: Foo {"
                 , "    init() {}"
                 , "}"]
    , "generate inharits test 4" ~:
        generate (Expr (Class "Person") [Inharit "Foo", Inharit "Bar"] [])
        ~?= text [ "class Person: Foo, Bar {"
                 , "    init() {}"
                 , "}"]
    ]

text :: [Text] -> Text
text = intercalate "\n"

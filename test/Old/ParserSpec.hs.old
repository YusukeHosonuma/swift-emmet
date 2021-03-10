{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Test.HUnit ( (~:), (~?=), Test(TestList) )
import Data.SwiftEmmet.Parser

parseExprTest :: Test
parseExprTest = TestList
    [ "parse test 0" ~:
        parseExpr "S.Person"
        ~?= Right (Expr (Struct "Person") [] [])
    , "parse test 1" ~:
        parseExpr "C.Person=l.name:String"
        ~?= Right (Expr (Class "Person") [] [Property Let "name" "String"])
    , "parse test 2" ~:
        parseExpr "S.Person=l.name:String,v.age:Int,v.weight:Double"
        ~?= Right (Expr (Struct "Person") [] [ Property Let "name" "String"
                                          , Property Var "age" "Int"
                                          , Property Var "weight" "Double"])
    , "parse test 3" ~:
        parseExpr "S.Person=l.name:S,v.age:I,v.weight:D"
        ~?= Right (Expr (Struct "Person") [] [ Property Let "name" "String"
                                          , Property Var "age" "Int"
                                          , Property Var "weight" "Double"])
    , "parse test 4" ~:
        parseExpr "s.Person=l.name:s,v.age:i,v.weight:d"
        ~?= Right (Expr (Struct "Person") [] [ Property Let "name" "String"
                                          , Property Var "age" "Int"
                                          , Property Var "weight" "Double"])
    , "parse test 5" ~:
        parseExpr "c.Person=l.name:s,v.age:i,v.weight:d"
        ~?= Right (Expr (Class "Person") [] [ Property Let "name" "String"
                                         , Property Var "age" "Int"
                                         , Property Var "weight" "Double"])
    , "parse test 6" ~:
        parseExpr "S.Person=name:String,age:Int,weight:Double"
        ~?= Right (Expr (Struct "Person") [] [ Property Var "name" "String"
                                          , Property Var "age" "Int"
                                          , Property Var "weight" "Double"])
    ]

parseExprWithSpaceTest :: Test
parseExprWithSpaceTest = TestList
    [ "parse white-space test 0" ~:
        parseExpr "S . Person"
        ~?= Right (Expr (Struct "Person") [] [])
    , "parse white-space test 1" ~:
        parseExpr "C . Person = l . name : String"
        ~?= Right (Expr (Class "Person") [] [Property Let "name" "String"])
    , "parse white-space test 2" ~:
        parseExpr "S . Person = l . name : String , v . age : Int , v . weight : Double"
        ~?= Right (Expr (Struct "Person") [] [ Property Let "name" "String"
                                          , Property Var "age" "Int"
                                          , Property Var "weight" "Double"])
    , "parse white-space test 3" ~:
        parseExpr "S . Person = l . name : S , v . age : I , v . weight : D"
        ~?= Right (Expr (Struct "Person") [] [ Property Let "name" "String"
                                          , Property Var "age" "Int"
                                          , Property Var "weight" "Double"])
    , "parse white-space test 4" ~:
        parseExpr "s . Person = l . name : s , v . age : i , v . weight : d"
        ~?= Right (Expr (Struct "Person") [] [ Property Let "name" "String"
                                          , Property Var "age" "Int"
                                          , Property Var "weight" "Double"])
    , "parse white-space test 5" ~:
        parseExpr "c . Person = l . name : s , v . age : i , v . weight : d"
        ~?= Right (Expr (Class "Person") [] [ Property Let "name" "String"
                                         , Property Var "age" "Int"
                                         , Property Var "weight" "Double"])
    , "parse white-space test 6" ~:
        parseExpr "S . Person = name : String , age : Int , weight : Double"
        ~?= Right (Expr (Struct "Person") [] [ Property Var "name" "String"
                                          , Property Var "age" "Int"
                                          , Property Var "weight" "Double"])
    ]

parseExprWithInheritsTest :: Test
parseExprWithInheritsTest = TestList
    [ "parse inherits test 1" ~:
        parseExpr "S.Person: Foo"
            ~?= Right (Expr (Struct "Person") [Inherit "Foo"] [])
    , "parse inherits test 2" ~:
        parseExpr "S.Parson: Foo = l.name: s"
            ~?= Right (Expr (Struct "Parson") [Inherit "Foo"] [Property Let "name" "String"])
    , "parse inherits test 3" ~:
        parseExpr "S.Person: Foo, Bar"
            ~?= Right (Expr (Struct "Person") [Inherit "Foo", Inherit "Bar"] [])
    , "parse inherits test 4" ~:
        parseExpr "C.Person: Foo"
            ~?= Right (Expr (Class "Person") [Inherit "Foo"] [])
    , "parse inherits test 5" ~:
        parseExpr "C.Parson: Foo = l.name: s"
            ~?= Right (Expr (Class "Parson") [Inherit "Foo"] [Property Let "name" "String"])
    , "parse inherits test 6" ~:
        parseExpr "C.Person: Foo, Bar"
            ~?= Right (Expr (Class "Person") [Inherit "Foo", Inherit "Bar"] [])
    ]

parseExprAliasTest :: Test
parseExprAliasTest = TestList
    [ "parseExpr alias test 1" ~:
      parseExpr "S.Foo: E,C = v.s:S, v.b:B, v.i:I, v.l:L, v.f:F, v.d:D, v.u:U, v.other:Other"
      ~?= Right (Expr (Struct "Foo")
        [ Inherit "Equatable"
        , Inherit "Codable"
        ]
        [ Property Var "s" "String"
        , Property Var "b" "Bool"
        , Property Var "i" "Int"
        , Property Var "l" "Long"
        , Property Var "f" "Float"
        , Property Var "d" "Double"
        , Property Var "u" "URL"
        , Property Var "other" "Other"
        ])
    ]

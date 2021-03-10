{-# LANGUAGE OverloadedStrings #-}

module SwiftEmmet (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.SwiftEmmet
import qualified Data.Text as T
import Data.List

tests :: TestTree 
tests = testGroup "SwiftEmmet"
    [ testStruct
    , testClass
    , testInherit
    , testAlias
    , testToken
    ]

testStruct :: TestTree
testStruct = testGroup "Struct"
    [ testEval "No fields"
        "s.Person"
        [ "struct Person {"
        , "}"
        ]
    , testEval "Has fields"
        "s.Person = name:String, age:Int"
        [ "struct Person {"
        , "    var name: String"
        , "    var age: Int"
        , "}"
        ]
    , testEval "Specify 'v' and 'l'"
        "s.Person = l.name:String, v.age:Int"
        [ "struct Person {"
        , "    let name: String"
        , "    var age: Int"
        , "}"
        ]
    ]

testClass :: TestTree
testClass = testGroup "Class"
    [ testEval "No fields"
        "c.Person"
        [ "class Person {"
        , "    init() {}"
        , "}"
        ]
    , testEval "Has fields"
        "c.Person = name:String, age:Int"
        [ "class Person {"
        , "    var name: String"
        , "    var age: Int"
        , ""
        , "    init(name: String,"
        , "         age: Int) {"
        , "        self.name = name"
        , "        self.age = age"
        , "    }"
        , "}"
        ]
    , testEval "Specify 'v' and 'l'"
        "c.Person = l.name:String, v.age:Int"
        [ "class Person {"
        , "    let name: String"
        , "    var age: Int"
        , ""
        , "    init(name: String,"
        , "         age: Int) {"
        , "        self.name = name"
        , "        self.age = age"
        , "    }"
        , "}"
        ]
    ]

testInherit :: TestTree
testInherit = testGroup "Inherits"
    [ testEval "Inherit one"
        "s.Person: Equatable = name:String"
        [ "struct Person: Equatable {"
        , "    var name: String"
        , "}"
        ]
    , testEval "Inherit two"
        "s.Person: Equatable, Codable = name:String"
        [ "struct Person: Equatable, Codable {"
        , "    var name: String"
        , "}"
        ]
    ]

testAlias :: TestTree
testAlias = testGroup "Alias"
    [ testEval "Use alias"
        "S.Foo: E, C, Bar = v.s:S, v.b:B, v.i:I, v.l:L, v.f:F, v.d:D, v.u:U, v.other:Other"
        [ "struct Foo: Equatable, Codable, Bar {"
        , "    var s: String"
        , "    var b: Bool"
        , "    var i: Int"
        , "    var l: Long"
        , "    var f: Float"
        , "    var d: Double"
        , "    var u: URL"
        , "    var other: Other"
        , "}"
        ]
    ]

testToken :: TestTree 
testToken = testGroup "Token"
    [ testEval "Upper-case"
        "S.Person:E,C=l.name:S,v.age:I" expected
    , testEval "Lower-case"
        "s.Person:e,c=l.name:s,v.age:i" expected
    , testEval "Use white-space"
        "S . Person : E , C = l . name : S , v . age : I" expected
    ]
    where
        expected = 
            [ "struct Person: Equatable, Codable {"
            , "    let name: String"
            , "    var age: Int"
            , "}"
            ]

testEval :: String -> String -> [T.Text] -> TestTree
testEval title input expected =
    testCase title $ assertEqual "" (Right expectedText) (eval (T.pack input))
        where
            expectedText = mconcat $ intersperse "\n" expected

module SwiftEmmet.ParserSpec where

import Test.HUnit ( (~:), (~?=), Test(TestList) )
import SwiftEmmet.Parser

parseTest :: Test
parseTest = TestList
    [ "parse test 1" ~:
        parse "S.Person>v.name:String"
        ~?= Struct "Person" [Var "name" "String"]
    ]

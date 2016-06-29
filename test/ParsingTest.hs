module ParsingTest where

import Andromeda

import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec
  

    
constStr1 = "const boosters = Controller (\"boosters\")"
constStr2 = "const str = \"boosters\""
beginStr = "begin boostersProgram"
endStr   = "end boostersProgram"
varStr   = "var result1 = Run (boosters, start)"

script = unlines [ "const boosters = Controller (\"boosters\")"
                 , "const start = Command (\"start\", Nothing)"
                 , "const stop = Command (\"stop\", Nothing)"
                 , ""
                 , "begin boostersProgram"
                 , "    var result1 = Run (boosters, start)"
                 , "    var result2 = Run (boosters, stop)"
                 , "end boostersProgram"
                 ]


test = do
    print ""
    
    parseTest constDeclaration constStr1
    parseTest constDeclaration constStr2
    parseTest beginDeclaration beginStr
    parseTest endDeclaration endStr
    
    print ""
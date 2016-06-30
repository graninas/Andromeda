{-# LANGUAGE FlexibleContexts #-}
module ParsingTest where

import Andromeda

import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec
  

    
constStr1 = "const boosters = ControllerAbc (\"boosters adsf 32 3 3$ %$@ TELaes jkfj lkf jkjfs\")"
constStr2 = "const boosters=Controller(\"boosters\")"
constStr3 = "const boosters=Controller "
constStr4 = "const str = \"boosters\""
constStr5 = "const str = \"boosters\"   "
valStr1   = "val result1 = Run (boosters, start)"
valStr2   = "val result2 = Run (boosters, Command (\"stop\", Nothing))  "
indentedValStr1   = "    val result2 = Run (boosters, CommandAbc (\"stop\", Nothing))"
indentedValStr2   = "    val result2 = Run (boosters, Command (\"stop\", Nothing))  "
indentedValStr3   = "        val result2 = Run (boosters, Command (\"stop\", Nothing))  "
indentedConstStr1 = "    const boosters = Controller (\"boosters\")"
indentedConstStr2 = "    const boosters = Controller (\"boosters\")  "
indentedConstStr3 = "        const boosters = ControllerAbc (\"boosters\")  "
indentedCallStr1  = "    LogError (\"stop command failed.\")"
indentedCallStr2  = "    LogError ( errorString )  "
indentedCallStr3  = "        LogError( \"stop command failed.\")  "
paramsList1 = "(val p1, val p2)"
paramsList2 = "(  val p1, val p2, val aLLL33    )"
paramsList3 = "(val p1, val p2p2p2p2p2p2)"
procStmtStr1 = "BoostersProgram:\n    \n    "
procStmtStr2 = "BoostersProgram   : \n    \n    "
procStmtStr3 = "BoostersProgram   (val abc, val cde): "
procLinedIdentedStr1 = "\n    val result2 = Run (boosters, CommandAbc (\"stop\", Nothing))"
procLinedIdentedStr2 = "\n    val result2 = Run (boosters, Command (\"stop\", Nothing))  "
procLinedIdentedStr3 = "\n"
procBody = "\n\n    val result2 = Run (boosters, CommandAbc (\"stop\", Nothing))"
procStr1 = "BoostersProgram:" ++ "\n    val result2 = Run (boosters, CommandAbc (\"stop\", Nothing))"
procStr2 = "BoostersProgram:" ++ "\n    val result2 = Run (boosters, CommandAbc (\"stop\", Nothing))\n    val result2 = Run (boosters, Command (\"stop\", Nothing))  "
procStr3 = "BoostersProgram   (val abc, val cde): " ++ "\n    val result2 = Run (boosters, CommandAbc (\"stop\", Nothing))"
procStr4 = "BoostersProgram   (val abc, val cde):" ++ "\n\n    val result2 = Run (boosters, CommandAbc (\"stop\", Nothing))"

parseTest' n a b = do
    putStr $ show n ++ ": "
    parseTest a b
    
test = do
    print ""
    
    parseTest'  1 constantStatement constStr1
    parseTest'  2 constantStatement constStr2
    parseTest'  3 constantStatement constStr3
    parseTest'  4 constantStatement constStr4
    parseTest' 95 constantStatement constStr5
    parseTest'  5 valStatement valStr1
    parseTest'  6 valStatement valStr2
    parseTest'  7 indentedStatement indentedValStr1
    parseTest'  8 indentedStatement indentedValStr2
    parseTest'  9 indentedStatement indentedValStr3
    parseTest' 10 indentedStatement indentedConstStr1
    parseTest' 11 indentedStatement indentedConstStr2
    parseTest' 12 indentedStatement indentedConstStr3
    parseTest' 13 indentedStatement indentedCallStr1
    parseTest' 14 indentedStatement indentedCallStr2
    parseTest' 15 indentedStatement indentedCallStr3
    parseTest' 16 paramDef paramsList1
    parseTest' 17 paramDef paramsList2
    parseTest' 18 paramDef paramsList3    
    parseTest' 19 procedureStmt procStmtStr1
    parseTest' 20 procedureStmt procStmtStr2
    parseTest' 21 procedureStmt procStmtStr3    
    parseTest' 22 linedStatement procLinedIdentedStr1
    parseTest' 23 linedStatement procLinedIdentedStr2
    parseTest' 24 linedStatement procLinedIdentedStr3
    parseTest' 25 procedureBody procBody    
    parseTest' 26 procedure procStr1
    parseTest' 27 procedure procStr2
    parseTest' 28 procedure procStr3
    parseTest' 29 procedure procStr4
    
    
    print ""
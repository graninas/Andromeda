{-# LANGUAGE FlexibleContexts #-}
module ParsingTest where

import Andromeda

import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec
  

    
constStr1 = "const boosters = Controller (\"boosters\")"
constStr2 = "const boosters=Controller(\"boosters\")"
constStr3 = "const str = \"boosters\""
constStr4 = "const str = \"boosters\"   "
valStr1   = "val result1 = Run (boosters, start)"
valStr2   = "val result2 = Run (boosters, Command (\"stop\", Nothing))  "
indentedValStr1   = "    val result2 = Run (boosters, Command (\"stop\", Nothing))"
indentedValStr2   = "    val result2 = Run (boosters, Command (\"stop\", Nothing))  "
indentedValStr3   = "        val result2 = Run (boosters, Command (\"stop\", Nothing))  "
indentedConstStr1 = "    const boosters = Controller (\"boosters\")"
indentedConstStr2 = "    const boosters = Controller (\"boosters\")  "
indentedConstStr3 = "        const boosters = Controller (\"boosters\")  "
indentedCallStr1  = "    LogError (\"stop command failed.\")"
indentedCallStr2  = "    LogError ( errorString )  "
indentedCallStr3  = "        LogError( \"stop command failed.\")  "

constrDeclStr1 = "BoostersProgram():"
constrDeclStr2 = "BoostersProgram  ( ) : "

{-

const boosters = Controller ("boosters")
const start = Command ("start", Nothing)
const stop = Command ("stop", Nothing)
const successCmd = Right ("OK")
const logInfoSuccess = LogInfo ("success")

BoostersProgram():
    val result1 = Run (boosters, start)
    if (result1 == successCmd) then
        val result2 = Run (boosters, Command ("stop", Nothing))
        if (result2 == successCmd) then
            LogError ("stop command failed.")
        else
            LogInfo ("stop command succeeded.")
            logInfoSuccess
            444
    else
        LogInfo ("start command succeeded.")
        -}

parseTest' n a b = do
    putStr $ show n ++ ": "
    parseTest a b
    
test = do
    print ""
    
    parseTest'  1 constantStatement constStr1
    parseTest'  2 constantStatement constStr2
    parseTest'  3 constantStatement constStr3
    parseTest'  4 constantStatement constStr4
    parseTest'  5 valStatement      valStr1
    parseTest'  6 valStatement      valStr2
    parseTest'  7 indentedStatement indentedValStr1
    parseTest'  8 indentedStatement indentedValStr2
    parseTest'  9 indentedStatement indentedValStr3
    parseTest' 10 indentedStatement indentedConstStr1
    parseTest' 11 indentedStatement indentedConstStr2
    parseTest' 12 indentedStatement indentedConstStr3
    parseTest' 13 indentedStatement indentedCallStr1
    parseTest' 14 indentedStatement indentedCallStr2
    parseTest' 15 indentedStatement indentedCallStr3
    
    
    --parseTest constrDeclaration constrDeclStr1
    --parseTest endDeclaration   endStr
    
    print ""
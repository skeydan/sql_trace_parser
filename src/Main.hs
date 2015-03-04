{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Control.Applicative


main :: IO ()
main = do
  print $ parseOnly parseCallType "PARSE"
  print $ parseOnly parseCloseType "1"
  print $ parseOnly parseOptimizerGoal "3"


parseCallType :: Parser CallType
parseCallType = 
     (string "PARSE" >> return Parse)
 <|> (string "EXEC" >> return Exec)
 <|> (string "FETCH" >> return  Fetch)

parseOptimizerGoal :: Parser OptimizerGoal
parseOptimizerGoal = do
     goal <- satisfy (inClass "1234")
     case goal of
         '1' -> return  AllRows
         '2' -> return FirstRows
         '3' -> return Rule
         '4' -> return Choose

parseCloseType :: Parser CloseType
parseCloseType = do
     close <- satisfy (inClass "0123")
     case close of
         '0' -> return  HardClose
         '1' -> return SoftCloseEmptySlot
         '2' -> return SoftCloseReplaceOther
         '3' -> return SoftClosePutBack
         
data Call = Call {
    callType      :: CallType,
    callCurNum    :: Int,
    callCpu       :: Int,
    callElapsed   :: Int,
    physReads     :: Int,
    consReads     :: Int,
    curReads      :: Int,
    misses        :: Int,
    numRows       :: Int,
    callCallDepth :: Int,
    optGoal       :: OptimizerGoal,
    phvalue       :: Int,
    callTim       :: Int
} deriving (Show)

data CallType = Parse | Exec | Fetch
  deriving (Show)

data OptimizerGoal = AllRows | FirstRows | Rule | Choose
  deriving (Show)

data Cursor = Cursor {
    crsrCurNum     :: Int,
    length         :: Int,
    crsrCallDepth  :: Int,
    parsingUserId  :: Int,
    commandType    :: Int,
    parsingSchemaId :: Int,
    crsrTim        :: Int,
    hash_value     :: Int,
    address        :: String,
    sqlId          :: String,
    sql_text       :: String
    }

data Wait = Wait {
    waitCurNum     :: Int,
    event          :: String,
    waitElapsed    :: Int,
    p1name         :: String,
    p1value        :: Int,
    p2name         :: String,
    p2value        :: Int,
    p3name         :: String,
    p3value        :: Int,
    obj            :: Int,
    waitTim        :: Int
    }

data CursorClose = CursorClose {
    closeCurNum    :: Int,
    closeCpu       :: Int,
    closeElapsed   :: Int,
    closeCallDepth :: Int,
    closeType      :: CloseType,
    closeTim       :: Int
    }

data CloseType = HardClose | SoftCloseEmptySlot | SoftCloseReplaceOther | SoftClosePutBack deriving (Show)
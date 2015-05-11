{-# LANGUAGE OverloadedStrings #-}


module Main where

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString                  as B
import qualified Data.Map                         as M

tracefile = "example.trc"

main :: IO ()
main = do
    file <- B.readFile tracefile
    let parsed = parseOnly parseLines file
    case parsed of
        Left str -> putStrLn "couldn't parse file"
        Right a -> bySqlId a

matchCursors :: Line -> StateHolder -> M.Map SqlId [Line]
matchCursors l s = case l of
    Cursor {sqlId = id} -> M.insert id [l] (mapAccum s)
    _ -> M.insertWith (++) 
                      (if (lastCurNum s) == (curNum l) then lastSqlId s
                       else error $ show (lastSqlId s) ++ "  " ++ show (lastCurNum s) ++ "  " ++ show (mapAccum s) )
                       [l]
                       (mapAccum s)


bySqlId :: [Line] -> IO ()
bySqlId lns = do
  let startState = StateHolder "" 0 M.empty
  let sqlIdsMap = foldr (\l s -> StateHolder {lastSqlId = case l of
                                                Cursor {sqlId = id} -> id
                                                _ -> lastSqlId s,
                                              lastCurNum = curNum l,
                                              mapAccum  = matchCursors l s})
                         startState
                         lns
  print sqlIdsMap

curNum :: Line -> CurNum
curNum Call {callCurNum = n} = n
curNum Cursor {crsrCurNum = n} = n
curNum Wait {waitCurNum = n} = n
curNum Stat {statCurNum = n} = n
curNum Close {closeCurNum = n} = n

-- --M.insertWith (++) (sqlId l) l (mapAccum s)
type SqlId = String
type CurNum = Int
data StateHolder = StateHolder {
  lastSqlId  :: SqlId,
  lastCurNum :: CurNum,
  mapAccum   :: M.Map SqlId [Line]
  } deriving (Show)

parseLines :: Parser [Line]
parseLines = many $ parseLine <* endOfLine

parseLine :: Parser Line
parseLine =  parseCursor <|> parseCall  <|> parseWait <|> parseClose <|> parseStat

parseCursor :: Parser Line
parseCursor = Cursor <$> (string "PARSING IN CURSOR #" *> (read <$> many1 digit) <* string " len=")
                     <*> (read <$> many1 digit <*  string " dep=")
                     <*> (read <$> many1 digit <*  string " uid=")
                     <*> (read <$> many1 digit <*  string " oct=")
                     <*> (read <$> many1 digit <*  string " lid=")
                     <*> (read <$> many1 digit <*  string " tim=")
                     <*> (read <$> many1 digit <*  string " hv=")
                     <*> (read <$> many1 digit <*  string " ad='")
                     <*> (manyTill (digit <|> letter_ascii) (char '\'') <*  string " sqlid='")
                     <*> manyTill (digit <|> letter_ascii) (string "\'\n")
                     <*> manyTill anyChar (string "\nEND OF STMT")


parseCall :: Parser Line
parseCall = Call <$> (parseCallType <*  string " #")
                 <*> (read <$> many1 digit <*  string ":c=")
                 <*> (read <$> many1 digit <*  string ",e=")
                 <*> (read <$> many1 digit <*  string ",p=")
                 <*> (read <$> many1 digit <*  string ",cr=")
                 <*> (read <$> many1 digit <*  string ",cu=")
                 <*> (read <$> many1 digit <*  string ",mis=")
                 <*> (read <$> many1 digit <*  string ",r=")
                 <*> (read <$> many1 digit <*  string ",dep=")
                 <*> (read <$> many1 digit <*  string ",og=")
                 <*> (parseOptGoal <*  string ",plh=")
                 <*> (read <$> many1 digit <*  string ",tim=")
                 <*> (read <$> many1 digit)

parseWait :: Parser Line
parseWait = Wait <$> (string "WAIT #" *> (read <$> many1 digit) <* string ": nam='")
                 <*> (manyTill anyChar (char '\'') <*  string " ela= ")
                 <*> (read <$> many1 digit <*  space)
                 <*> (manyTill anyChar (char '='))
                 <*> (read <$> many1 digit <*  space)
                 <*> (manyTill anyChar (char '='))
                 <*> (read <$> many1 digit <*  space)
                 <*> (manyTill anyChar (char '='))
                 <*> (read <$> many1 digit <*  string " obj#=")
                 <*> (read <$> many1 digit <*  string " tim=")
                 <*> (read <$> many1 digit)

parseClose :: Parser Line
parseClose =  Close <$> (string "CLOSE #" *> (read <$> many1 digit) <* string ":c=")
                    <*> (read <$> many1 digit <*  string ",e=")
                    <*> (read <$> many1 digit <*  string ",dep=")
                    <*> (read <$> many1 digit <*  string ",type=")
                    <*> (parseCloseType <*  string ",tim=")
                    <*> (read <$> many1 digit)


parseCallType :: Parser CallType
parseCallType =
     (string "PARSE" >> return Parse)
 <|> (string "EXEC" >> return Exec)
 <|> (string "FETCH" >> return  Fetch)

parseOptGoal :: Parser OptimizerGoal
parseOptGoal = do
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


parseStat :: Parser Line
parseStat = Stat <$> (string "STAT #" *> (read <$> many1 digit) <* string " id=")
                 <*> (read <$> many1 digit <*  string " cnt=")
                 <*> (read <$> many1 digit <*  string " pid=")
                 <*> (read <$> many1 digit <*  string " pos=")
                 <*> (read <$> many1 digit <*  string " obj=")
                 <*> (read <$> many1 digit <*  string " op='")
                 <*> manyTill anyChar (char '\'')

data CallType = Parse | Exec | Fetch
  deriving (Show)

data OptimizerGoal = AllRows | FirstRows | Rule | Choose
  deriving (Show)

data Line =
    Call {
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
        phValue       :: Int,
        callTim       :: Int
    } |
    Cursor {
        crsrCurNum      :: Int,
        length          :: Int,
        crsrCallDepth   :: Int,
        parsingUserId   :: Int,
        commandType     :: Int,
        parsingSchemaId :: Int,
        crsrTim         :: Int,
        hash_value      :: Int,
        address         :: String,
        sqlId           :: String,
        sqlText        :: String
    } |
    Wait {
        waitCurNum  :: Int,
        event       :: String,
        waitElapsed :: Int,
        p1name      :: String,
        p1value     :: Int,
        p2name      :: String,
        p2value     :: Int,
        p3name      :: String,
        p3value     :: Int,
        obj         :: Int,
        waitTim     :: Int
    } |
    Close {
        closeCurNum    :: Int,
        closeCpu       :: Int,
        closeElapsed   :: Int,
        closeCallDepth :: Int,
        closeType      :: CloseType,
        closeTim       :: Int
    } |
    Stat {
        statCurNum :: Int,
        id         :: Int,
        cnt        :: Int,
        pid        :: Int,
        pos        :: Int,
        statObj    :: Int,
        op         :: String
        } deriving (Show)


data CloseType = HardClose | SoftCloseEmptySlot | SoftCloseReplaceOther | SoftClosePutBack deriving (Show)

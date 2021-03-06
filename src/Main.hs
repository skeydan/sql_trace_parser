{-# LANGUAGE OverloadedStrings #-}


module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString                  as B
import qualified Data.Map                         as M

tracefile = "CDB11_ora_27190.trc"

main :: IO ()
main = do
    file <- B.readFile tracefile
    let parsed = parseOnly parseLines file
    case parsed of
        Left str -> putStrLn "couldn't parse file"
        Right a -> do
            let allBySqlId = sqlIdsMap (groupBySqlId a)
                stats = M.map statementStats allBySqlId
              in print $ formatStats stats allBySqlId

formatStats :: M.Map SqlId StatsMap -> M.Map SqlId [Line] -> String
formatStats = undefined

statementStats :: [Line] -> StatsMap
statementStats lns =
  let start = StatsMap (CallStats 0 0 0 0 0 0 0 0) (CallStats 0 0 0 0 0 0 0 0) (CallStats 0 0 0 0 0 0 0 0) (WaitStats 0 0 0) in
      foldr (\l sm -> updateStats l sm) start lns

updateStats :: Line -> StatsMap -> StatsMap
updateStats l sm = case l of
    Call ctype _ cpu ela physrd consrd currd mis nrows _ _ _ _  -> case ctype of
        Parse -> updateForCallType sm Parse cpu ela physrd consrd currd mis nrows
        Exec -> updateForCallType sm Exec cpu ela physrd consrd currd mis nrows
        Fetch -> updateForCallType sm Fetch cpu ela physrd consrd currd mis nrows
        _ -> sm
    Wait _ event ela _ _ _ _ _ _ _ _  -> updateWaitTime sm event ela
    _ -> sm

updateWaitTime :: StatsMap -> String -> Int -> StatsMap
updateWaitTime sm event ela =
  StatsMap { parseStats = parseStats sm,
             execStats = execStats sm,
             fetchStats = fetchStats sm,
             waitStats = WaitStats {timesWaited = timesWaited (waitStats sm) + 1,
                                    maxWait = maxWait (waitStats sm) + 1,
                                    totalWaited = totalWaited (waitStats sm) + 1 }}

updateForCallType :: StatsMap -> CallType -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> StatsMap
updateForCallType sm ctype newcpu newela physrd consrd currd newmis newrows =
  StatsMap {parseStats = case ctype of
      Parse -> CallStats {calls = calls (parseStats sm) + 1,
                          cpu = cpu (parseStats sm) + newcpu,
                          elapsed = elapsed (parseStats sm) + newela,
                          disk = disk (parseStats sm) + physrd,
                          current = current (parseStats sm) + currd,
                          query = query (parseStats sm) + consrd,
                          rows = rows (parseStats sm) + newrows,
                          mis = mis (parseStats sm) + newmis }
      _ -> parseStats sm,
      execStats = case ctype of
      Exec -> CallStats {calls = calls (execStats sm) + 1,
                          cpu = cpu (execStats sm) + newcpu,
                          elapsed = elapsed (execStats sm) + newela,
                          disk = disk (execStats sm) + physrd,
                          current = current (execStats sm) + currd,
                          query = query (execStats sm) + consrd,
                          rows = rows (execStats sm) + newrows,
                          mis = mis (execStats sm) + newmis }
      _ -> execStats sm,
      fetchStats = case ctype of
      Parse -> CallStats {calls = calls (fetchStats sm) + 1,
                          cpu = cpu (fetchStats sm) + newcpu,
                          elapsed = elapsed (fetchStats sm) + newela,
                          disk = disk (fetchStats sm) + physrd,
                          current = current (fetchStats sm) + currd,
                          query = query (fetchStats sm) + consrd,
                          rows = rows (fetchStats sm) + newrows,
                          mis = mis (fetchStats sm) + newmis }
      _ -> fetchStats sm,
      waitStats = waitStats sm}

data StatsMap = StatsMap {
  parseStats :: CallStats,
  execStats  :: CallStats,
  fetchStats :: CallStats,
  waitStats  :: WaitStats
} deriving (Show)

data CallStats = CallStats {
  calls   :: Int,
  cpu     :: Int,
  elapsed :: Int,
  disk    :: Int,
  current :: Int,
  query   :: Int,
  rows    :: Int,
  mis     :: Int
} deriving (Show)

data WaitStats = WaitStats {
  timesWaited :: Int,
  maxWait     :: Int,
  totalWaited :: Int
} deriving (Show)

groupBySqlId :: [Line] -> CMaps
groupBySqlId lns = do
  let start = CMaps M.empty M.empty in
    foldl (\m l -> case l of
                    Cursor {sqlId = id, crsrCurNum = n} -> CMaps { sqlIdsMap = M.insertWith (++) id [l] (sqlIdsMap m),
                                                                   cursorsMap = M.insertWith (++) n [l] (cursorsMap m) }
                    _ -> let mySqlId = sqlId (head (filter isCursor (M.findWithDefault undefined (curNum l) (cursorsMap m)))) in
                           CMaps { sqlIdsMap = M.insertWith (++)  mySqlId [l] (sqlIdsMap m),
                                   cursorsMap = M.insertWith (++) (curNum l) [l] (cursorsMap m) })
          start
          lns

isCursor :: Line -> Bool
isCursor (Cursor _ _ _ _ _ _ _ _ _ _ _) = True
isCursor _ = False

curNum :: Line -> CurNum
curNum Call {callCurNum = n} = n
curNum Cursor {crsrCurNum = n} = n
curNum Wait {waitCurNum = n} = n
curNum Stat {statCurNum = n} = n
curNum Close {closeCurNum = n} = n

type SqlId = String
type CurNum = Int

data CMaps = CMaps {
  sqlIdsMap  :: M.Map SqlId [Line],
  cursorsMap :: M.Map CurNum [Line]
  }

instance Show CMaps where
    show (CMaps smap cmap) =
      let slist = M.toList smap
          clist = M.toList cmap
      in "Cursors by sql_id:\n\n" ++ concatMap showSqlId slist ++ "\n\nCursors by cursor number:\n\n" ++ concatMap showCursor clist
      where showSqlId = \(s,lns) -> s ++ ":\n" ++ concatMap showLine (reverse lns) ++ "\n"
            showCursor = \(c,lns) -> show c ++ ":\n" ++ concatMap showLine (reverse lns) ++ "\n"
            showLine = \l -> show l ++ "\n"

parseLines :: Parser [Line]
parseLines = do
    all <- many1 $ (parseLine <* endOfLine) <|> skipLine
    return $ (filter (\l -> case l of
                              EmptyLine -> False
                              _         -> True))
             all

skipLine :: Parser Line
skipLine = do
    skipWhile (/= '\n') >> endOfLine
    return EmptyLine

parseLine :: Parser Line
parseLine =  parseCursor <|> parseCall  <|> parseWait <|> parseClose <|> parseStat <|> skipLine

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
        sqlText         :: String
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
        }
      |
    EmptyLine
      deriving (Show)


data CloseType = HardClose | SoftCloseEmptySlot | SoftCloseReplaceOther | SoftClosePutBack deriving (Show)

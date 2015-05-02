{-# LANGUAGE OverloadedStrings #-}


import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8



main :: IO ()
main = testMain

realMain :: IO ()
realMain = print "hello"

testMain :: IO ()
testMain = do
  print $ parseOnly parseCall "PARSE #140611565469136:c=1000,e=198,p=0,cr=0,cu=0,mis=0,r=0,dep=0,og=1,plh=1719253974,tim=1420815241318034"
  print $ parseOnly parseCall "EXEC #140611565469136:c=0,e=48,p=0,cr=0,cu=0,mis=0,r=0,dep=0,og=1,plh=1719253974,tim=1420815241318199"
  print $ parseOnly parseCall "FETCH #140611565469136:c=0,e=130,p=0,cr=4,cu=0,mis=0,r=1,dep=0,og=1,plh=1719253974,tim=1420815241318449"
  print $ parseOnly parseWait "WAIT #140611565469136: nam='SQL*Net message from client' ela= 1132 driver id=675562835 #bytes=1 p3=0 obj#=69200 tim=1420815241319726"
  print $ parseOnly parseClose "CLOSE #140611565469136:c=0,e=10,dep=0,type=0,tim=1420815241317768"
  print $ parseOnly parseStat "STAT #140611565469136 id=1 cnt=1 pid=0 pos=1 obj=68951 op='TABLE ACCESS BY INDEX ROWID SESSION (cr=4 pr=0 pw=0 time=129 us cost=4 size=39 card=1)'"
  print $ parseOnly parseLine "PARSE #140611565469136:c=1000,e=198,p=0,cr=0,cu=0,mis=0,r=0,dep=0,og=1,plh=1719253974,tim=1420815241318034"


parseLine :: Parser Line
parseLine =  parseCall  <|> parseWait <|> parseClose <|> parseStat

parseCursor :: Parser Line
parseCursor = Cursor <$> (string "PARSING IN CURSOR #" *> (read <$> many1 digit) <* string ": len='")
                     <*> (read <$> many1 digit <*  string " dep=")
                     <*> (read <$> many1 digit <*  string " uid=")
                     <*> (read <$> many1 digit <*  string " oct=")
                     <*> (read <$> many1 digit <*  string " lid=")
                     <*> (read <$> many1 digit <*  string " tim=")
                     <*> (read <$> many1 digit <*  string " hv=")
                     <*> (read <$> many1 digit <*  string " ad='")
                     <*> (manyTill anyChar (char '\'') <*  string " sqlid='' ")
                     <*> manyTill anyChar (char '\'') 
                     <*> manyTill anyChar (string "END OF STMT")


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
        sql_text        :: String
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

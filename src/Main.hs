{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.Text
import           Control.Applicative

{-
PARSING IN CURSOR #140611565469136 len=70 dep=0 uid=76 oct=3 lid=76 tim=1420815241318036 hv=1034183590 ad='659573c30' sqlid='cfpbyk4yu8sx6'
                                select * from SESSION
                                 where KEY like :1

END OF STMT
PARSE #140611565469136:c=1000,e=198,p=0,cr=0,cu=0,mis=0,r=0,dep=0,og=1,plh=1719253974,tim=1420815241318034
EXEC #140611565469136:c=0,e=48,p=0,cr=0,cu=0,mis=0,r=0,dep=0,og=1,plh=1719253974,tim=1420815241318199
WAIT #140611565469136: nam='SQL*Net message to client' ela= 2 driver id=675562835 #bytes=1 p3=0 obj#=69200 tim=1420815241318273
FETCH #140611565469136:c=0,e=130,p=0,cr=4,cu=0,mis=0,r=1,dep=0,og=1,plh=1719253974,tim=1420815241318449
STAT #140611565469136 id=1 cnt=1 pid=0 pos=1 obj=68951 op='TABLE ACCESS BY INDEX ROWID SESSION (cr=4 pr=0 pw=0 time=129 us cost=4 size=39 card=1)'
STAT #140611565469136 id=2 cnt=1 pid=1 pos=1 obj=69455 op='INDEX RANGE SCAN SESSION_PK (cr=3 pr=0 pw=0 time=110 us cost=3 size=0 card=1)'
WAIT #140611565469136: nam='SQL*Net message from client' ela= 1132 driver id=675562835 #bytes=1 p3=0 obj#=69200 tim=1420815241319726
-}

main :: IO ()
main = do
  print $ parseOnly parseCall "PARSE #140611565469136:c=1000,e=198,p=0,cr=0,cu=0,mis=0,r=0,dep=0,og=1,plh=1719253974,tim=1420815241318034"
  print $ parseOnly parseCall "EXEC #140611565469136:c=0,e=48,p=0,cr=0,cu=0,mis=0,r=0,dep=0,og=1,plh=1719253974,tim=1420815241318199"
  print $ parseOnly parseCall "FETCH #140611565469136:c=0,e=130,p=0,cr=4,cu=0,mis=0,r=1,dep=0,og=1,plh=1719253974,tim=1420815241318449"
  print $ parseOnly parseWait "WAIT #140611565469136: nam='SQL*Net message from client' ela= 1132 driver id=675562835 #bytes=1 p3=0 obj#=69200 tim=1420815241319726"

parseCall :: Parser Call
parseCall = do
    callType <- parseCallType
    string " #"
    curNum <- read <$> many1 digit
    string ":c="
    cpu <- read <$> many1 digit
    string ",e="
    elapsed <- read <$> many1 digit
    string ",p="
    physReads <- read <$> many1 digit
    string ",cr="
    consReads <- read <$> many1 digit
    string ",cu="
    curReads <- read <$> many1 digit
    string ",mis="
    misses <- read <$> many1 digit
    string ",r="
    rows <- read <$> many1 digit
    string ",dep="
    callDepth <- read <$> many1 digit
    string ",og="
    optGoal <- parseOptGoal
    string ",plh="
    phValue <- read <$> many1 digit
    string ",tim="
    tim <- read <$> many1 digit
    return $ Call callType curNum cpu elapsed physReads consReads curReads misses rows callDepth optGoal phValue tim


parseWait :: Parser Wait
parseWait = do
    string "WAIT #"
    curNum <- read <$> many1 digit
    string ": nam='"
    event <- manyTill anyChar (char '\'')
    string " ela= "
    elapsed <- read <$> many1 digit
    space
    p1name <- manyTill anyChar (char '=')
    p1value <- read <$> many1 digit
    space
    p2name <- manyTill anyChar (char '=')
    p2value <- read <$> many1 digit
    space
    p3name <- manyTill anyChar (char '=')
    p3value <- read <$> many1 digit
    string " obj#="
    obj <- read <$> many1 digit
    string " tim="
    tim <- read <$>  many1 digit
    return $ Wait curNum event elapsed p1name p1value p2name p2value p3name p3value obj tim




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
    phValue       :: Int,
    callTim       :: Int
} deriving (Show)

data CallType = Parse | Exec | Fetch
  deriving (Show)

data OptimizerGoal = AllRows | FirstRows | Rule | Choose
  deriving (Show)

data Cursor = Cursor {
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
    } deriving (Show)

data Wait = Wait {
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
    } deriving (Show)

data CursorClose = CursorClose {
    closeCurNum    :: Int,
    closeCpu       :: Int,
    closeElapsed   :: Int,
    closeCallDepth :: Int,
    closeType      :: CloseType,
    closeTim       :: Int
    } deriving (Show)


data CloseType = HardClose | SoftCloseEmptySlot | SoftCloseReplaceOther | SoftClosePutBack deriving (Show)

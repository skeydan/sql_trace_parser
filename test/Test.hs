{-# LANGUAGE OverloadedStrings #-}

module Test where

import           Data.Attoparsec.ByteString.Char8
import qualified Data.Map                         as M
import Main

main :: IO ()
main = do
  print test
  {-
  print $ parseOnly parseCursor "PARSING IN CURSOR #140611565469136 len=70 dep=0 uid=76 oct=3 lid=76 tim=1420815241318036 hv=1034183590 ad='659573c30' sqlid='cfpbyk4yu8sx6'\nselect * from SESSION\nwhere KEY like :1\nEND OF STMT"
  print $ parseOnly parseCall "PARSE #140611565469136:c=1000
  print $ parseOnly parseCall "PARSE #140611565469136:c=1000,e=198,p=0,cr=0,cu=0,mis=0,r=0,dep=0,og=1,plh=1719253974,tim=1420815241318034"
  print $ parseOnly parseCall "EXEC #140611565469136:c=0,e=48,p=0,cr=0,cu=0,mis=0,r=0,dep=0,og=1,plh=1719253974,tim=1420815241318199"
  print $ parseOnly parseCall "FETCH #140611565469136:c=0,e=130,p=0,cr=4,cu=0,mis=0,r=1,dep=0,og=1,plh=1719253974,tim=1420815241318449"
  print $ parseOnly parseWait "WAIT #140611565469136: nam='SQL*Net message from client' ela= 1132 driver id=675562835 #bytes=1 p3=0 obj#=69200 tim=1420815241319726"
  print $ parseOnly parseClose "CLOSE #140611565469136:c=0,e=10,dep=0,type=0,tim=1420815241317768"
  print $ parseOnly parseStat "STAT #140611565469136 id=1 cnt=1 pid=0 pos=1 obj=68951 op='TABLE ACCESS BY INDEX ROWID SESSION (cr=4 pr=0 pw=0 time=129 us cost=4 size=39 card=1)'"
  print $ parseOnly parseLine "PARSE #140611565469136:c=1000,e=198,p=0,cr=0,cu=0,mis=0,r=0,dep=0,og=1,plh=1719253974,tim=1420815241318034"
  -}
test = map (\(k, v) -> show k ++ v ++ "\n") [(3,"b"), (5,"a")] 
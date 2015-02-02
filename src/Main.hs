module Main where

data Call = Call {
  lineno :: Int,
  call_type :: CallType,
  crsr :: Int,
  cpu :: Int,
  elapsed :: Int,
  phys_reads :: Int,
  cons_reads :: Int,
  cur_reads :: Int,
  misses :: Int,
  num_rows :: Int,
  call_depth :: Int,
  opt_goal :: OptimizerGoal,
  ph_value :: Int,
  tim :: Int
} deriving (Show)

data CallType = Parse | Exec | Fetch 
  deriving (Show)

data OptimizerGoal = Rule | AllRows
  deriving (Show)
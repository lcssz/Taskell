module Types where

import Data.Time.Calendar (Day)

data Status = Pending | Completed deriving (Show, Read, Eq)

data Priority = Low | Medium | High deriving (Show, Read, Eq, Ord)

data Category = Work | Study | Personal | Other deriving (Show, Read, Eq)

data Task = Task
  { taskId :: Int,
    description :: String,
    status :: Status,
    priority :: Priority,
    category :: Category,
    deadline :: Maybe Day,
    tags :: [String]
  }
  deriving (Show, Read, Eq)

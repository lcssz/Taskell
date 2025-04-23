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

createTask :: Int -> String -> Status -> Priority -> Category -> Maybe Day -> [String] -> Task
createTask _taskId _description _status _priority _category _deadline _tags =
  Task
    { taskId = _taskId,
      description = _description,
      status = _status,
      priority = _priority,
      category = _category,
      deadline = _deadline,
      tags = _tags
    }

addTask :: Task -> [Task] -> [Task]
addTask task = (:) task

markCompleted :: Int -> [Task] -> [Task]
markCompleted targetId = map updateStatus
  where
    updateStatus task
      | taskId task == targetId = task {status = Completed}
      | otherwise = task

removeTask :: Int -> [Task] -> [Task]
removeTask removeId = filter (\task -> removeId /= taskId task)

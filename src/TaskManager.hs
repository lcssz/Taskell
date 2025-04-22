module TaskManager (createTask, addTask, markCompleted, taskExists, removeTask) where

import Data.Time.Calendar (Day)
import Types

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
addTask task xt = task : xt

taskExists :: [Task] -> Int -> Bool
taskExists tasks _id = any ((== _id) . taskId) tasks

markCompleted :: Int -> [Task] -> [Task]
markCompleted targetId tasks = map updateStatus tasks
  where
    updateStatus task
      | taskId task == targetId = task {status = Completed}
      | otherwise = task

removeTask :: Int -> [Task] -> [Task]
removeTask _id = filter ((/= _id) . taskId)

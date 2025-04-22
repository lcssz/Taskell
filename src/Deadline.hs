module Deadline (checkOverdueTasks, remainingDays) where

import Data.Time (Day, diffDays)
import Types

checkOverdueTasks :: [Task] -> Day -> [Task]
checkOverdueTasks xt day = filter verifyDay xt
  where
    verifyDay task = maybe False (day >) (deadline task)

remainingDays :: Task -> Day -> Maybe Int
remainingDays task day = fmap diffDaysInt (deadline task)
  where
    diffDaysInt = fromInteger . diffDays day
